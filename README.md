# Assignment 2: Write Open Addres Set


## Киселёв Михаил Васильевич

- Хэш-множество (неизменяемая полиморфная структура данных) [src/Structure.hs](src/Structure.hs)
- Тестирование (unit + property-based) [test/Main.hs](test/Main.hs)

# Реализация

## [Структура HS](src/Structure.hs)

Само множество: 
```haskell
data Slot k = Empty | Deleted | Occupied k
  deriving (Show, Eq)


data OASet k = OASet
  { vecRef    :: IORef (IOVector (Slot k)) 
  , sizeRef   :: IORef Int                 
  , tombRef   :: IORef Int                 
  }


```
является "списком списков", то есть бакетов состоящих из элементов.
Позиция (бакет, в который нужно добавить) каждого элемента вычисляется при помощи hash-функции, что обеспечивает быстрый доступ к элементам.




Основные операции:
- `addElement` / `deleteElement` — добавление и удаление с автоматической нормализацией.
- `filterHS` / `mapHS` — функциональные преобразования без протечки внутреннего представления.
- `foldlHS` / `foldrHS` — свертки по бакетам.
- `memberHS` — проверка принадлежности за счёт адресации по хешу.


**`insert`**
```haskell
insert :: (Eq k, Hashable k) => OASet k -> k -> IO Bool
insert st key = do
  v <- readIORef (vecRef st)
  let cap = MV.length v
      start = hash key .&. (cap - 1)
  res <- probeInsert v cap start 0 Nothing key
  case res of
    Left True -> return False
    Left False -> do
      rehash st (cap * 2)
      insert st key
    Right pos -> do
      prev <- MV.read v pos
      when (prev == Deleted) $ modifyIORef' (tombRef st) (subtract 1)
      MV.write v pos (Occupied key)
      modifyIORef' (sizeRef st) (+1)

      sz <- readIORef (sizeRef st)
      tb <- readIORef (tombRef st)
      let capD = MV.length v
          load = fromIntegral (sz + tb) / fromIntegral capD
      when (load > loadFactorThreshold) $
        rehash st (capD * 2)
      return True
```


находим необходимый индекс (бакет), в случае превышения loadfactor
перехэшируем всё множество. Находим необходимый бакет, если такого элемента еще нет то возвращаем обновленный бакет. 
В силу иммутабельности структуры возвращаем новое множество. 



**`delete`**

```haskell
delete :: (Eq k, Hashable k) => OASet k -> k -> IO Bool
delete st key = do
  v <- readIORef (vecRef st)
  let cap = MV.length v
      start = hash key .&. (cap - 1)
  probe v cap start 0
  where
    probe v cap i cnt
      | cnt >= cap = return False
      | otherwise = do
          s <- MV.read v i
          case s of
            Empty -> return False
            (Occupied k') | k' == key -> do
              MV.write v i Deleted
              modifyIORef' (sizeRef st) (subtract 1)
              modifyIORef' (tombRef st) (+1)
              return True
            _ -> probe v cap ((i + 1) .&. (cap - 1)) (cnt + 1)

```

аналогично addElement, но возвращается обновленная структура без заданного элемента.


**`filterHS`**
```haskell
filterHS :: (Hashable a) => (a -> Bool) -> HS a -> HS a
filterHS p hs =
  let (buckets', newSize) = filterBuckets p (buckets hs)
      updated = hs {buckets = buckets', size = newSize}
   in normalizeHS updated
   


filterBuckets :: (a -> Bool) -> Buckets a -> (Buckets a, Int)
filterBuckets _ BucketsNil = (BucketsNil, 0)
filterBuckets p (BucketsCons bucket rest) =
  let (bucket', keptInBucket) = filterBucket p bucket
      (rest', keptInRest) = filterBuckets p rest
   in (BucketsCons bucket' rest', keptInBucket + keptInRest)

filterBucket :: (a -> Bool) -> Bucket a -> (Bucket a, Int)
filterBucket _ BNil = (BNil, 0)
filterBucket p (BCons y ys)
  | p y =
      let (ys', count) = filterBucket p ys
       in (BCons y ys', count + 1)
  | otherwise = filterBucket p ys
```

оставляем те эелементы, которые соответствуют предикату.

**`mapHS`**
```haskell
mapHS :: (Hashable b) => (a -> b) -> HS a -> HS b
mapHS f hs = normalizeHS (mapBuckets f (buckets hs) (emptyHS (bucketCount hs)))

mapBuckets :: (Hashable b) => (a -> b) -> Buckets a -> HS b -> HS b
mapBuckets _ BucketsNil acc = acc
mapBuckets f (BucketsCons bucket rest) acc =
  let acc' = mapBucket f bucket acc
   in mapBuckets f rest acc'

mapBucket :: (Hashable b) => (a -> b) -> Bucket a -> HS b -> HS b
mapBucket _ BNil acc = acc
mapBucket f (BCons bucketHead bucketTail) acc =
  let acc' = insertInternal True (f bucketHead) acc
   in mapBucket f bucketTail acc'
```

применяем функцию к элементам исходного множества и всталяем в новое множество (это необходимо для того, чтобы высчитать хэш на основе новых значений после применения функции)


**`foldlHS/foldrHS`**
```haskell
foldlHS :: (b -> a -> b) -> b -> HS a -> b
foldlHS f z hs = foldlBuckets f z (buckets hs)

foldrHS :: (a -> b -> b) -> b -> HS a -> b
foldrHS f z hs = foldrBuckets f z (buckets hs)

foldlBuckets :: (b -> a -> b) -> b -> Buckets a -> b
foldlBuckets _ acc BucketsNil = acc
foldlBuckets f acc (BucketsCons bucket rest) =
  let acc' = foldlBucket f acc bucket
   in foldlBuckets f acc' rest

foldlBucket :: (b -> a -> b) -> b -> Bucket a -> b
foldlBucket _ acc BNil = acc
foldlBucket f acc (BCons y ys) =
  let acc' = f acc y
   in foldlBucket f acc' ys

foldrBuckets :: (a -> b -> b) -> b -> Buckets a -> b
foldrBuckets _ acc BucketsNil = acc
foldrBuckets f acc (BucketsCons bucket rest) =
  let accRest = foldrBuckets f acc rest
   in foldrBucket f accRest bucket

foldrBucket :: (a -> b -> b) -> b -> Bucket a -> b
foldrBucket _ acc BNil = acc
foldrBucket f acc (BCons y ys) =
  f y (foldrBucket f acc ys)
```




## [Тестирование](test/Main.hs)
### Unit-тесты (HUnit)
```haskell
unitTests :: Test
unitTests = TestList
  [ TestLabel "insert" testInsertPreservesMembership
  , TestLabel "delete" testDeleteRemovesMembership
  , TestLabel "filter" testFilterKeepsOnlyMatching
  , TestLabel "fold" testFoldAggregatesValues
  ]
```
Покрывают корректность вставки, удаления, фильтрации и свертки.

### Property-based (QuickCheck)
```haskell
prop_monoidAssociativity :: IntSet -> IntSet -> IntSet -> Bool
prop_monoidAssociativity (IntSet a) (IntSet b) (IntSet c) =
  (a <> b) <> c == a <> (b <> c)
```
Свойства включают:
- Левую и правую нейтральность `mempty`.
- Ассоциативность `(<>)`.
- Совпадение `filterHS` и `mapHS` с эталонным поведением списков.


# Итог
Создана неизменяемая полиморфная структура данных, удовлетворяющая требованиям моноида, с операциями добавления/удаления, фильтрации, отображения и сверток. Реализованы эффективное сравнение без преобразования к спискам и подробное тестирование: HUnit проверяет базовые сценарии, QuickCheck — свойства структуры и API.