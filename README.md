# Assignment 2: Write Open Addres Set


## Киселёв Михаил Васильевич

- Хэш-множество (неизменяемая полиморфная структура данных) [src/Structure.hs](src/Structure.hs)
- Тестирование (unit + property-based) [test/Main.hs](test/Main.hs)

# Реализация

## [Структура OASet](src/Structure.hs)

Само множество: 
```haskell

data State = Empty | Deleted | Occupied
  deriving (Eq, Show)

data Slot k = Slot
  { slotState :: State
  , slotKey   :: k
  } deriving (Show)


data Slots k = Slots
  { capacity   :: Int
  , size       :: Int
  , tombstones :: Int
  , slots      :: Vector (Slot k)
  } deriving (Show)

```
является "списком списков", то есть бакетов состоящих из элементов.
Позиция (бакет, в который нужно добавить) каждого элемента вычисляется при помощи hash-функции, что обеспечивает быстрый доступ к элементам.

Основные операции:
- `insert` / `delete` — добавление и удаление с автоматической нормализацией.
- `filterHS` / `mapHS` — функциональные преобразования без протечки внутреннего представления.
- `foldlHS` / `foldrHS` — свертки по бакетам.
- `memberHS` — проверка принадлежности за счёт адресации по хешу.


**`insert`**
```haskell
insert :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
insert st = insertLoop (oaBalance st)

insertLoop :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
insertLoop s key = go (startIndex s key) 0 Nothing
  where
    cap = capacity s

    go i probe firstDel
      | probe >= cap = (s, False)
      | otherwise =
          case slots s V.! i of
            Slot Empty _ ->
              let pos = Data.Maybe.fromMaybe i firstDel
               in ( s { size       = size s + 1
                      , tombstones =
                          if isNothing firstDel
                          then tombstones s
                          else tombstones s - 1
                      , slots =
                          slots s V.// [(pos, Slot Occupied key)]
                      }
                  , True
                  )

            Slot Deleted _ ->
              go ((i + 1) .&. (cap - 1))
                 (probe + 1)
                 (case firstDel of
                    Nothing -> Just i
                    _       -> firstDel)

            Slot Occupied k
              | k == key -> (s, False)
              | otherwise ->
                  go ((i + 1) .&. (cap - 1))
                     (probe + 1)
                     firstDel
```


находим необходимый индекс (бакет), в случае превышения loadfactor
перехэшируем всё множество. Находим необходимый бакет, если такого элемента еще нет то возвращаем обновленный бакет. 
В силу иммутабельности структуры возвращаем новое множество. 



**`delete`**

```haskell
delete :: (Eq k, Hashable k) =>  Slots k -> k -> (Slots k, Bool)
delete st key = do
    case deleteLoop st key of
      (s', True)  -> (oaBalance s', True)
      result     -> result

deleteLoop :: (Eq k, Hashable k) => Slots k -> k -> (Slots k, Bool)
deleteLoop s key = go (startIndex s key) 0
  where
    cap = capacity s

    go i probe
      | probe >= cap = (s, False)
      | otherwise =
          case slots s V.! i of
            Slot Empty _ ->
              (s, False)

            Slot Occupied k
              | k == key ->
                  ( s { size       = size s - 1
                      , tombstones = tombstones s + 1
                      , slots =
                          slots s V.// [(i, Slot Deleted undefined)]
                      }
                  , True
                  )

            _ ->
              go ((i + 1) .&. (cap - 1)) (probe + 1)
```

аналогично addElement, но возвращается обновленная структура без заданного элемента.


**`filterHS`**
```haskell
filterOA :: (Eq k, Hashable k) => (k -> Bool) -> Slots k -> Slots k
filterOA p s =
  rehash $
    foldlOA step (initSlots (capacity s)) s
  where
    step acc k
      | p k       = fst (oaInsertRaw acc k)
      | otherwise = acc
```

оставляем те эелементы, которые соответствуют предикату.

**`mapHS`**
```haskell
mapOA :: (Eq k2, Hashable k2)
      => (k1 -> k2)
      -> Slots k1
      -> Slots k2
mapOA f s =
  rehash $
    foldlOA step (initSlots (capacity s)) s
  where
    step acc k =
      fst (oaInsertRaw acc (f k))
```

применяем функцию к элементам исходного множества и всталяем в новое множество (это необходимо для того, чтобы высчитать хэш на основе новых значений после применения функции)


**`foldlHS/foldrHS`**
```haskell
foldlOA :: (a -> k -> a) -> a -> Slots k -> a
foldlOA f z s = 
    V.foldl' step z (slots s)
  where
    step acc (Slot Occupied k) = f acc k
    step acc _                = acc

foldrOA :: (k -> a -> a) -> a -> Slots k -> a
foldrOA f z s = 
  V.foldr step z (slots s)
  where
    step (Slot Occupied k) acc = f k acc
    step _ acc                = acc

```




## [Тестирование](test/Spec.hs)
### Unit-тесты (HUnit)
```haskell
tests :: Test
tests = TestList
  [ TestLabel "powerTwo edge cases" test_powerTwo
  , TestLabel "initSlots empty" test_initSlots
  , TestLabel "insert/member basic" test_insertMember
  , TestLabel "insert duplicate" test_insertDuplicate
  , TestLabel "delete present" test_deletePresent
  , TestLabel "delete missing" test_deleteMissing
  , TestLabel "toList/fromList set semantics" test_toListFromList
  , TestLabel "stress insert many (resize/rehash)" test_stress
  , TestLabel "map transforms elements" test_mapTransforms
  , TestLabel "filter keeps matching elements" test_filterKeepsMatching
  , TestLabel "filter removes all elements" test_filterRemovesAll
  , TestLabel "foldl sums elements" test_foldlSums
  , TestLabel "foldr concatenates strings" test_foldrConcats
  , TestLabel "foldl on empty set" test_foldlEmpty
  , TestLabel "map on empty set" test_mapEmpty
  ]
```
Покрывают корректность вставки, удаления, фильтрации и свертки.

# Итог
Создана неизменяемая полиморфная структура данных, удовлетворяющая требованиям моноида, с операциями добавления/удаления, фильтрации, отображения и сверток. Реализованы эффективное сравнение без преобразования к спискам и подробное тестирование: HUnit проверяет базовые сценарии.