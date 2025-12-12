# Assignment 1: Basics of Haskell


## Киселёв Михаил Васильевич
- Первая задача [Первая задача](https://projecteuler.net/problem=21) 
- Вторая задача [Вторая задача](https://projecteuler.net/problem=9)

## [Первая задача](https://projecteuler.net/problem=21)
Реализация через рекурсию: 
```haskell
divisionSumRec' :: Int -> Int -> Int
divisionSumRec' n division
    | division > n `div` 2 = 0
    | n `mod` division == 0 = (+) division (divisionSumRec' n (division + 1) )
    | otherwise = divisionSumRec' n (division + 1)

amicableNumbersSumRec' :: Int -> Int
amicableNumbersSumRec' a
    | a > 10000 = 0
    | a == sum_b && a /= b = (+) (a + b) (amicableNumbersSumRec' (a+1))
    | otherwise = amicableNumbersSumRec' (a+1)
        where b = divisionSumRec' a 1
              sum_b = divisionSumRec' b 1
```
Реализация через хвостовую рекурсию: 
```haskell
divisionSumRecTail' :: Int -> Int -> Int -> Int
divisionSumRecTail' n division acc
    | division > n `div` 2 = acc
    | n `mod` division == 0 = divisionSumRecTail' n (division + 1) (acc + division)
    | otherwise = divisionSumRecTail' n (division + 1) acc

amicableNumbersSumRecTail' :: Int -> Int -> Int
amicableNumbersSumRecTail' a acc
    | a > 10000 = acc
    | a == sum_b && a /= b = amicableNumbersSumRecTail' (a+1) (acc + a + b)
    | otherwise = amicableNumbersSumRecTail' (a+1) acc
        where b = divisionSumRecTail' a 1 0
              sum_b = divisionSumRecTail' b 1 0 
```

Реализация через генерацию, фильтрацию и свёртку (generate/filter/reduce): 
```haskell
allVariants' = [(x, divisionSumRecTail' x 1 0) | x <- [1..10000]]

targetCondition :: (Int, Int) -> Bool
targetCondition (x, y) = (x /= y) && divisionSumRecTail' y 1 0 == x

targetCasesFilter = filter targetCondition allVariants'

finalSum = foldl (\acc (x, y) -> acc + x + y) 0 targetCasesFilter

```
Реализация через map
```haskell
checkCondition :: (Int, Int) -> Int
checkCondition (x, y)
    | x /= y && divisionSumRecTail' y 1 0 == x = x + y
    | otherwise = 0

mapTargerCases = map checkCondition allVariants' 

targetCasesMap = sum mapTargerCases

```

Реализация через бесконечные списки: 
```haskell
divisionSum :: Int -> Int
divisionSum n = sum [d | d <- [1..(n `div` 2)], n `mod` d == 0]

areAmicable :: Int -> Int -> Bool
areAmicable a b = (a /= b) && (divisionSum a == b) && (divisionSum b == a)

amicableNumbers :: [(Int, Int)]
amicableNumbers = [(a, b) | a <- [1..10000], let b = divisionSum a, areAmicable a b]

amicableNumbersSum :: Int
amicableNumbersSum =
    sum [a + b | (a, b) <- takeWhile (\(x, _) -> x <= 10000) amicableNumbers]
```

Реализация на Python: 
```python
def amicable_numbers_sum():
    sum = 0
    for a in range(1, 10001):
        sum_a = 0
        for division in range(1, a // 2 + 1):
            if a % division == 0:
                sum_a += division

        sum_b = 0
        for division in range(1, sum_a // 2 + 1):
            if sum_a % division == 0:
                sum_b += division

        if a == sum_b and a != sum_a:
            sum += a
            sum += sum_a

    return sum


if __name__ == "__main__":
    print(amicable_numbers_sum())
```

Реализация наиболее лаконична с использованием map и свёртывания, вариант через рекурсию даёт более быстрый результат, чем вариант, написанный на Python.


## [Вторая задача](https://projecteuler.net/problem=9)

Реализация через рекурсию:
```haskell
checkTripleRec' :: Int -> Int -> Int
checkTripleRec' n m
    | m >= 25 = 1
    | n >= 25 = checkTripleRec' 0 (m + 1)
    | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = checkTripleRec' (n + 1) m

```
Реализация через хвостовую рекурсию:
```haskell
allVariantsPairs' :: [(Int, Int)]
allVariantsPairs' = [(x, y) | x <- [1..25], y <- [1..25]]

checkTripleRecTail :: [(Int, Int)] -> Int
checkTripleRecTail [] = 1  
checkTripleRecTail ((m, n) : xs)
  | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
  | otherwise = checkTripleRecTail xs
```

Реализация через свертку (foldl):
```haskell
allVariants' = [(n, m) | n <- [1..25], m <- [1..25]]

targetCondition :: (Int, Int) -> Bool
targetCondition (n, m) = 2 * m ^ 2 + 2 * m * n == 1000

targetCase = filter targetCondition allVariants'

targetMul :: [(Int, Int)] -> Int
targetMul = foldl (\acc (n, m) -> acc + (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)) 0

checkTripleReduce = targetMul targetCase
```
Реализация через map
```haskell
checkCondition :: (Int, Int) -> Int
checkCondition (n, m)
    | 2 * m ^ 2 + 2 * m * n == 1000 = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)
    | otherwise = 0

targetCases' = map checkCondition allVariants'
getCase = filter (> 0) targetCases'

checkTripleMap = head getCase
```

Реализация через бесконечные списки:
```haskell
allVariantsInf' = [(n, m) | n <- [1..], m <- [1..25]]

isValidPair :: (Int, Int) -> Bool
isValidPair (n, m) = 2 * m ^ 2 + 2 * m * n == 1000

computeResult :: Maybe (Int, Int) -> Int
computeResult (Just (n, m)) = (m ^ 2 - n ^ 2) * 2 * m * n * (m ^ 2 + n ^ 2)

checkTripleInfinite :: Maybe (Int, Int)
checkTripleInfinite = find isValidPair allVariantsInf'

```

Реализация на Python:
```python
def find_triple_cycle():
    for n in range(25):
        for m in range(25):
            if 2 * m ** 2 + 2 * m * n == 1000:
                return (m ** 2 - n ** 2) * 2 * m * n * (m ** 2 + n ** 2)
            

if __name__ == "__main__":
    print(find_triple_cycle())
```

Прямолинейная реализация на Python наиболее простая, но вариант через реккурсию даёт более быстрый и понятный код

# Итог 
Haskell позволяет решать задачи лаконично и эффективно, но не всегда нагляднл, в сравнении с традиционными языками программирования 