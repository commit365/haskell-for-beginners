## Implementing Common Algorithms

### 1. Sorting Algorithms

Sorting is a common operation in programming. Here, we will implement the **Bubble Sort** and **Quick Sort** algorithms.

#### Bubble Sort

Bubble Sort is a simple sorting algorithm that repeatedly steps through the list, compares adjacent elements, and swaps them if they are in the wrong order.

```haskell
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = foldr (\_ acc -> bubblePass acc) xs [1..length xs]
  where
    bubblePass [] = []
    bubblePass [x] = [x]
    bubblePass (x:y:rest)
      | x > y     = y : bubblePass (x:rest)
      | otherwise = x : bubblePass (y:rest)
```

#### Example Usage of Bubble Sort

```haskell
main :: IO ()
main = do
    let unsorted = [5, 3, 8, 1, 2]
    let sorted = bubbleSort unsorted
    print sorted  -- Output: [1, 2, 3, 5, 8]
```

#### Quick Sort

Quick Sort is a more efficient sorting algorithm that uses a divide-and-conquer approach.

```haskell
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot:xs) =
    let smallerSorted = quickSort [x | x <- xs, x <= pivot]
        biggerSorted = quickSort [x | x <- xs, x > pivot]
    in smallerSorted ++ [pivot] ++ biggerSorted
```

#### Example Usage of Quick Sort

```haskell
main :: IO ()
main = do
    let unsorted = [5, 3, 8, 1, 2]
    let sorted = quickSort unsorted
    print sorted  -- Output: [1, 2, 3, 5, 8]
```

### 2. Searching Algorithms

Searching is another common operation. We will implement **Linear Search** and **Binary Search**.

#### Linear Search

Linear Search checks each element in the list until it finds the target value or reaches the end of the list.

```haskell
linearSearch :: Eq a => a -> [a] -> Bool
linearSearch _ [] = False
linearSearch target (x:xs)
    | target == x = True
    | otherwise   = linearSearch target xs
```

#### Example Usage of Linear Search

```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]
    print $ linearSearch 3 numbers  -- Output: True
    print $ linearSearch 6 numbers  -- Output: False
```

#### Binary Search

Binary Search is an efficient algorithm for finding an item from a sorted list. It works by repeatedly dividing the search interval in half.

```haskell
binarySearch :: Ord a => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch target xs = binarySearchHelper target xs 0 (length xs - 1)

binarySearchHelper :: Ord a => a -> [a] -> Int -> Int -> Bool
binarySearchHelper target xs low high
    | low > high = False
    | midValue == target = True
    | midValue < target = binarySearchHelper target xs (mid + 1) high
    | otherwise = binarySearchHelper target xs low (mid - 1)
  where
    mid = (low + high) `div` 2
    midValue = xs !! mid
```

#### Example Usage of Binary Search

```haskell
main :: IO ()
main = do
    let sortedNumbers = [1, 2, 3, 4, 5]
    print $ binarySearch 3 sortedNumbers  -- Output: True
    print $ binarySearch 6 sortedNumbers  -- Output: False
```

### 3. Tree Traversal Algorithms

For tree structures, we can implement **In-Order**, **Pre-Order**, and **Post-Order** traversals.

#### Defining a Simple Binary Tree

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Example tree
myTree :: Tree Int
myTree = Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty)
```

#### In-Order Traversal

```haskell
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node value left right) = inOrder left ++ [value] ++ inOrder right
```

#### Pre-Order Traversal

```haskell
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node value left right) = [value] ++ preOrder left ++ preOrder right
```

#### Post-Order Traversal

```haskell
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node value left right) = postOrder left ++ postOrder right ++ [value]
```

#### Example Usage of Tree Traversals

```haskell
main :: IO ()
main = do
    print $ inOrder myTree   -- Output: [2, 1, 4, 3]
    print $ preOrder myTree  -- Output: [1, 2, 3, 4]
    print $ postOrder myTree -- Output: [2, 4, 3, 1]
```

### Summary

- **Sorting Algorithms**: Implemented Bubble Sort and Quick Sort.
- **Searching Algorithms**: Implemented Linear Search and Binary Search.
- **Tree Traversal**: Implemented In-Order, Pre-Order, and Post-Order traversals for binary trees.

### Practice Exercises

1. Implement a Merge Sort algorithm.
2. Write a function that performs a depth-first search (DFS) on a graph represented using an adjacency list.
3. Create a function that finds the maximum element in a binary tree.

### Conclusion

Implementing common algorithms in Haskell helps you understand both the algorithms themselves and how to express them in a functional programming context. Practice these algorithms to improve your skills in Haskell and algorithm design!