data Bst a = Nil | Node a (Bst a) (Bst a) deriving (Show, Eq, Ord)


isEmpty :: (Ord a) => Bst a -> Bool
insert :: (Ord a) => a -> Bst a -> Bst a
find :: (Ord a) => a -> Bst a -> Bool
delete :: (Ord a) => a -> Bst a -> Bst a
deleteMax :: (Ord a) => Bst a -> (a, Bst a)
getMax :: (Ord a) => Bst a -> a
getMin :: (Ord a) => Bst a -> a
preOrder :: (Ord a) => Bst a -> [a]
postOrder :: (Ord a) => Bst a -> [a]
inOrder :: (Ord a) => Bst a -> [a]


isEmpty Nil = True
isEmpty _ = False

insert x Nil = Node x Nil Nil
insert x (Node y left right) 
       | x == y = Node y left right
       | x < y = Node y (insert x left) right
       | x > y = Node y left (insert x right)

find x Nil = False 
find x (Node y left right)
       | x == y = True 
       | x < y = find x left
       | x > y = find x right

deleteMax (Node x left Nil) = (x, left)
deleteMax (Node x left right) = let (y, ys) = deleteMax right 
                                    in (y, (Node x left ys))

delete n Nil = Nil
delete n (Node x left right) 
        | n < x = Node x (delete n left) right
        | n > x = Node x left (delete n right)
        | n == x = if isEmpty left then right else (Node y ys left) 
                    where (y, ys) = deleteMax left


getMax (Node x left right) = max x (max y z) where 
      y = if isEmpty left then x else getMax left    
      z = if isEmpty right then x else getMax right

getMin (Node x left right) = min x (min y z) where
      y = if isEmpty left then x else getMin left    
      z = if isEmpty right then x else getMin right

preOrder Nil = []
preOrder (Node x left right) = [x] ++ (preOrder left) ++ (preOrder right)

postOrder Nil = []
postOrder (Node x left right) = (postOrder left) ++ (postOrder right) ++ [x]

inOrder Nil = []
inOrder (Node x left right) = (inOrder left) ++ [x] ++ (inOrder right)

