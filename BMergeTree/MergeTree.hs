module MergeTree where 

data Pair a = Pair a a deriving Show

data Tree a = Leaf a
            | Node (Tree a) (Tree a) deriving Show

mergeTrees :: (a -> a -> a) -> Tree (Pair a) -> Tree (Pair a) -> Tree a
mergeTrees f (Leaf (Pair a1 a2)) (Leaf (Pair b1 b2)) = Leaf (f (f a1 a2) (f b1 b2))
mergeTrees f (Node left1 right1) (Node left2 right2) = Node (mergeTrees f left1 left2) (mergeTrees f right1 right2)

tree1 :: Tree (Pair Int)
tree1 = Node (Leaf (Pair 1 2)) (Leaf (Pair 3 4))

tree2 :: Tree (Pair Int)
tree2 = Node (Leaf (Pair 5 6)) (Leaf (Pair 7 8))

main :: IO ()
main = print (mergeTrees (+) tree1 tree2)