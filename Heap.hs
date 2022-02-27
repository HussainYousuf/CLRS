class Heap heap where
    {-# MINIMAL
        isEmpty
        , empty
        , (singleton | insert)
        , (fromList | (singleton, merge))
        , (insert | (merge, singleton))
        , (merge | (decompose, insert))
        , (decompose | (findMax, deleteMax))
        #-}

    type Elem heap

    isEmpty :: heap -> Bool

    findMax :: heap -> Maybe (Elem heap)
    findMax = (fst <$>) <$> decompose

    empty :: heap

    singleton :: Elem heap -> heap
    singleton = (`insert` empty)

    fromList :: [Elem heap] -> heap
    fromList xs =
        case go (map singleton xs) of
            [heap] -> heap
            [] -> empty
            _ -> error "Fatal error. Did not converge to a single heap."
      where
        go [] = []
        go [x] = [x]
        go (x : y : rest) = go (merge x y : go rest)

    decompose :: heap -> Maybe (Elem heap, heap)
    decompose heap = case (findMax heap, deleteMax heap) of
        (Just heapMax, Just heapRest) -> Just (heapMax, heapRest)
        (Nothing, Nothing) -> Nothing
        (Just _, Nothing) -> error "Impossible happened. There is a max but the heap is empty."
        (Nothing, Just _) -> error "Impossible happened. Heap is non-empty but there is a max."

    insert :: Elem heap -> heap -> heap
    insert elem = merge $ singleton elem

    deleteMax :: heap -> Maybe heap
    deleteMax = (snd <$>) <$> decompose

    merge :: heap -> heap -> heap
    merge heap1 heap2 = case decompose heap1 of
        Just (maxElem, restHeap) -> merge restHeap $ maxElem `insert` heap2
        Nothing -> heap2

    rank :: heap -> Int
    rank = undefined

data LeftistHeap a = Leaf | Node a Int (LeftistHeap a) (LeftistHeap a)
    deriving (Eq, Show)

instance (Ord a) => Heap (LeftistHeap a) where
    type Elem (LeftistHeap a) = a

    rank Leaf = 0
    rank (Node _ r _ _) = r

    merge h1 Leaf = h1
    merge Leaf h2 = h2
    merge h1@(Node x _ l1 r1) h2@(Node y _ l2 r2)
        | x <= y = mkNode x l1 $ merge r1 h2
        | otherwise = mkNode y l2 $ merge r2 h1
      where
        mkNode elem left right
            | rank left >= rank right = Node elem (rank right + 1) left right
            | otherwise = Node elem (rank left + 1) right left

    decompose Leaf = Nothing
    decompose (Node a _ l r) = Just (a, merge l r)

    empty = Leaf

    isEmpty = (== empty)

    singleton x = Node x 1 Leaf Leaf

heapSort :: forall a. (Ord a) => [a] -> [a]
heapSort xs = toList $ decompose xs'
  where
    toList Nothing = []
    toList (Just (x, xs)) = x : toList (decompose xs)
    xs' = fromList xs :: LeftistHeap a