-- file: ch03/convexhull.hs

-- Graham Scan algorithm implemented in Haskell
-- 
-- Excercises 10, 11 and 12 of Chapter 3 of the Real World Haskell book
-- http://book.realworldhaskell.org/read/defining-types-streamlining-functions.html
--
-- about Graham Scan - http://en.wikipedia.org/wiki/Graham_scan

import Data.List (sortBy)


data Direction = Clockwise | CounterClockwise | Collinear
                 deriving Eq


-- improvement: How to use Num to accept a wider range of numeric types?
data Vector = Vector {
                vectorX :: Int,
                vectorY :: Int }
              deriving (Eq, Show)

-- Implements Ord class for the Vector type
-- it makes possible to use comparison functions such as
-- min :: Ord a => a -> a -> a

instance Ord Vector where
    -- compare :: Ord a => a -> a -> Ordering
    compare a b | (vectorY a) < (vectorY b) = LT
            | (vectorY a) > (vectorY b) = GT
            | (vectorX a) < (vectorX b) = LT
            | (vectorX a) > (vectorX b) = GT
            | otherwise = EQ



-- determines the turn by calculating the cross product of the three vectors
calcTurn       :: Vector -> Vector -> Vector -> Direction
calcTurn a b c | xprod > 0 = CounterClockwise
               | xprod < 0 = Clockwise
               | otherwise = Collinear
               where xprod = crossProduct a b c

crossProduct :: Vector -> Vector -> Vector -> Int
crossProduct a b c = ((vectorX b) - (vectorX a)) * ((vectorY c) - (vectorY a)) 
                   - ((vectorY b) - (vectorY a)) * ((vectorX c) - (vectorX a))


-- from excercise ch03 - ex11
--   * I do not use it in the convex hull algorithm
-- calcListTurns :: [Vector] -> [Direction]
-- calcListTurns vs
    -- | length vs < 3  = []
    -- | length vs == 3 = [headTurn]
    -- | otherwise      = headTurn : calcListTurns (tail vs)
    -- where headTurn = calcTurn (vs !! 0) (vs !! 1) (vs !! 2)


-- get the lowest Vector in a list
getLowestVector :: [Vector] -> Vector
getLowestVector []      = error "Vector list is empty"
getLowestVector (v:vs)  = foldl min v vs

    

-- sort a vector list by the angle they and the point P makes with the x-axis
-- this sorting algorithm recalculates the angles multiple times
--      * simpler, but consumes processor
sortByTheta' :: Vector -> [Vector] -> [Vector]
sortByTheta' p vs = sortBy theta' vs
    where theta' a b -- the function that compares the angle made by P-A and P-B
            | ta > tb   = GT
            | ta < tb   = LT
            | otherwise = EQ
            where ta = theta p a
                  tb = theta p b


-- sort a vector list by the angle they and the point P makes with the x-axis
-- makes tuples of (vector, theta) avoiding recalculation of the angles
--      * consumes more memory than sortByTheta'
sortByTheta :: Vector -> [Vector] -> [Vector]
sortByTheta p vs = (unpack . sort . calcNpack) vs
    where 
        -- calculates theta and combines in tuple
        calcNpack = (zip vs) . (map (\v -> theta p v)) 
        -- sorts the thetas
        sort      = sortBy (\a b -> compare (snd a) (snd b)) 
        -- untuples to list of vertors
        unpack    = fst . unzip


-- calculates the angle that a and b makes with the x-axis
theta :: RealFloat a => Vector -> Vector -> a
theta a b = atan2 dy dx
    where dy = fromIntegral((vectorY b) - (vectorY a))
          dx = fromIntegral((vectorX b) - (vectorX a))



-- computes the convex hull of a bunch of vectors
-- result: a list of vertors containing the hull
convexHull :: [Vector] -> [Vector] 
convexHull vs 
    | length vs < 3  = error "the list of vectors must have at least 3 elements"
    | otherwise      = p : a : (hullScan a (svs !! 1) (svs !! 2) (drop 3 svs))
    
    where 
        -- P is the initial point for the hull scan
        p   = getLowestVector vs
        -- "remove" p from vs and sort by theta
        svs = ((sortByTheta p) . (filter (\v -> p /= v))) vs
        -- the convexhull always contains A because has the maximum forward angle
        a   = head svs 



-- scans the vector list to find the appropriate hull
hullScan :: Vector -> Vector -> Vector -> [Vector] -> [Vector]
-- a is the initial vector (already accepted)
-- rvs is remaining vs
hullScan a b c rvs 
    -- base case (at the end of the list)
    | null rvs  = if turn == Clockwise
                    then [c]
                    else b : [c]

    -- b makes a clockwise turn, so discard it
    | turn == Clockwise  = hullScan a c d (tail rvs)

    -- b is collinear or counterclockwise turn, so use it
    | otherwise          = b : (hullScan b c d (tail rvs))

    where d     = head rvs
          turn  = calcTurn a b c


-- some predefined vector lists to test the algorithm
mypoints = [(Vector 7 7), (Vector 5 5), (Vector 3 3), (Vector 6 0), 
    (Vector 0 6), (Vector 6 12), (Vector 12 6), (Vector 6 6), (Vector 10 1)] 

mypoints' = [(Vector 0 0), (Vector 10 0), (Vector 0 10), (Vector 10 10), 
    (Vector 5 5), (Vector 7 8)] 

mypoints'' = [(Vector 6 0), (Vector 0 6), (Vector 6 12), (Vector 12 6), 
    (Vector 6 6), (Vector 10 1)] 