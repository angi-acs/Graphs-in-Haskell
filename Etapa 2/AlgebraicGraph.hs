{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node x) = S.fromList [x]
nodes (Overlay x y) = S.union (nodes x) (nodes y)
nodes (Connect x y) = S.union (nodes x) (nodes y)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node _) = S.empty
edges (Overlay x y) = S.union (edges x) (edges y)
edges (Connect x y) = S.union (S.cartesianProduct (nodes x) (nodes y)) 
                              (S.union (edges x) (edges y))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors _ Empty = S.empty
outNeighbors _ (Node _) = S.empty
outNeighbors node (Overlay x y) = S.union (outNeighbors node x) (outNeighbors node y)
outNeighbors node (Connect x y)
    | S.member node (nodes x) = S.union (nodes y) (outNeighbors node y)
    | otherwise = S.union (outNeighbors node x) (outNeighbors node y)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors _ Empty = S.empty
inNeighbors _ (Node _) = S.empty
inNeighbors node (Overlay x y) = S.union (inNeighbors node x) (inNeighbors node y)
inNeighbors node (Connect x y)
    | S.member node (nodes y) = S.union (nodes x) (inNeighbors node y)
    | otherwise = S.union (inNeighbors node x) (inNeighbors node y)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node = helper
    where
        helper Empty = Empty
        helper (Node x)
            | x == node = Empty
            | otherwise = Node x
        helper (Overlay x y) = Overlay (helper x) (helper y)
        helper (Connect x y) = Connect (helper x) (helper y)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph = removeNode old $ foldr helper graph news
    where
        helper new Empty = Empty
        helper new (Node x)
            | x == old = Overlay (Node x) (Node new)
            | otherwise = Node x
        helper new (Overlay x y) = Overlay (helper new x) (helper new y)
        helper new (Connect x y) = Connect (helper new x) (helper new y)

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node = helper
    where
        helper Empty = Empty
        helper (Node x)
            | prop x = Node node
            | otherwise = Node x
        helper (Overlay x y) = Overlay (helper x) (helper y)
        helper (Connect x y) = Connect (helper x) (helper y)
