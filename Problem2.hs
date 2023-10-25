import Cp
import Data.List (nub, sort)


mkInd :: (Eq a, Eq b, Ord b, Ord c) => ([(a, [b])] , [(c, [a])]) -> [(b, [c])]
mkInd = sort . map (id >< sort) . f' . g . (f >< f)



--Função que transforma algo do tipo (A x B*)* em (A x B)*
f :: [(a, [b])] -> [(a, b)]
f x = [(a, b) | (a, bs) <- x, b <- bs]


--Função que transforma algo do tipo (A x B)* em (A x B*)*, ou seja, fº
f' :: Eq a => [(a, b)] -> [(a, [b])]
f' x = [(a1, [b | (a2, b) <- x, a1 == a2]) | a1 <- y]
    where y = (nub . map fst) x


--Função para juntar de acordo com as suas chaves
g :: Eq a => ([(a, b)], [(c, a)]) -> [(b, c)]
g (x, y) = [(b, c) | (a1, b) <- x, (c, a2) <- y, a1 == a2]