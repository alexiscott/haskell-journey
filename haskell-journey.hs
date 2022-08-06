-- https://stackoverflow.com/questions/60695016/what-does-non-type-variable-argument-in-the-constraint-really-mean

data Lista = Nil
            | Cons Int Lista
            | Concat Lista Lista
    deriving (Show, Eq, Ord)


cabeza :: Lista -> Int
cabeza Nil = error "Lista vacia"
cabeza (Cons x _ ) = x


cola :: Lista -> Lista
cola Nil = error "Lista vacia (empty)"
cola (Cons _ y) = y


-- Invertir with two arguments.
invertir :: Lista -> Lista -> Lista
invertir Nil l2 = l2
invertir l1 l2 =
  invertir (cola l1) (Cons (cabeza l1) l2)


invertir''' :: Lista -> Lista
invertir''' xs =
  invertir_aux' xs Nil

invertir_aux' :: Lista -> Lista -> Lista
invertir_aux' Nil l2 = l2
invertir_aux' (Cons ln lns) l2 =
  invertir_aux' lns (Cons ln l2)

-- Here we have two args.
rev :: [a] -> [a] -> [a]
rev [] l = l
rev (x : xs) l =
  rev xs (x : l)

-- Let's call our two argument function with a single argument.
rev' :: [a] -> [a]
rev' l =
  rev l []

-- Note to self. Interesting to see that this reverses the chars.

-- λ> map rev' [1,2]
-- • Non type-variable argument in the constraint: Num [a]

-- So that got me a bit confused.
-- Map has this type signature:

-- λ> :t map
-- map :: (a -> b) -> [a] -> [b]

-- Our rev' is taking two lists.
-- With map, the type of data going  into the function (this case rev') is the same as the  the type that our data list has.

-- The expectation is that the type of data coming out will be of a different type.

-- Here we can use map with integer:
-- λ> map (+2) [1,2,3]
-- [3,4,5]
