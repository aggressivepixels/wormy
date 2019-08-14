module NonEmptyList exposing
    ( NonEmptyList
    , all
    , cons
    , dropLast
    , from
    , head
    , length
    , tail
    , toList
    )


type NonEmptyList a
    = NonEmptyList a (List a)


from : a -> List a -> NonEmptyList a
from x xs =
    NonEmptyList x xs


cons : a -> NonEmptyList a -> NonEmptyList a
cons newX (NonEmptyList x xs) =
    NonEmptyList newX (x :: xs)


head : NonEmptyList a -> a
head (NonEmptyList x _) =
    x


tail : NonEmptyList a -> List a
tail (NonEmptyList _ xs) =
    xs


all : (a -> Bool) -> NonEmptyList a -> Bool
all f (NonEmptyList x xs) =
    List.all f (x :: xs)


dropLast : NonEmptyList a -> NonEmptyList a
dropLast (NonEmptyList x xs) =
    NonEmptyList x
        (let
            len =
                List.length xs
         in
         case len of
            0 ->
                []

            _ ->
                List.take (len - 1) xs
        )


toList : NonEmptyList a -> List a
toList (NonEmptyList x xs) =
    x :: xs


length : NonEmptyList a -> Int
length (NonEmptyList _ xs) =
    1 + List.length xs
