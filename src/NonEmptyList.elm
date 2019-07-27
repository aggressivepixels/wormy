module NonEmptyList exposing
    ( NonEmptyList
    , any
    , cons
    , from
    , head
    , init
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


any : (a -> Bool) -> NonEmptyList a -> Bool
any f (NonEmptyList x xs) =
    if f x then
        True

    else
        List.any f xs


init : NonEmptyList a -> NonEmptyList a
init (NonEmptyList x xs) =
    NonEmptyList x
        (case List.reverse xs of
            [] ->
                []

            _ :: xs_ ->
                List.reverse xs_
        )


toList : NonEmptyList a -> List a
toList (NonEmptyList x xs) =
    x :: xs
