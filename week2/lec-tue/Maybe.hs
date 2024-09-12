module Maybe where

type Age = Integer

votingAge :: Age -> Bool
votingAge age = age >= 18

canVote :: Maybe Age -> Maybe Bool
canVote Nothing = Nothing
canVote (Just age) = Just (votingAge age)

sameAge :: Maybe Age -> Maybe Age -> Maybe Bool
sameAge Nothing _ = Nothing
sameAge _ Nothing = Nothing
sameAge (Just age1) (Just age2) = Just (age1 == age2)

ordered :: Age -> Age -> Age -> Bool
ordered age1 age2 age3 = age1 <= age2 && age2 <= age3

isTriple :: Maybe Age -> Maybe Age -> Maybe Age -> Maybe Bool
isTriple Nothing _ _ = Nothing
isTriple _ Nothing _ = Nothing
isTriple _ _ Nothing = Nothing
isTriple (Just age1) (Just age2) (Just age3) = Just (ordered age1 age2 age3)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)

canVote' :: Maybe Age -> Maybe Bool
canVote' x = maybeMap votingAge x -- first try of abstracting the Nothing and Just handling away. We only pattern match once but only for one argument. 

maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f Nothing _ = Nothing -- Now we have to start on multiple maybes, since it has two arguments
maybeMap2 f _ Nothing = Nothing
maybeMap2 f (Just x) (Just y) = Just (f x y)


sameAge' :: Maybe Age -> Maybe Age -> Maybe Bool
sameAge' x y = maybeMap2 (==) x y

maybeMap3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
maybeMap3 f Nothing _ _ = Nothing
maybeMap3 f _ Nothing _ = Nothing
maybeMap3 f _ _ Nothing = Nothing
maybeMap3 f (Just x) (Just y) (Just z) = Just (f x y z)

isTriple' :: Maybe Age -> Maybe Age -> Maybe Age -> Maybe Bool
isTriple' x y z = maybeMap3 ordered x y z


apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply Nothing _ = Nothing
apply _ Nothing = Nothing
apply (Just f) (Just x) = Just (f x)

canVote'' :: Maybe Age -> Maybe Bool
canVote'' x = Just votingAge `apply` x

sameAge'' :: Maybe Age -> Maybe Age -> Maybe Bool
sameAge'' x y = Just (==) `apply` x `apply` y

isTriple'' :: Maybe Age -> Maybe Age -> Maybe Age -> Maybe Bool
isTriple'' x y z = Just ordered `apply` x `apply` y `apply` z



-- instances

{-
instance Functor Maybe where
    fmap = mapMaybe

    fmap f x = Just f `apply` x

instance Applicative Maybe where
    pure = Just  -- Pure is the same as return in Monad, it just turns a value into a Maybe type (a -> Maybe a)
    (<*>) = apply

-- Using this we can rewrite isTriple'' above e.g.:
isTriple'' x y z = Just ordered <*> x <*> y <*> z

-- Since apply is set to be equal to <*> in the Applicative definition. 

-- <$> is basically a map function
-- e.g.: 
ordered <$> Just 1 Just 2 <*> Just 3 -- THIS IS CALLED APPLICATIVE STYLE. We use $ to apply the first argument and * to apply the other arguments. 
-}

>>= Just 1 Just 2
type Name = String

people :: Name -> Maybe Age
people "Homer" = Just 38
people "Bart" = Just 10
people "Lisa" = Just 8
people _ = Nothing

personCanVote :: Maybe Name -> Maybe Bool
personCanVote Nothing = Nothing
personCanVote (Just name) = canVote (people name)

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x

-- call the function through maybeBind (Just "Homer") people   // The just is the return / pure of the monad turns normal value into Monads Maybe type 

personCanVote' :: Maybe Name -> Maybe Bool
personCanVote' x =
    maybeBind x $ \name -> canVote (people name)

-- \name -> _ is a inline function which turns a name into a maybe b to match the maybeBind type

-- instance
{-
instance Monad Maybe where
    -- We also need a return/pure - for monad this should be Just (unsure if thats true)
    (>>=) = maybeBind
-}
