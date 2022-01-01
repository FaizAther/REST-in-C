

newtype Compose f g a = Compose {
        -- Unwrap it 
        getCompose :: f (g a)
    } deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
    -- fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap d p = Compose ((\ f m -> (<$>) f <$> getCompose m) d p)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose ((pure . pure) x)

    -- <*> :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (<*>) (Compose y) (Compose x) = Compose ((<*>) <$> y <*> x)

    -- (<*>) (Compose y) x = Compose ((_) <$> x)
    -- (<*>) (Compose y) x = Compose ((\d -> ((\k -> (k _)) <$> Compose y )) <$> x)

instance (Applicative f, Applicative g, Semigroup a) => Semigroup (Compose f g a) where
    (<>) c1 c2 = (<>) <$> c1 <*> c2
