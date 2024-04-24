-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

monadd :: (Monad m, Num b) => m b -> m b -> m b
monadd mx my =
    mx >>= (\x -> my >>= (\y -> return $ x+y ))