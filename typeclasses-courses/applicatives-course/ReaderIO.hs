{-# LANGUAGE InstanceSigs #-}

module ReaderIO where

newtype ReaderIO env a = ReaderIO (env -> IO a)

runReaderIO :: ReaderIO env a -> env -> IO a
runReaderIO (ReaderIO f) env = f env


instance Functor (ReaderIO env) where

    fmap :: (a -> b) -> ReaderIO env a
                     -> ReaderIO env b

    fmap f (ReaderIO g) =     -- 1
        ReaderIO $ \env ->    -- 2
            fmap f (g env)    -- 3


instance Applicative (ReaderIO env) where

    pure :: a -> ReaderIO env a

    pure x = ReaderIO (\_env -> pure x)

    liftA2 :: (a -> b -> c)
           -> ReaderIO env a
           -> ReaderIO env b
           -> ReaderIO env c

    liftA2 f (ReaderIO g) (ReaderIO h) =
        ReaderIO $ \env ->
            pure f <*> g env <*> h env


-- Configurable sorting and IO

data Order = Alphabetical | Forward | Reverse

arrange :: Order -> String -> String -> [String]
arrange Forward x y = [x, y]
arrange Reverse x y = [y, x]
arrange Alphabetical x y = sort [x, y]

data LineLimit = NoLimit | MaxLength Int

applyLineLimit :: LineLimit -> String -> String
applyLineLimit NoLimit x = x
applyLineLimit (MaxLength n) x = take n x

-- If we were writing a command-line app, we might imagine having a command-line argument parser that produces a value of type Config:
data Config =
    Config { configOrder :: Order,
             configLineLimit :: LineLimit }

arrange' :: Config -> String -> String -> [String]
arrange' config = arrange (configOrder config)

getLine' :: Config -> IO String
getLine' config =
    applyLineLimit (configLineLimit config)
        <$> getLine


fn :: (env -> a) -> ReaderIO env a
fn f = ReaderIO (\env -> pure (f env))

getAndArrange' :: Config -> IO [String]
getAndArrange' = runReaderIO (fn arrange' <*> ReaderIO getLine' <*> ReaderIO getLine')
