module OuterInner where
import Control.Monad.Trans.Except 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Reader
import Distribution.Compat.Lens (_1)
import Data.Type.Equality (inner)
-- We only need to use return once, -- because it's one big monad. 

embedded :: MaybeT
            (ExceptT String
                    (ReaderT () IO))
            Int
embedded = return 1


--- peeling away the layers
maybeUnwrap :: ExceptT String
              (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- Next
eitherUnwrap :: ReaderT () IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- Lastly
readerUnwrap :: ()
             -> IO (Either String
                           (Maybe Int)) 
readerUnwrap = runReaderT eitherUnwrap


reEmbedded :: MaybeT
             (ExceptT String
                    (ReaderT () IO))
             Int
reEmbedded = MaybeT $ ExceptT $ ReaderT $ innit
             where 
               innit :: () -> IO (Either String (Maybe Int))
               innit = pure <$> const (Right (Just 1))
