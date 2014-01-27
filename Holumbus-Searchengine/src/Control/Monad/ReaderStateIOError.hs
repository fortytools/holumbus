{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- ------------------------------------------------------------

module Control.Monad.ReaderStateIOError
    ( module Control.Monad.ReaderStateIOError
    )
where
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State

import           System.IO.Error

-- ------------------------------------------------------------

-- |
-- reader state io error monad implemented directly without any monad transformers

newtype ReaderStateIOError env state res = RSIO ( env -> state -> IO (Either String res, state) )

instance Functor (ReaderStateIOError env state) where
    fmap  = liftM

instance Applicative (ReaderStateIOError env state) where
    pure  = return
    (<*>) = ap

instance Monad (ReaderStateIOError env state) where
    return v
        = RSIO $ \ _e s -> return (Right v, s)

    RSIO cmd >>= f
        = RSIO $ \ e s -> do
                          (r', ! s') <- cmd e s
                          either (\ err -> return (Left err, s'))
                                 (\ x   -> let RSIO cmd2 = f x in cmd2 e s') $ r'


instance MonadIO (ReaderStateIOError env state) where
    liftIO a
        = RSIO $ \ _e s -> do
                           r <- tryIOError a
                           return ( either (Left . show) Right r
                                  , s
                                  )

instance MonadState state (ReaderStateIOError env state) where
    get
        = RSIO $ \ _e  s -> return (Right s, s)

    put s
        = RSIO $ \ _e _s -> return (Right (), s)

instance MonadReader env (ReaderStateIOError env state) where
    ask
        = RSIO $ \  e  s -> return (Right e, s)

    local f (RSIO cmd)
        = RSIO $ \  e  s -> cmd (f e) s

instance MonadError String (ReaderStateIOError env state) where
    throwError err
        = RSIO $ \ _e s -> return (Left err, s)

    catchError (RSIO cmd) h
        = RSIO $ \ e s -> do (r', ! s') <- cmd e s
                             either (\ err -> runReaderStateIOError (h err) e s)
                                    (\ x   -> return (Right x, s')) $ r'

modifyIO                :: (state -> IO state) -> ReaderStateIOError env state ()
modifyIO f              = do
                          s0 <- get
                          s1 <- liftIO (f s0)
                          put s1

runReaderStateIOError   :: ReaderStateIOError env state res  ->
                           env -> state -> IO (Either String res, state)
runReaderStateIOError (RSIO cmd) e s
    = cmd e s

-- ------------------------------------------------------------
