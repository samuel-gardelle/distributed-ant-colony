{-# LANGUAGE RecordWildCards #-}
module Random where

import           Control.Monad.State as S

import           Definitions

generateRandom :: Int -> StateM s Int
generateRandom bound =

  S.modify (\s@StateMachine{..} ->
      s { random= (random * 810237 + 89461367) `mod` 91283961283 } )

  >> (`mod` bound) <$> (S.gets random)

next :: Int -> State Int Int
next bound = do
    S.modify (\s -> (s * 8123787 + 12367) `mod` 9162397)
    a <- get
    return $ a `mod` bound

