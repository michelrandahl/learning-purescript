module Main where

import Prelude

import Effect (Effect)
--import Effect.Console (log)
import Effect.Class.Console (log)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff (Aff, Fiber, delay, forkAff, launchAff_, killFiber, joinFiber)
import Effect.Exception (error)
import Data.Time.Duration (Milliseconds(..))

data TickTock = Tick | Tock
derive instance Eq TickTock

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  clock ttAVar

data BombState = WaitingTick | WaitingTock
derive instance Eq BombState

bomb :: AVar TickTock -> Int -> Aff Unit
bomb ttAVar detinationCount = go 0 WaitingTick where
  go :: Int -> BombState -> Aff Unit
  go count state = do
    if count == detinationCount then log "boom!"
    else do
      delay (Milliseconds 500.0)
      tt <- AVar.read ttAVar
      case state of
        WaitingTick ->
          -- `*>` is operator alias for applySecond which discards the effect of the first arg
          if tt == Tick then log "Tick" *> go count WaitingTock
          else go count state
        WaitingTock ->
          if tt == Tock then log "Tock" *> go (count + 1) WaitingTick
          else go count state

main :: Effect Unit
main = launchAff_ do
  ttAVar <- AVar.empty
  clockFiber <- forkAff $ clock ttAVar
  bombFiber <- forkAff $ bomb ttAVar 3
  AVar.put Tick ttAVar
  -- TODO read up on the ~> in type signatures
  joinFiber bombFiber
  killFiber (error "Exploded") clockFiber




