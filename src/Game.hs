{-# LANGUAGE DeriveFunctor #-}
module Game where

import Control.Monad.Except
import Control.Monad.Free

import Models

data Account = TeamAcc Team | Bank

data GameF token next
  = GetRent Site ((Currency, [DieResult]) -> next)
  | Transfer Currency Account Account (Bool -> next)
  | CreateSite Site (token -> next)
  | CreateTeam String Currency (token -> next)
  | GetTeam token (Maybe Team -> next)
  | GetChanceCard token (Maybe ChanceCard -> next)
  | GetSite token (Maybe Site -> next)
  | CreateChanceCard ChanceCard (token -> next)
  | DrawChanceCards Int ([ChanceCard] -> next)
  | GetOwner Site (Maybe Team -> next)
  | PutInJail Team next
  | FreeFromJail Team (Team -> next) -- we need to recurse with the new value
  | PayStartBonus Team next
  | IsRepeatedVisit Site Team (Bool -> next) -- also stores visits
  | MakeOwner Site Team next
  deriving Functor

type GameAPI token = Free (GameF token)

getTeam :: token -> GameAPI token (Maybe Team)
getTeam teamTk = liftF $ GetTeam teamTk id

getSite :: token -> GameAPI token (Maybe Site)
getSite siteTk = liftF $ GetSite siteTk id

getChanceCard :: token -> GameAPI token (Maybe ChanceCard)
getChanceCard cardTk = liftF $ GetChanceCard cardTk id

isRepeatedVisit :: Site -> Team -> GameAPI token Bool
isRepeatedVisit site team = liftF $ IsRepeatedVisit site team id

getRent :: Site -> GameAPI token (Currency, [DieResult])
getRent site = liftF $ GetRent site id

transfer :: Currency -> Account -> Account -> GameAPI token Bool
transfer amount src dest = liftF $ Transfer amount src dest id

makeOwner :: Site -> Team -> GameAPI token ()
makeOwner site team = liftF $ MakeOwner site team ()

getOwner :: Site -> GameAPI token (Maybe Team)
getOwner site = liftF $ GetOwner site id

createSite :: Site -> GameAPI token token
createSite site = liftF $ CreateSite site id

createTeam :: String -> Currency -> GameAPI token token
createTeam name funds = liftF $ CreateTeam name funds id

putInJail :: Team -> GameAPI token ()
putInJail team = liftF $ PutInJail team ()

payStartBonus :: Team -> GameAPI token ()
payStartBonus team = liftF $ PayStartBonus team ()

freeFromJail :: Team -> GameAPI token Team
freeFromJail team = liftF $ FreeFromJail team id

drawChanceCards :: Int -> GameAPI token [ChanceCard]
drawChanceCards count = liftF $ DrawChanceCards count id

createChanceCard :: ChanceCard -> GameAPI token token
createChanceCard card = liftF $ CreateChanceCard card id
