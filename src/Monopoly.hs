module Monopoly where

import Data.Time.Clock (UTCTime)

import Entities
import Types

startingMoney :: Money
startingMoney = 10000

createVisitResult :: Entity Team -> Entity Location -- ^ owner and visited location
                  -> Entity Team -> Entity Location -- ^ visitor and last visited
                  -> VisitResult
createVisitResult = undefined

createVisit :: Entity Location -> Entity Team -> UTCTime -> Visit
createVisit = undefined

createTransactions :: VisitResult -> [Transaction]
createTransactions = undefined

createTeam :: TeamDetails -> Team
createTeam (TeamDetails nm) = Team nm startingMoney False 0
