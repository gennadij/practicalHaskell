module GetProgWithHaskell.Unit5 (
  leftArm, rightArm, robotHead, renderHtml
) where 

import qualified Data.Map as Map

data RobotPart = RobotPart {
  name :: String,
  description :: String,
  cost :: Double,
  count :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart { 
    name = "left arm"
  , description = "left arm for face punching!"
  , cost = 1000.00
  , count = 3
}

rightArm :: RobotPart
rightArm = RobotPart { 
    name = "right arm"
  , description = "right arm for kind hand gestures" 
  , cost = 1025.00
  , count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
    name = "robot head"
  , description = "this head looks mad"
   , cost = 5092.25
   , count = 2
}

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2>", partName , "</h2>"
                          , "<p><h3> desc </h3>", partDesc, "</p>"
                          , "<p><h3> cost </h3>", partCost ,"</p>"
                          , "<p><h3> count </h3>", partCount, "</p>" ]
  where partName = name part
        partDesc = description part
        partCost = show (cost part)
        partCount = show (count part)

-- partsDB :: Map.Map Int RobotPart
-- partsDB = Map.fromList keysVals
--  where keysVals = zip [1, 2, 3] [leftArm, rightArm, robotHead]
