module Chapter09.GoodKingMarkov where


data Island =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  deriving (Show, Enum, Bounded)


next :: Island -> Island
next current 
  | current == maxBound = minBound
  | otherwise = succ current
  
prev :: Island -> Island
prev current 
  | current == minBound = maxBound
  | otherwise = pred current 