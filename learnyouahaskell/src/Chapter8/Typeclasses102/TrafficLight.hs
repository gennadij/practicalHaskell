module Chapter8.Typeclasses102.TrafficLight (TrafficLight (..)) where

data TrafficLight = R | Y | G

instance Eq TrafficLight where 
  R == R = True 
  G == G = True 
  Y == Y  = True 
  _ == _ = False

instance Show TrafficLight where
  show R = "Red"
  show Y = "Yellow"
  show G = "Green"

