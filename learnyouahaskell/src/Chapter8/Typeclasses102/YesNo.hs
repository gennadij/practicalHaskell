module Chapter8.Typeclasses102.YesNo where
import Chapter8.Typeclasses102.TrafficLight (TrafficLight (..))
class YesNo a where 
  yesno :: a -> Bool 

instance YesNo Int where
  yesno 0 = False 
  yesno _ = True

instance YesNo [a] where
  yesno [] = False 
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe m) where
  yesno (Just _) = True 
  yesno Nothing  = False

instance YesNo TrafficLight where
  yesno R = False 
  yesno _ = True

yesNoIf :: (YesNo y) => y -> a -> a -> a
yesNoIf yesnoVal yesRes noRes = 
  if yesno yesnoVal then yesRes else noRes