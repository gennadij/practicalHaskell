module Chapter7 where

import Control.Monad (replicateM)
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S


brockenThreeJumps :: Int  -> [Int]
brockenThreeJumps wishYear = do
  jump1 <- [-1, 3, 5]
  jump2 <- [-1, 3, 5]
  jump3 <- [-1, 3, 5]
  return $ wishYear + jump1 + jump2 + jump3

brockenJumps :: Int -> Int -> [[Int]]
brockenJumps year jumps = replicateM jumps [-1, 3 , 5]

find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f as =
  let finder f = map (\a -> if f a then Just a else Nothing)
  in  msum $ finder f as

-- ########################################################

data Client = GovOrg     { clientName :: String }
            | Company    { clientName :: String
                         , person :: Person
                         , duty :: String }
            | Individual { person :: Person }
            deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg
                | KindCompany
                | KindIndividual
                deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     , gender :: Gender }
                     deriving (Show, Eq, Ord)

data Gender = Male
            | Female
            | UnknownGender
            deriving (Show, Eq, Ord)

data Product = Product { productId :: Integer
                       , productType :: ProductType }
                       deriving (Show, Eq, Ord)

data ProductType = TimeMachine
                 | TravelGuide
                 | Tool
                 | Trip
                 deriving (Show, Eq, Ord)

data Purchase = Purchase { client :: Client
                         , products :: [Product] }
                         deriving (Show, Eq, Ord)

data PurchaseInfo = InfoClientKind           ClientKind
                  | InfoClientDuty           String
                  | InfoClientGender         Gender
                  | InfoPurchasedProduct     Integer
                  | InfoPurchasedProductType ProductType
                  deriving (Show, Eq, Ord)

newtype Transaction = Transaction (Set PurchaseInfo)
                     deriving (Eq, Ord)

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr
  (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i) $
                            S.insert (InfoPurchasedProductType t) pinfos)
  S.empty
purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c p) =
  Transaction $ clientToPurchaseInfo c `S.union` productsToPurchaseInfo p

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo (GovOrg _) = S.fromList [InfoClientKind KindGovOrg]
clientToPurchaseInfo (Company _ (Person _ _ gender_) duty_) =
  S.fromList [ InfoClientKind KindCompany
             , InfoClientDuty duty_
             , InfoClientGender gender_]
clientToPurchaseInfo (Individual (Person _ _ gender_)) =
  S.fromList [InfoClientKind KindIndividual, InfoClientGender gender_]