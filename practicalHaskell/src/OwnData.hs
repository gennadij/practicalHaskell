module OwnData where


data InfNumber a = MinusInf
                 | PlusInf
                 | Number a deriving Show

data Client i = GovOrg   { clientId :: i
                         , clientName :: String}
              | Company  { clientId :: i
                         , clientName :: String
                         , companyId :: Integer
                         , person :: Person
                         , duty :: String }
              | Individual { clientId :: i
                         , person :: Person } deriving Show

data Person = Person     { fName :: String
                        , lName :: String
                        , gender :: Gender } deriving Show

data Gender = Male | Female | Unknown deriving Show

data TimeMachine = TimeMachine { manufactureName :: String
                               , tMModel :: Integer
                               , tMName :: String
                               , tMType :: TMType
                               , price ::Double } deriving Show

data TMType = Past | Future deriving Show

clientGovOrg :: Num a => Client a
clientGovOrg = GovOrg 1 "GovOrg Test Test"

clientCompany :: Num a => Client a
clientCompany = Company 2 "Limited Bla Bla" 123 (Person "Mustername" "Mustername" Male) "Bla Bla"

clientIndividual :: Num a => Client a
clientIndividual = Individual 3 (Person "IndMuastername" "IndMustername" Female)

timeMachine :: TimeMachine
timeMachine = TimeMachine "TM Manufacture" 1234 "FirstTM" Past 1235.50
-- ====================================================================================================================

data ClientLens i = GovOrgLens             { _clientIdLens :: i
                                           , _clientNameLens :: String}
                  | CompanyLens            { _clientIdLens :: i
                                           , _clientNameLens :: String
                                           , _companyIdLens :: Integer
                                           , _personLens :: PersonLens
                                           , _dutyLens :: String }
                  | IndividualLens         { _clientIdLens :: i
                                           , _personLens :: PersonLens } deriving Show

data PersonLens = PersonLens               { _fNameLens :: String
                                           , _lNameLens :: String
                                           , _genderLens :: Gender
                                           } deriving Show

data TimeMachineLens = TimeMachineLens     { _manufactureNameLens :: String
                                           , _tMModelLens :: Integer
                                           , _tMNameLens :: String
                                           , _tMTypeLens :: TMType
                                           , _priceLens ::Double } deriving Show

data KMeansStateLens e v = KMeansStateLens { _centroids :: [v]
                                           , _points :: [e]
                                           , _e :: Double -- error
                                           , _threshold :: Double
                                           , _steps :: Int }

data KMeansStateComb v = KMeansStateComb   { centroids__ :: [v]
                                           , threshold__ :: Double
                                           , steps__ :: Int } deriving Show

data KMeansStateCombLens v = KMeansStateCombLens   { _centroidsLens :: [v]
                                                   , _thresholdLens :: Double
                                                   , _stepsLens :: Int } deriving Show

data Settings e v = Settings                { i :: Int -> [e] -> [v]
                                            , k :: Int
                                            , th :: Double
                                            , user :: Person }

