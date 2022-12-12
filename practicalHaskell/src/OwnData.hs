module OwnData where


data InfNumber a = MinusInf
                 | PlusInf
                 | Number a deriving Show

data Client = GovOrg     { clientName :: String}
            | Company    { clientName :: String
                         , companyId :: Integer
                         , person :: Person
                         , duty :: String }
            | Individual { person :: Person } deriving Show

data Person = Person      { fName :: String
                          , lName :: String
                          , gender :: Gender } deriving Show

data Gender = Male | Female | Unknown deriving Show

data TimeMachine = TimeMachine { manufactureName :: String
                               , tMModel :: Integer
                               , tMName :: String
                               , tMType :: TMType
                               , price ::Double } deriving Show

data TMType = Past | Future deriving Show

clientGovOrg :: Client
clientGovOrg = GovOrg "GovOrg Test Test"

clientCompany :: Client
clientCompany = Company "Limited Bla Bla" 123 (Person "Mustername" "Mustername" Male) "Bla Bla"

clientIndividual :: Client
clientIndividual = Individual (Person "IndMuastername" "IndMustername" Female)

timeMachine :: TimeMachine
timeMachine = TimeMachine "TM Manufacture" 1234 "FirstTM" Past 1235.50

