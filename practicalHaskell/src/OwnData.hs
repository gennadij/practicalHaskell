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

clientGovOrg :: Client
clientGovOrg = GovOrg "GovOrg Test Test"

clientCompany :: Client
clientCompany = Company "Limited Bla Bla" 123 (Person "Mustername" "Mustername" Male) "Bla Bla"

clientIndividual :: Client
clientIndividual = Individual (Person "IndMuastername" "IndMustername" Female)

data TimeMachine = TimeMachine String Integer String TMType Double

data TMType = Past | Future

timeMachine :: TimeMachine
timeMachine = TimeMachine "TM Manufacture" 1234 "FirstTM" Past 1235.50

