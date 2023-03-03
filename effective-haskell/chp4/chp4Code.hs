{-# LANGUAGE RecordWildCards #-}

module Chp4Code where
import Distribution.PackageDescription (BuildType(Custom), PackageFlag (flagName))

-- CustomerInfo with record syntax
data CustomerInfo = CustomerInfo
  { firstName :: String
  , lastName :: String
  , widgetCount :: Int
  , balance :: Int 
  }

-- constructing records with named args
custGeorge :: CustomerInfo
custGeorge = 
  CustomerInfo
  { balance = 100
  , lastName = "Bird"
  , firstName = "George"
  , widgetCount = 10}

-- A disadvantage to constructing records this way is that you can’t partially apply fields to the data constructor using record syntax. For example, if we wanted to create some function to initialize new customers with some bonus items using record syntax, we would need to manually accept the missing fields as parameters:
custFactory :: String -> String -> CustomerInfo
custFactory fname lname =
  CustomerInfo { balance = 0
               , widgetCount = 5
               , firstName = fname
               , lastName = lname
               }


-- record update syntax
emptyCart :: CustomerInfo -> CustomerInfo
emptyCart customer =
  customer { widgetCount = 0
           , balance = 0
           }

------ Record wild cards
-- destructuring a record with record wild cards

showCustomer :: CustomerInfo -> String
showCustomer CustomerInfo{..} = 
  firstName
  <> " "
  <> lastName
  <> " "
  <> show widgetCount
  <> " "
  <> show balance

-- creating a new value with record wild cards
customerGeorge = 
  let firstName = "George"
      lastName = "Bird"
      widgetCount = 10
      balance = 100
  in CustomerInfo {..}

-- field names don't need to be defined as let bindings; just neeed to be in the environment / in scope
customerFactory firstName lastName =
  let widgetCount = 10
      balance = 100
  in CustomerInfo {..}


  ---------

data PreferredContactMethod = Email String
  | TextMessage String
  | Mail String String String Int

emailContact :: PreferredContactMethod
emailContact = Email "me@example.com"

textContact :: PreferredContactMethod
textContact = TextMessage "+65 12345678"

mailContact :: PreferredContactMethod
mailContact = Mail "1123 S. Road St." "Suite 712" "Examplesville, OH" 98142

confirmContact' :: PreferredContactMethod -> String
confirmContact' contact =
  case contact of
    Mail{}        -> "letter"
    Email{}       -> "email!"
    TextMessage{} -> "text!"

-------
{-
data CustomerInfo = CustomerInfo 
  { customerName :: String
  , customerBalance :: Int
  }

data EmployeeInfo = EmployeeInfo 
  { employeeName :: String
  , employeeManagerName :: String , employeeSalary :: Int
  }

data Person
  = Customer CustomerInfo
  | Employee EmployeeInfo

-}
type Booly a = Bool
data Rec a = MkRec { foo :: Booly a, bar :: a, baz :: Double} 
  deriving Show
example = MkRec { foo = True, bar = 10, baz = 3.14}

f x = x { bar = 5}






