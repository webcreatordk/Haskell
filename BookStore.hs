-- file:BookStore.hs

type Vector=(Double,Double)

data Shape=Circle Vector Double
		  | Poly [Vector]
		  deriving(Show)

data BookInfo=Book Int String [String]
			  deriving(Show)

bookID (Book id title authors)=id
bookTitle (Book id title authors)=title
bookAuthors (Book id title authors)=authors

data Customer = Customer {
	customerID :: CustomerID,
	customerName :: String,
	customerAddress :: Address
			} deriving (Show)

customerId :: Customer->Int
customerId (Customer id _ _)=id

customerName ::Customer->String
customerName (Customer _ name _)=name

customerAddress :: Customer->[String]
customerAddress (Customer _ _ address)=address


data Roygbiv=Red
			| Orange
			| Yellow
			| Green
			| Blue
			| Indigo
			| Violet
				deriving (Eq,Show)
