-- a simple library database
module Loan(..) where

type Person = String
type Book   = String

data Loan = Loan Person Book

-- person has borrowed the book
type Database = [(Person, Book)]

-- example base
exampleBase :: Database
exampleBase
= [("Alice", "Tintin"), ("Anna", "Little Women"),
   ("Alice", "Asterix"), ("Rory", "Tintin")]

-- books that who has borrowed
books :: Database -> Person -> [Book]
books database name = [book | (n, book) <- database, n == name]

-- who borrowed this book
borrowers :: Database -> Book -> [Person]
borrowers database book = [name | (name, b) <- database, b == book]

makeLoan :: Database -> Person -> Book -> Database
makeLoan database person book = [(person, book)] ++ database

returnLoan :: Database -> Person -> Book -> Database
returnLoan database person book = [pair | pair <- database, pair /= (person, book)]