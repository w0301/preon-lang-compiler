module Parser.Declarations where

import Data.Maybe

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String

import Parser.Common
import Parser.Expressions


data ObjectDeclarationName
  = SimpleObjectDeclarationName String
  | GenericObjectDeclarationName String [String]
  deriving Show

parseObjectDeclarationName :: Parser ObjectDeclarationName
parseObjectDeclarationName =
  (try parseGenericObjectDeclarationName)
  <|>
  (try parseSimpleObjectDeclarationName)

parseSimpleObjectDeclarationName :: Parser ObjectDeclarationName
parseSimpleObjectDeclarationName = fmap SimpleObjectDeclarationName simpleName

parseGenericObjectDeclarationName :: Parser ObjectDeclarationName
parseGenericObjectDeclarationName = do
  name <- simpleName
  spaces >> char '<' >> spaces
  firstSubName <- simpleName
  restSubNames <- (many (spaces >> char ',' >> spaces >> simpleName))
  spaces >> char '>' >> spaces
  return $ GenericObjectDeclarationName name ([firstSubName] ++ restSubNames)


data DeclarationName
  = SimpleDeclarationName String
  | GenericDeclarationName String [DeclarationName]
  deriving Show

parseDeclarationName :: Parser DeclarationName
parseDeclarationName =
  (try parseGenericDeclarationName)
  <|>
  (try parseSimpleDeclarationName)

parseSimpleDeclarationName :: Parser DeclarationName
parseSimpleDeclarationName = fmap SimpleDeclarationName simpleName

parseGenericDeclarationName :: Parser DeclarationName
parseGenericDeclarationName = do
  name <- simpleName
  spaces >> char '<' >> spaces
  firstSubName <- parseDeclarationName
  restSubNames <- (many (spaces >> char ',' >> spaces >> parseDeclarationName))
  spaces >> char '>' >> spaces
  return $ GenericDeclarationName name ([firstSubName] ++ restSubNames)


data FieldSignature
  = FieldSignature [DeclarationName] DeclarationName
  deriving Show

parseFieldSignature :: Parser FieldSignature
parseFieldSignature = do
  firstDeclaration <- parseDeclarationName
  restDeclarations <- (many (try (spaces >> string "->" >> spaces >> parseDeclarationName)))
  return $ let declarations = [firstDeclaration] ++ restDeclarations
           in FieldSignature (take ((length declarations) - 1) declarations) (last declarations)


data FieldBody
  = EmptyFieldBody
  | ExpressionFieldBody [String] Expression
  deriving Show

parseFieldBody :: Parser FieldBody
parseFieldBody = do
  (try parseExpressionFieldBody)
  <|>
  (return EmptyFieldBody)

parseExpressionFieldBody :: Parser FieldBody
parseExpressionFieldBody = do
  spaces >> char '=' >> spaces
  spaces >> char '|' >> spaces
  firstName <- optionMaybe simpleName
  restNames <- (many (try (spaces >> char ',' >> spaces >> simpleName)))
  spaces >> char '|' >> spaces >> string "->" >> spaces
  expr <- parseExpression
  return $ ExpressionFieldBody ((maybeToList firstName) ++ restNames) expr


data FieldDeclaration
  = NormalFieldDeclaration DeclarationName FieldSignature FieldBody
  | ConstantFieldDeclaration DeclarationName FieldSignature FieldBody
  deriving Show

parseFieldDeclaration :: Parser FieldDeclaration
parseFieldDeclaration =
  (try parseConstantFieldDeclaration)
  <|>
  (try parseNormalFieldDeclaration)

parseNormalFieldDeclaration :: Parser FieldDeclaration
parseNormalFieldDeclaration = do
  spaces
  name <- parseDeclarationName
  spaces >> char ':' >> spaces
  signature <- parseFieldSignature
  body <- parseFieldBody
  return $ NormalFieldDeclaration name signature body

parseConstantFieldDeclaration :: Parser FieldDeclaration
parseConstantFieldDeclaration = do
  spaces >> string "const" >> spaces
  decl <- parseNormalFieldDeclaration
  return $ case decl of NormalFieldDeclaration name signature body -> ConstantFieldDeclaration name signature body


data ObjectDeclaration
  = AbstractObjectDeclaration ObjectDeclarationName [FieldDeclaration]
  | ConcreteObjectDeclaration ObjectDeclarationName [FieldDeclaration]
  deriving Show

parseObjectDeclaration :: Parser ObjectDeclaration
parseObjectDeclaration =
  (try parseAbstractObjectDeclaration)
  <|>
  parseConcreteObjectDeclaration

parseAbstractObjectDeclaration :: Parser ObjectDeclaration
parseAbstractObjectDeclaration = do
  spaces >> string "abstract" >> spaces
  decl <- parseConcreteObjectDeclaration
  return $ case decl of ConcreteObjectDeclaration name fields -> AbstractObjectDeclaration name fields

parseConcreteObjectDeclaration :: Parser ObjectDeclaration
parseConcreteObjectDeclaration = do
  spaces >> string "object" >> spaces
  name <- parseObjectDeclarationName
  fields <- (many parseFieldDeclaration)
  spaces >> string "end" >> spaces
  return $ ConcreteObjectDeclaration name fields
