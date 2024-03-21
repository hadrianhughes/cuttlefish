module Cuttlefish.Parser.Types where

primTypeP :: Parser PrimType
primTypeP = Int   <$ rword "int"
        <|> Float <$ rword "float"
        <|> Char  <$ rword "char"
        <|> Unit  <$ symbol "()"

typeConstraintP :: Parser TypeConstraint
typeConstraintP = TypeConstraint <$> typeIdentifier <*> identifier

dataConstructorP :: Parser (Text, [TypeExpr])
dataConstructorP = do
  name <- typeIdentifier
  args <- optional (parens $ typeExprP `sepBy1` comma)

  let args' = case args of
    Just xs -> xs
    Nothing -> []

  return (name, args')

typeExprP :: Parser TypeExpr
typeExprP = FuncType         <$> (typeExprP <* symbol "->") <*> typeExprP
        <|> ListType         <$> brackets typeExprP
        <|> TupleType        <$> parens (typeExprP `sepBy1` comma)
        <|> StructType       <$> braces (keyValPair `sepBy` comma)
        <|> SetType          <$> braces typeExprP
        <|> try (Constructor <$> dataConstructorP `sepBy1` pipe)
        <|> TypeVar          <$> identifier
        <|> PrimType         <$> primTypeP
        where
          keyValPair = do
            key <- (identifier <* colon)
            val <- typeExprP
            return (key, val)

typeDefnP :: Parser TypeDefn
typeDefnP = TypeDefn <$> (rword "type" *> typeIdentifier)
                     <*> angles (typeConstraintP `sepBy1` comma)
                     <*> (symbol "=" *> typeExprP)
