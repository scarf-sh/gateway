{-# LANGUAGE OverloadedStrings #-}

module Scarf.Gateway.ImagePattern
  ( Pattern,
    match,
    isConstPattern,
    fromText,
    toText,
    Scarf.Gateway.ImagePattern.null,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec

-- | A pattern is represented by a sequences of Globs per path
-- segment.
newtype Pattern = Pattern {unPattern :: [[Glob]]}
  deriving (Eq, Show)

instance ToJSON Pattern where
  toJSON = toJSON . toText

instance FromJSON Pattern where
  parseJSON = withText "Pattern" $ \s ->
    case fromText s of
      Nothing -> fail "invalid pattern"
      Just p -> pure p

data Glob
  = WildCard
  | Literal Text
  | Union [[Glob]]
  deriving (Eq, Show)

null :: Pattern -> Bool
null (Pattern []) = True
null _ = False

-- | A predicate whether a given input matches the pattern.
match :: Pattern -> [Text] -> Bool
match (Pattern []) [] = True
match (Pattern (globs : rest)) (component : components) =
  matchGlob globs component && match (Pattern rest) components
match _ _ = False

matchGlob :: [Glob] -> Text -> Bool
matchGlob = go
  where
    go [] "" = True
    go [WildCard] "" = True
    go (WildCard : rest) xs
      | not (Text.null xs) =
          go rest xs || go (WildCard : rest) (Text.tail xs)
    go (Literal lit : rest) xs
      | Just xs' <- Text.stripPrefix lit xs =
          go rest xs'
      | otherwise =
          False
    go (Union globs : rest) xs =
      any (\glob -> go (glob ++ rest) xs) globs
    go [] xs =
      Text.null xs
    go (_ : _) xs =
      not (Text.null xs)

-- | Returns the textual representation of a `Pattern`.
toText :: Pattern -> Text
toText = Text.intercalate "/" . map goGlobs . unPattern
  where
    goGlobs :: [Glob] -> Text
    goGlobs = foldMap goGlob

    goGlob :: Glob -> Text
    goGlob glob = case glob of
      WildCard -> "*"
      Literal lit ->
        Text.concatMap
          ( \c ->
              if isGlobEscapedChar c
                then Text.cons '\\' (Text.singleton c)
                else Text.singleton c
          )
          lit
      Union globs ->
        "{" <> Text.intercalate "," (map goGlobs globs) <> "}"

-- | Parse a `Pattern` from a Text.
fromText :: Text -> Maybe Pattern
fromText input =
  case Megaparsec.runParser patternParser "pattern" input of
    Left _ -> Nothing
    Right p -> Just p

-- | Returns the Text represented by a Pattern iff the pattern expands
-- to exactly one text.
--
-- >>> isConstPattern "linkerd/proxy" == Just "linkerd/proxy"
-- >>> isConstPattern "linkerd/*" == Nothing
isConstPattern :: Pattern -> Maybe Text
isConstPattern pat = Text.intercalate "/" <$> traverse goGlobs (unPattern pat)
  where
    goGlobs :: [Glob] -> Maybe Text
    goGlobs = foldMap goGlob

    goGlob :: Glob -> Maybe Text
    goGlob glob = case glob of
      WildCard -> Nothing
      Literal lit -> Just lit
      Union [x] -> goGlobs x
      _ -> Nothing

type Parser = Megaparsec.Parsec Void Text

patternParser :: Parser Pattern
patternParser = do
  Pattern <$> Megaparsec.sepBy1 globParser (Megaparsec.char '/')

-- | Parser for a segment in a docker image.
globParser :: Parser [Glob]
globParser = Megaparsec.some piece
  where
    piece =
      Megaparsec.choice [wildcard, union, literal]

    wildcard =
      WildCard <$ Megaparsec.char '*'
    union =
      Union
        <$> Megaparsec.between
          (Megaparsec.char '{')
          (Megaparsec.char '}')
          (Megaparsec.sepBy1 globParser (Megaparsec.char ','))
    literal =
      Literal . Text.concat <$> Megaparsec.some (text Megaparsec.<|> escape)
    text =
      Megaparsec.takeWhile1P
        Nothing
        (\c -> not (isGlobEscapedChar c) && c /= '/' && c /= '\\')
    escape = Megaparsec.try $ do
      _ <- Megaparsec.char '\\'
      c <- Megaparsec.satisfy isGlobEscapedChar
      pure (Text.singleton c)

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar c = case c of
  '*' -> True
  '{' -> True
  '}' -> True
  ',' -> True
  _ -> False
