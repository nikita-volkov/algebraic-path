module AlgebraicPath.Ast.Path
  ( Path (..),
    attoparsecParserOf,
    toTextBuilder,
  )
where

import AlgebraicPath.Ast.Component qualified as Component
import AlgebraicPath.Util.MonadPlus
import AlgebraicPath.Util.Prelude
import Data.Attoparsec.Text qualified as Attoparsec
import TextBuilder qualified as TextBuilder

data Path
  = Path Bool [Component.Component]

attoparsecParserOf :: Attoparsec.Parser Path
attoparsecParserOf = do
  abs <- Attoparsec.char '/' $> True <|> pure False
  components <- reverseSepBy Component.attoparsecParserOf (Attoparsec.char '/')
  return $ Path abs components

toTextBuilder :: Path -> TextBuilder.TextBuilder
toTextBuilder (Path abs components) =
  if abs
    then "/" <> relative
    else relative
  where
    relative =
      TextBuilder.intercalate "/" . fmap Component.toTextBuilder . reverse $ components
