module AlgebraicPath.Ast.Component
  ( Component (..),
    attoparsecParserOf,
    toTextBuilder,
  )
where

import AlgebraicPath.Ast.Name qualified as Name
import AlgebraicPath.Util.Prelude
import Data.Attoparsec.Text qualified as Attoparsec
import TextBuilder qualified

data Component
  = NameComponent Name.Name
  | DotComponent
  | DotDotComponent

attoparsecParserOf :: Attoparsec.Parser Component
attoparsecParserOf = do
  name <- Name.attoparsecParserOf
  if Name.null name
    then do
      mplus
        ( do
            _ <- Attoparsec.char '.'
            mplus
              (Attoparsec.char '.' $> DotDotComponent)
              (pure DotComponent)
        )
        (pure (NameComponent name))
    else pure (NameComponent name)

toTextBuilder :: Component -> TextBuilder.TextBuilder
toTextBuilder = \case
  NameComponent name -> Name.toTextBuilder name
  DotComponent -> "."
  DotDotComponent -> ".."
