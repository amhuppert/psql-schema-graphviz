{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude hiding (list)

import Data.Colour.Palette.BrewerSet
import Data.Colour.SRGB
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import PostgresVis.Parse

main :: IO ()
main = do
  args <- getArgs
  case args of
    [schemaPath, outputPath] -> do
      schemaRaw <- readFile schemaPath
      case parseSchema schemaRaw of
        Left e -> die $ show e
        Right schema -> writeFile outputPath (renderDotFile schema)
    _ -> die "usage: postgres-viz <SCHEMA> <OUTPUT>"
          

pastel1 :: Vector Kolor
pastel1 = Vector.fromList $ brewerSet Pastel1 9

paired :: Vector Kolor
paired = Vector.fromList $ brewerSet Paired 12

backgroundColor :: Doc ann
backgroundColor = dquotes "#2b2b2b"

renderDotFile :: Schema -> Text
renderDotFile (Schema tables fks) = renderStrict . layoutPretty defaultLayoutOptions . vcat $
  [ text "strict digraph {"
  , indent 2 body
  , text "}"
  ]
  where
    body = vcat $
      [ text "graph" <+> attrList graphAttrs <> semi
      ] ++ map renderTableData (Set.toList tables)
    attrList = list
    graphAttrs =
      [ attr "bgcolor" backgroundColor
      ]
    attr name val = name <> text "=" <> val
    renderTableData t = vcat $
      [ text "/* Table: " <+> renderTableName t <+> text "*/"
      , renderNode t
      ] ++
      case Map.lookup t fks of
        Nothing -> [ hardline ]
        Just referenced -> (map (renderForeignKey t) $ Set.toList referenced) ++ [ hardline ]
    renderNode t = renderTableName t <+> nodeAttrs t <> semi
    nodeAttrs t =
      let color = renderColor $ calcTableColor t
       in attrList
            [ attr "color" color
            , attr "fillcolor" color
            , attr "style" (dquotes $ text "filled")
            ]
    renderColor = dquotes . pretty . sRGB24show 
    renderTableName (Table t) = dquotes . text $ t
    renderForeignKey from to =
      renderTableName from <+> text "->" <+> renderTableName to <+> attrList (edgeAttrs from to) <> semi
    edgeAttrs from _ =
      [ attr "color" $ renderColor $ calcTableColor from
      ]
    calcTableColor t = 
      let possibilities = paired
          colorIndex = hash t `mod` Vector.length possibilities
          color = Vector.unsafeIndex possibilities colorIndex
       in color
    
text :: Text -> Doc ann
text = pretty
