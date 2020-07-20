-- Based on:
-- https://github.com/brownplt/webbits/blob/master/src/BrownPLT/JavaScript/PrettyPrint.hs
module Pretty where

import           Prelude          hiding (const, (<>))
import           Text.PrettyPrint

import           Syntax

import           Data.Map.Strict  as Map hiding (foldl, map)

const :: Const -> Doc
const (CInt n)      = int n
const (CBool True)  = text "true"
const (CBool False) = text "false"
const CUndefined    = text "undefined"

trace :: Trace -> Doc
trace (TConst c) = const c
trace (TId x) = text x
trace (TClos clos) =
  parens $
  sep $
  punctuate comma $
  map (\(k, v) -> (quotes (text k)) <+> text "->" <+> trace v) $ toList clos
trace (TFrom lval field) = (trace lval) <> text "." <> text field
-- TODO(arjun): Parentheses needed
trace (TOp2 Add e1 e2) = trace e1 <+> text "+" <+> trace e2
trace (TOp2 Sub e1 e2) = trace e1 <+> text "-" <+> trace e2
trace (TOp2 Eq e1 e2) = trace e1 <+> text "==" <+> trace e2
trace (TOp2 OGT e1 e2) = trace e1 <+> text ">" <+> trace e2
-- End of todo
trace (TSeq ts) = sep [lbrace, nest 4 $ sep (map trace ts), rbrace]
trace (TIf t1 t2 t3) =
  sep [text "if" <+> parens (trace t1), trace t2, text "else", trace t3]
trace (TWhile t1 t2) = vcat [text "while" <+> parens (trace t1), trace t2]
trace (TLabel l t) = sep [text l <+> text ":", trace t]
trace (TBreak l t) = sep [text "break" <+> text l, trace t <> text ";"]
trace (TLet x t) = text "let" <+> text x <+> text "=" <+> trace t <> text ";"
trace (TSet x t) = trace x <+> text "=" <+> trace t <> text ";"
trace TUnknown = text "💣"

prettyTrace t = render (trace t)
