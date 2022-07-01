-- Minimal example: parse a file, and pretty print it again
module Main where
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Text.PrettyPrint.HughesPJ
import qualified Data.ByteString as BS
import Data.List

import Language.C              -- simple API
import Language.C.System.GCC   -- preprocessor used
import Language.C.System.Preprocess
import Language.C.Data.Name
import Language.C.Syntax.Utils
import Language.C.Data.Ident

-------------------------------------------------------------------------------------------------------------------------------------

data ShowPlaceholder = ShowPlaceholder
instance Show ShowPlaceholder where
  showsPrec _ ShowPlaceholder = showString "_"

decorate :: ShowS -> ShowS
decorate app = showString "(" . app . showString ")"

-------------------------------------------------------------------------------------------------------------------------------------

usageMsg :: String -> String
usageMsg prg = render $
  text "Usage:" <+> text prg <+> hsep (map text ["CPP_OPTIONS","input_file.c"])

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . (msg++).show) return
errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg

-- | @parseCStatements input initialPos@ parses the given preprocessed C-source input and returns the AST or a list of parse errors.
parseCStatements :: InputStream -> Position -> Either ParseError CStat
parseCStatements input initialPosition =
  fmap fst $ execParser statementP inputStatement initialPosition builtinTypeNames (namesStartingFrom 0)
    where inputStatement  =  (mapBS "{ ") `BS.append` input `BS.append` (mapBS " }")
          mapBS = BS.pack . map (toEnum.fromEnum)

writeAST ast = do
    -- dump AST
    putStrLn $ (decorate (shows (fmap (const ShowPlaceholder) ast)) "")
    -- pretty print
    print $ pretty ast

main :: IO ()
main = do
    let usageErr = (hPutStrLn stderr (usageMsg "./magus") >> exitWith (ExitFailure 1))
    args <- getArgs
    when (length args < 1) usageErr
    let (opts,input_file) = (init args, last args)

    -- read
    input_stream <- readInputStream input_file

    -- parse
    ast <- errorOnLeft "Parse Error:" $
        parseCStatements input_stream (initPos input_file)

    putStrLn "------------------- original:"
    writeAST ast
    putStrLn "------------------- transpiled:"
    writeAST (transpile ast)


transpile = mapSubStmts (const False) $ \stat -> case stat of
    CExpr (Just expr) ctx
        | Just asm <- mapExpr expr
        -> CAsm (CAsmStmt Nothing (CStrLit (CString asm False) ctx) [] [] [] ctx) ctx
    _ -> stat


mapExpr (CCall (CVar (Ident func id a0) a1) params a2) =
    Just$ func++mapParams params
--    CCall (CVar (Ident ("__"++name) id a0) a1) params a2
mapExpr _ = Nothing


mapParams [] = ""
mapParams (x:xs) = (" "++)$ intercalate ", "$ map mapParam (xs++[x])


mapParam (CVar (Ident name id a0) a1)    = "%%"++name
mapParam (CConst (CIntConst num a0))     = "$"++show num

