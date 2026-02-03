{-# LANGUAGE DeriveDataTypeable #-}

module Args
    ( Options (..)
    , options
    ) where

import           Data.Data              (Data, Typeable)

import           System.Console.CmdArgs (argPos, def, help, name, program, summary, typ,
                                         (&=))

-- | Data structure to hold command line options
data Options
  = Options
      { expr        :: String
        -- ^ The expression to evaluate
      , printTokens :: Bool
        -- ^ Whether to print the token list
      , printAst    :: Bool
        -- ^ Whether to print the AST
      }
  deriving (Data, Show, Typeable)

-- | Command line options
options :: Options
options =
    Options
        { expr = def &= argPos 0 &= typ "EXPR"
        , printTokens = False &= name "t" &= help "Print the token list"
        , printAst = False &= name "a" &= help "Print the AST"
        }
        &= program "calc"
        &= summary "calc - expression evaluator, parser, and printer"
