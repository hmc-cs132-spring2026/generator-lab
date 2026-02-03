{
module Parser (parse) where
import Lexer (tokenize)
import Tokens (Token(..))
import Expr (Expr(..))
}

-- Parser configuration
%name calc
%tokentype { Token }
%error { parseError }

-- Token declarations
%token
  '+'      { TokenPlus }
  '*'      { TokenTimes }
  '('      { TokenLParen }
  ')'      { TokenRParen }
  int      { TokenInt $$ }

%%

-- Grammar
Exp : Exp '+' Exp    { Plus  $1 $3 }
    | Exp '*' Exp    { Times $1 $3 }
    | '(' Exp ')'    { $2 }
    | int            { Int $1 }

-- Additional code
{

parseError :: [Token] -> a
parseError _ = error "Parse error"   -- not a very good error message :)

parse :: String -> Expr
parse = calc . tokenize

}
    
