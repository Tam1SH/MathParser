{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where


data FuncType = Multiply
              | Addition
              | Division
              | Subtraction

type V = [Char]

data Expression = Expression { func::String
                             , parameters::String
                             , record::String }


instance Show Expression where
    show exp = "func: " ++ func exp ++ ", " ++ "value: " ++ parameters exp




parseP expr depth (x:xs) = case x of
                                '(' -> do expr : parseP expr' (depth + 1) xs
                                ')' -> do
                                        if depth == 1
                                        then expr' : parseP "" depth xs
                                        else parseP expr' (depth - 1) xs
                                ' ' -> parseP expr depth xs
                                _ -> parseP expr' depth xs
                                where
                                    expr' = expr ++ [x]


parseP _ _ _ = []

parseLexms = parseLexms' ""
parseLexms' lexm (x:xs) = case x of
                              ' ' -> parseLexms' lexm xs
                              '+' -> copypasta
                              '-' -> copypasta
                              '^' -> copypasta
                              '*' -> copypasta
                              '/' -> copypasta
                              _ -> parseLexms' lexm' xs
                              where
                                lexm' = lexm ++ [x]
                                copypasta = lexm : [x] : parseLexms' "" xs

parseLexms' lexm _ = [lexm]


elem' _ [] = False
elem' x (y : ys) = (x == y) || elem' x ys

in' [] _ = False
in' (x:xs) list = elem' x list || in' xs list

typeOfLexem lexm = case lexm of
                        "^" -> "Exp"
                        "+" -> "Add"
                        "-" -> "Sub"
                        "/" -> "Div"
                        "*" -> "Mul"
                        _ -> if ['x', 'y', 'z', 'a', 'b'] `in'` lexm
                                then "var"
                                else "value"


findParentheses :: t -> [Char] -> [Char] -> [Char]
findParentheses str foundExp (x:xs) = do 
                                        if x == '('
                                        then do
                                            --foundExp' <- x : foundExp
                                            findParentheses str foundExp xs
                                        else
                                            findParentheses str foundExp xs

                                        if [')'] `in'` foundExp
                                            then foundExp ++ findParentheses str foundExp ""
                                            else findParentheses str foundExp xs

findParentheses _ _ _ = []

str = "(a * (a + b))"
main :: IO()
main = do
    print $ findParentheses str "" str
    print $ parseLexms str
    print . map typeOfLexem $ parseLexms str
