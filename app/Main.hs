{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where


data FuncType = Multiply
              | Addition
              | Division
              | Subtraction


data Expression = Expression { func::String
                             , parameters::String
                             , record::String }


instance Show Expression where
    show exp = "func: " ++ func exp ++ ", " ++ "value: " ++ parameters exp

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

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


findParentheses = removeItem "" . findParentheses' False [""] "" 0

findParentheses' :: Bool -> [String] -> String -> Integer -> String -> [String]
findParentheses' foundParenthese foundExps currentExp countParenthese (x:xs) = do
    if x == ' '
    then findParentheses' foundParenthese foundExps (currentExp ++ [x])  countParenthese xs
    else
        if x == '('
        then findParentheses' True foundExps (currentExp ++ [x])  (countParenthese + 1) xs
        else
            if x == ')'
            then
                if countParenthese == 1
                    then ((currentExp ++ [x]) : foundExps) ++ findParentheses' False foundExps "" (countParenthese - 1) xs
                    else findParentheses' False foundExps (currentExp ++ [x])  (countParenthese - 1) xs
            else
                if foundParenthese
                    then findParentheses' foundParenthese foundExps (currentExp ++ [x])  countParenthese xs
                    else findParentheses' foundParenthese foundExps currentExp  countParenthese xs

findParentheses' _ _ _ _ _  = []



str = "1+1*(a+(2+6))*sin(1)+1+(1+1)+1+(1+2+1+(1+2))"
main :: IO()
main = do
    print str
    print $ findParentheses str
    print $ parseLexms str
    print . map typeOfLexem $ parseLexms str
