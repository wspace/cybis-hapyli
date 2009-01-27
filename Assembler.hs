module Assembler (assemble) where

import Ast (Instruction(..))
import Data.List (nub, mapAccumR)
import Data.Maybe (catMaybes, fromJust)
import Data.Bits (shiftR)

assemble :: [Instruction] -> String
assemble ins = let codesTable = numberLabels $ getLabels ins
               in  concat $ map (translate codesTable) ins

s = ' '
t = '\t'
f = '\n'

translate :: [(String, Integer)] -> Instruction -> String

translate _ (Push n)        = s : s : (toWhitespace n)
translate _ (Dup)           = s : f : s : []
translate _ (Copy n)        = s : t : s : (toWhitespace n)
translate _ (Swap)          = s : f : t : []
translate _ (Pop)           = s : f : f : []
translate _ (Slide n)       = s : t : f : (toWhitespace n)

translate _ (Add)           = t : s : s : s : []
translate _ (Sub)           = t : s : s : t : []
translate _ (Mul)           = t : s : s : f : []
translate _ (Div)           = t : s : t : s : []
translate _ (Mod)           = t : s : t : t : []

translate _ (Store)         = t : t : s : []
translate _ (Load)          = t : t : t : []

translate codes (Label lbl) = f : s : s : (translateLabel codes lbl)
translate codes (Call lbl)  = f : s : t : (translateLabel codes lbl)
translate codes (Jump lbl)  = f : s : f : (translateLabel codes lbl)
translate codes (Jz lbl)    = f : t : s : (translateLabel codes lbl)
translate codes (Jn lbl)    = f : t : t : (translateLabel codes lbl)
translate _     (Ret)       = f : t : f : []
translate _     (End)       = f : f : f : []

translate _ (Pc)            = t : f : s : s : []
translate _ (Pn)            = t : f : s : t : []
translate _ (Rc)            = t : f : t : s : []
translate _ (Rn)            = t : f : t : t : []

translateLabel :: [(String, Integer)] -> String -> String
translateLabel labelCodes label = toWhitespace $ fromJust $ lookup label labelCodes

toWhitespace :: Integer -> String
toWhitespace i = if i >= 0
                    then (map bitCode $ 0:(bits i)) ++ "\n"
                    else (map bitCode $ 1:(bits (-i))) ++ "\n"
               where bits 0 = [0]
                     bits n | odd n     = (bits $ shiftR n 1) ++ [1] 
                            | otherwise = (bits $ shiftR n 1) ++ [0]
                     bitCode 0 = ' '
                     bitCode 1 = '\t'
                     
numberLabels :: [String] -> [(String, Integer)]
numberLabels lbs = snd $ mapAccumR enumerate 0 lbs
                 where enumerate i item = (i+1, (item, i))    

getLabels :: [Instruction] -> [String]
getLabels ins = nub $ catMaybes $ map getLabel ins

getLabel :: Instruction -> Maybe String
getLabel (Label lb) = Just lb
getLabel (Call lb) = Just lb
getLabel (Jump lb) = Just lb
getLabel (Jz lb) = Just lb
getLabel (Jn lb) = Just lb
getLabel _ = Nothing