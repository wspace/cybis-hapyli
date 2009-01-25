module FileLoader (loadModule, loadAssembly) where

import Ast
import Parser

loadModule :: FilePath -> IO Module
loadModule file = do
    modules <- loadModules [] [file] []
    return $ combine modules

loadAssembly :: FilePath -> IO [Instruction]
loadAssembly file = do
    text <- readFile file
    let program = parseAssembly file text
    return program    
    
loadModules :: [FilePath] -> [FilePath] -> [Module] -> IO [Module]
loadModules _ [] modulesImported = return modulesImported
loadModules filesImported (currentFile:filesToImport) modulesImported = do
    text <- readFile currentFile
    let currentModule@(Module imports _ _ _) = parseModule currentFile text
    let notImportedYet = filter (\x -> not $ x `elem` currentFile:filesImported) imports
    let newImports = filter (\x -> not $ x `elem` filesToImport) notImportedYet
    loadModules (currentFile:filesImported) (newImports ++ filesToImport) (currentModule:modulesImported)
            
combine :: [Module] -> Module
combine modules = recurse modules [] [] []
            where recurse :: [Module] ->
                             [Variable] ->
                             [AssemblyMacro] ->
                             [Function] ->
                             Module
                  recurse                   []  vs as fs = Module [] vs as fs
                  recurse ((Module _ v a f):ms) vs as fs = recurse ms (v++vs) (a++as) (f++fs)

