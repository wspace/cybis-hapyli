module FileLoader (loadProgram, loadAssembly) where

import Ast
import Parser

loadProgram :: FilePath -> IO Program
loadProgram file = do
    modules <- loadModules [] [file] []
    return $ fromModules modules

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

