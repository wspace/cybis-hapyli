module FileLoader (loadProgram) where

import Ast
import Parser

loadProgram :: FilePath -> IO Program
loadProgram file = do
    modules <- loadModules [] [file] []
    return $ fromModules modules
    
loadModules :: [FilePath] -> [FilePath] -> [Module] -> IO [Module]
loadModules _ [] modulesImported = return modulesImported
loadModules filesImported (currentFile:filesToImport) modulesImported = do
    text <- readFile currentFile
    let currentModule@(Module imports _ _) = parseModule currentFile text
    let notImportedYet = filter (\x -> not $ x `elem` currentFile:filesImported) imports
    let newImports = filter (\x -> not $ x `elem` filesToImport) notImportedYet
    loadModules (currentFile:filesImported) (newImports ++ filesToImport) (currentModule:modulesImported)

