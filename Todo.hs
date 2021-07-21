import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "view"   = view
dispatch "remove" = remove
dispatch "bump"   = bump 
dispatch command  = doesntExist command 

doesntExist :: String  -> [String] -> IO ()
doesntExist command _ = 
    putStrLn $  "The " ++ command ++ " command doesn't exist"

main = do
    (command:argList) <- getArgs 
    dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName ("\n" ++ todoItem)
add _ = putStrLn "The add command takes exactly 2 arguments"

view :: [String] -> IO ()
view [fileName] = do 
    contents <- readFile fileName
    let todoTasks = lines contents 
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn "the view command take's a file name only"

remove :: [String] -> IO ()
remove [fileName, numberString] = do 
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    let number = read numberString 
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do 
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
remove _ = putStrLn "The remove command takes exactly 2 arguments"

bump :: [String] -> IO ()
bump [fileName, numberString] = do 
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                        [0..] todoTasks
    let number = read numberString 
        newTodoItems = delete (todoTasks !! number) todoTasks
        bumpItem = unlines $ todoTasks !! number : newTodoItems
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do 
            hPutStr tempHandle bumpItem
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
bump _ = putStrLn "The bump command takes exactly 2 arguments"