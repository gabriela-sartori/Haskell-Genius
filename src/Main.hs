import System.Random (randomRIO)
import Data.Char (toLower)

data Color = Red | Green | Yellow | Blue deriving (Show, Read, Eq)

getRandomColor :: IO Color
getRandomColor = do
    num <- randomRIO (1, 4) :: IO Int
    return $ case num of
        1 -> Red
        2 -> Green
        3 -> Yellow
        _ -> Blue

colorInput :: [(Int, Color)] -> IO Bool
colorInput [] = return True
colorInput ((index, color):xs) = do
    putStrLn $ "Color with index " ++ show index ++ ": "
    input_color <- getLine
    let success = map toLower (show color) == map toLower input_color
    if success then colorInput xs
               else return False

gameLoop :: [Color] -> Int -> IO Int
gameLoop colors points = do
    putStrLn $ if null colors then "-- Starting round --"
                              else "Points: " ++ show points
    new_color <- getRandomColor
    putStrLn $ "Last Color: " ++ show new_color
    success <- colorInput (zip [1..] (colors ++ [new_color]))
    if success then gameLoop (colors ++ [new_color]) (points + 1)
               else return points

main :: IO ()
main = do
    points <- gameLoop [] 0
    putStrLn $ "You did: " ++ show points
    putStrLn "Press anything to continue MackMobile Genius Game"
    _ <- getLine
    main
