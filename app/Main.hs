module Main where

import Lib
import System.Random.Mersenne.Pure64 (newPureMT, randomWord64, randomDouble)
import Data.Word (Word64)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as Vec
import Data.Time (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
    putStrLn "This is RLmaze."
    print $ "environment is " ++ (show $ length field - 2) ++ " by " ++ (show $ length (field Vec.! 0) - 2) ++ " field."
    gen <- newPureMT
    let (r0, gen') = randomWord64 gen
    let (r1, _) = randomWord64 gen'
    let q = init_Qs (get_actions' field (1,1)) r0
    let qs = Map.fromList [((1,1),q)]
    let learned_Qs = episodes' 0 field qs r1
    trace <- learned_actions field (1,1) learned_Qs []
    print $ length trace
    let field_trace = show_field trace $ Vec.map (Vec.map show) field
    putStrLn "The field is following."
    print_field $ show_field [] $ Vec.map (Vec.map show) field
    putStrLn "Trace of actions after learning of 1000 episodes :"
    print_field field_trace


-- 以下の5行は学習成功率測定用（ついでに時間も計る）
-- 学習が成功したかどうかは、StartからGoalまでの通ったマスの数で判断する
-- （このフィールドならS、Gを含めて33）
--    start <- getCurrentTime
--    rate <- success_rate 100 33
--    end <- getCurrentTime
--    print $ "Success rate of learning is " ++ show (rate * 100) ++ "%."
--    print $ "Processing time is " ++ show (diffUTCTime end start) ++ " sec."

field :: Field Cell               -- 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13
field = Vec.fromList [Vec.fromList [ W, W, W, W, W, W, W, W, W, W, W, W, W, W ],--0
                      Vec.fromList [ W, S, R, R, R, R, R, R, R, R, R, R, R, W ],--1
                      Vec.fromList [ W, R, W, R, W, W, W, W, W, W, W, W, R, W ],--2
                      Vec.fromList [ W, R, W, R, R, W, W, W, W, W, W, W, R, W ],--3
                      Vec.fromList [ W, R, W, W, R, W, R, R, R, R, R, W, R, W ],--4
                      Vec.fromList [ W, R, W, W, R, W, R, W, W, W, R, W, R, W ],--5
                      Vec.fromList [ W, R, W, R, R, W, R, W, R, W, R, W, R, W ],--6
                      Vec.fromList [ W, R, W, R, W, W, R, W, R, W, R, W, W, W ],--7
                      Vec.fromList [ W, R, W, R, R, R, R, W, R, W, R, R, R, W ],--8
                      Vec.fromList [ W, R, W, W, W, W, W, W, R, W, W, W, R, W ],--9
                      Vec.fromList [ W, R, R, R, R, R, R, R, R, R, R, W, R, W ],--10
                      Vec.fromList [ W, R, W, W, W, W, W, W, W, W, R, W, R, W ],--11
                      Vec.fromList [ W, R, R, R, R, R, R, R, R, W, R, W, G, W ],--12
                      Vec.fromList [ W, W, W, W, W, W, W, W, W, W, W, W, W, W ]]--13

print_field :: Field [Char] -> IO ()
print_field field = do
    let loop_i i | i < length field = do
            let row = Vec.foldr (++) [] $ Vec.map (++ "\\t") (field Vec.! i)
            let format = '"' : row ++ "\""
            putStrLn (read format :: String)
            putStrLn ""
            loop_i $ i + 1
        loop_i _ = return ()
    loop_i 0

learned_actions :: Field Cell
                   -> (Int, Int)
                   -> VValues (Int, Int) (QValues Direction Double)
                   -> [(Int, Int)]
                   -> IO [(Int, Int)]
learned_actions field current_position qs trace = do
    gen <- newPureMT
    let (rand, _) = randomDouble gen
    let action = Map.keys (qs Map.! current_position) !! (take_action 1000 (Map.elems $ qs Map.! current_position) rand)
    let new_position = move current_position action
    let trace' = trace ++ [current_position]
    if field Vec.! (fst new_position) Vec.! (snd new_position) == G
        then do
            return (trace' ++ [new_position])
        else learned_actions field new_position qs trace'

learn_1000 :: IO Int
learn_1000 = do
    gen <- newPureMT
    let (r0, gen') = randomWord64 gen
    let (r1, _) = randomWord64 gen'
    let q = init_Qs (get_actions' field (1,1)) r0
    let qs = Map.fromList [((1,1),q)]
    let learned_Qs = episodes' 0 field qs r1
    trace <- learned_actions field (1,1) learned_Qs []
    --print trace
    return $ length trace

success_rate :: (Fractional r, Ord r) => r -> Int -> IO r
success_rate n success = do
    let 
        loop i sum_i | i < n = do
            num <- learn_1000
            if num == success
                then do
                    loop (i + 1) (sum_i + 1)
                else do
                    loop (i + 1) sum_i
        loop _ sum_i = return $ sum_i / n
    loop 0 0
-- n回episodesを実行して、学習成功率を返す。