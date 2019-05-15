import GoM.Types
import System.Environment
import System.FilePath

main :: IO ()
main = getArgs >>= mapM_ (\pid -> readFile ("/proc" </> pid </> "stat") >>= print . parseStat)
