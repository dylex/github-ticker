import Codec.Picture (readImage)
import Codec.Picture.Types (DynamicImage(..), Image(..), PixelF, pixelMap, dropTransparency, pixelAt)
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad ((>=>), unless, void)
import Data.Char (isSpace)
import Data.Int (Int16)
import Data.List (foldl', dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Time.Calendar (Day, addDays)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Word (Word16)
import qualified System.Console.GetOpt as GetOpt
import System.Environment (getProgName, getArgs, setEnv, lookupEnv)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess, readProcess)

tickerWidth, tickerHeight :: Int
tickerWidth = 51
tickerHeight = 7

type Cell = Word16
type Scale = Int16

type Tree = String
type Commit = Maybe String

data Settings = Settings
  { settingBranch :: String
  , settingScale :: Scale
  , settingTime :: String
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingBranch = "ticker"
  , settingScale = 8
  , settingTime = "12:00"
  }

options :: [GetOpt.OptDescr (Settings -> Settings)]
options =
  [ GetOpt.Option "b" ["branch"]
      (GetOpt.ReqArg (\b s -> s { settingBranch = b }) "BRANCH")
      ("branch to create and populate with ticker [" ++ settingBranch defaultSettings ++ "]")
  , GetOpt.Option "s" ["scale"]
      (GetOpt.ReqArg (\f s -> s { settingScale = read f }) "COUNT")
      ("maximum number of commits per day (for black), negative for reverse [" ++ show (settingScale defaultSettings) ++ "]")
  ]

git :: [String] -> IO String
git args = dropWhileEnd isSpace <$> readProcess "git" args ""

ii :: (Integral i, Num a) => i -> a
ii = fromIntegral

scaleF :: RealFrac f => Scale -> f -> Cell
scaleF s f
  | s > 0 = ii s - ii s `min` truncate (ii (s+1) * f)
  | s < 0 = ii (-s) `min` truncate (ii (-s+1) * f)
  | otherwise = 0

scaleI :: (Bounded i, Integral i) => Scale -> i -> Cell
scaleI s = scaleF s . realToFrac . (% maxBound)

greyscale :: Scale -> DynamicImage -> Either String (Image Cell)
greyscale s (ImageY8 i) = Right (pixelMap (scaleI s) i)
greyscale s (ImageY16 i) = Right (pixelMap (scaleI s) i)
greyscale s (ImageYF i) = Right (pixelMap (scaleF s) i)
greyscale s (ImageYA8 i) = Right (pixelMap (scaleI s . dropTransparency) i)
greyscale s (ImageYA16 i) = Right (pixelMap (scaleI s . dropTransparency) i)
greyscale _ _ = Left "Image must be greyscale"

setEnvDef :: String -> String -> IO ()
setEnvDef var val = maybe (setEnv var val) (void . return) =<< lookupEnv var

loop :: (Monad m, Integral i) => i -> (i -> a -> m a) -> a -> m a
loop n f = l 0 where
  l i = if i >= n then return else f i >=> l (succ i)

main = do
  prog <- getProgName
  args <- getArgs
  let errout = hPutStrLn stderr
      die err = errout err >> exitFailure
  (set, img) <- case GetOpt.getOpt GetOpt.Permute options args of
    (s, [f], []) -> return (foldl' (\s t -> t s) defaultSettings s, f)
    (_, _, err) -> do
      mapM_ errout err
      die $ GetOpt.usageInfo ("Usage: " ++ prog ++ " [OPTIONS] IMAGE\n") options

  img <- either die return . (greyscale (settingScale set) =<<) =<< readImage img
  unless (imageWidth img <= tickerWidth && imageHeight img == tickerHeight) $
    die ("Image is wrong size: must be at most " ++ show tickerWidth ++ " by exactly " ++ show tickerHeight)

  first <- addDays (ii $ -365 + tickerHeight*(tickerWidth - imageWidth img)) . utctDay <$> getCurrentTime
  let start = fromWeekDate y w 7 where (y, w, _) = toWeekDate first

  setEnvDef "GIT_COMMITTER_NAME" "GitHub Ticker"
  setEnvDef "GIT_COMMITTER_EMAIL" "ticker@dylex.net"

  tree <- git ["mktree"]

  let
    render =
      loop (imageWidth img) $ \x ->
        loop (imageHeight img) $ \y ->
          cell (pixelAt img x y) (addDays (ii $ y + tickerHeight*x) start)
    cell c d = loop c (\_ -> commit d)
    commit d p = do
      setEnv "GIT_AUTHOR_DATE" (show d ++ 'T' : settingTime set)
      Just <$> git ("commit-tree" : maybe [] (("-p":) . return) p ++ [tree])

  commit <- render Nothing
  git ["update-ref", "refs/heads/" ++ settingBranch set, fromMaybe "" commit, ""]
