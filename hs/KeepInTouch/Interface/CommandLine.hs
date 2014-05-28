module KeepInTouch.Interface.CommandLine
(
  CommandLine(..)
, main
) where

import Data.List(intercalate)
import Data.Time.Clock(getCurrentTime,utctDay)
import System.Environment(getArgs)
import System.IO.Error(IOError,ioeGetErrorString)
import System.Random(randoms,newStdGen)
import Text.Read(readMaybe)

import KeepInTouch.ParseFormat(Parser,plaintextParser)
import KeepInTouch.ParseFormat(Formatter,plaintextFormatter)
import KeepInTouch.Handler(scheduleHandler,contactHandler)
import KeepInTouch.Schedule(Backlog(..),Weight(..),defaultWeight)
import KeepInTouch.Type(Entry(..),Interface(..),Problem(..))
import KeepInTouch.Util(todayIO)

data CommandLine = CommandLine
    { dataFile  :: FilePath
    , parser    :: Parser
    , formatter :: Formatter
    }

instance Interface CommandLine where
    entriesIO this = do
        -- TODO: Catch problems
        contents <- readFile $ dataFile this
      
        -- TODO: Necessary to unlazify this?
        let entries = parser this $ contents
      
        return entries

    newPerson this name = do
        -- TODO: i18n?
        putStrLn $ "How often do you want to contact '" ++ name ++ "' (days)?:"

        -- TODO: Detect empty line and EOF
        intervalString <- getLine
        case readMaybe intervalString of
            -- Valid interval
            Just i | 0 < i -> do
                now <- getCurrentTime

                return $ Just $ Entry
                  { interval      = i
                  , lastContacted = utctDay now
                  , names         = [name]
                  }

            -- Ask again
            _ -> newPerson this name

    replaceData this entries = do
        let entriesString = formatter this $ entries
        -- TODO: Use temp file here?
        writeFile (dataFile this) entriesString 
        return Nothing

    scheduled _ entries = do
        let showEntriesPeople = intercalate "\n" . map (intercalate ", " . names)
        putStrLn $ showEntriesPeople entries
        return Nothing

handleArgs :: (Interface a) => a -> [String] -> IO (Maybe Problem)
-- Contact
handleArgs i ("contact"  : pieces@(_:_))      =
    contactHandler i $ unwords pieces 

-- Backlog Scheduler
handleArgs i ["schedule"]                   =
    handleArgs i ["schedule", "backlog"]
handleArgs i ["schedule", "backlog"]        = do
    today <- todayIO
    let policy = Backlog { today = today }
    scheduleHandler i policy

-- Weight Scheduler
handleArgs i ["schedule", "weight"]         =
    handleArgs i ["schedule", "weight", show defaultWeight]
handleArgs i ["schedule", "weight", w]      =
  let
    weight = case readMaybe w of
        Just f | 0 <= f && f <= 1 -> f
        _                         -> defaultWeight
  in do
    gen <- newStdGen
    let policy = Weight {
          weight    = weight
        , generator = gen
        }
    scheduleHandler i policy

handleArgs _ _ = return $ Just Usage

handleResult :: Maybe Problem -> String
handleResult (Just (Fail e)) = ioeGetErrorString e
handleResult (Just Usage)    = format usage
  where
    format = unlines . map ("keepintouch FILE " ++)
    usage  = 
        [ "contact NAME"
        , "schedule [backlog]"
        , "schedule weight"
        , "schedule weight WEIGHT"
        ]
handleResult _               = ""

getFile :: [String] -> (CommandLine, [String])
getFile (file : rest) = (defaultCommandLine { dataFile = file }, rest)
getFile _             = (defaultCommandLine,                     [])

defaultCommandLine = CommandLine
    { dataFile  = "~/.keepintouch.data"
    , parser    = plaintextParser
    , formatter = plaintextFormatter
    }

main :: IO ()
main = do
    params <- getArgs
    let (interface, args) = getFile params
    result <- handleArgs interface args
    putStr $ handleResult result
    return ()
