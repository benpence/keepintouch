module KeepInTouch.Interface.CommandLine
(
  CommandLine(..)
, handleInput
) where

import Data.List(intercalate)
import Data.Time.Clock(getCurrentTime,utctDay)
import System.IO.Error(IOError,ioeGetErrorString)
import System.Random(newStdGen)
import Text.Read(readMaybe)

import KeepInTouch.ParseFormat(Parser,plaintextParser)
import KeepInTouch.ParseFormat(Formatter,plaintextFormatter)
import KeepInTouch.Handler(scheduleHandler,contactHandler)
import KeepInTouch.Schedule(Backlog(..),Weight(..),defaultWeight,Shuffle(..))
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

handleInput :: [String] -> IO String
handleInput (file : rest) = fmap handleResult result
  where
    result = handleArgs (defaultCommandLine file) rest
handleInput _             = return . handleResult $ Just Usage

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
    case readMaybe w of
        Just f | 0 <= f && f <= 1 -> do
            gen <- newStdGen
            let policy = Weight {
                  weight    = f
                , generator = gen
                }
            scheduleHandler i policy
        _                         -> return $ Just Usage

-- Random Scheduler
handleArgs i ["schedule", "random"]         =
    fmap Shuffle newStdGen >>= scheduleHandler i

-- Default Scheduler
handleArgs i [] = handleArgs i ["schedule", "backlog"]

handleArgs _ _ = return $ Just Usage

handleResult :: Maybe Problem -> String
handleResult (Just (Fail e)) = ioeGetErrorString e
handleResult (Just Usage)    = format usage
  where
    format = unlines . map ("keepintouch FILE " ++)
    usage  = 
        [ "contact NAME"
        , "[schedule [backlog]]"
        , "schedule weight [WEIGHT]"
        , "schedule random"
        ]
handleResult _               = ""

defaultCommandLine :: String -> CommandLine
defaultCommandLine file = CommandLine
    { dataFile  = file
    , parser    = plaintextParser
    , formatter = plaintextFormatter
    }
