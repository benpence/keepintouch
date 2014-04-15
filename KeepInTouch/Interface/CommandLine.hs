module KeepInTouch.Interface.CommandLine
(
  CommandLine(..)
, main
) where

import Data.List(intercalate)
import Data.Time.Clock(getCurrentTime,utctDay)
import System.Random(randoms,newStdGen)
import Text.Read(readMaybe)

import KeepInTouch.Entry(Entry(..))
import KeepInTouch.Interface(Interface(..))
import KeepInTouch.ParseFormat(Parser,plaintextParser)
import KeepInTouch.ParseFormat(Formatter,plaintextFormatter)
import KeepInTouch.Handler(scheduleHandler,contactHandler)

data CommandLine = CommandLine
    { dataFile  :: FilePath
    , parser    :: Parser
    , formatter :: Formatter
    } deriving (Eq)

instance Interface CommandLine where
    entriesIO this = do
        -- TODO: Catch problems
        contents <- readFile $ dataFile this
      
        -- TODO: Necessary to unlazify this?
        let entries = parser $ lines contents
      
        return Right entries

    newPerson this name = do
        -- TODO: i18n?
        putStrLn "How often do you want to contact '" ++ name ++ "' (days)?:"

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
            _              ->
                return $ newPerson this name

    replaceData this entries = do
        let entriesString = formatter entries
        -- TODO: Use temp file here?
        writeFile (dataFile this) entriesString 
        return Nothing

    scheduled entries = do
        let showEntriesPeople = unlines . map (intercalate ", " . names)
        putStrLn $ showEntriesPeople entries
        return Nothing

handleArgs :: (Interface a) => a -> [String] -> IO (Maybe Problem)
-- Contact
handleArgs i ("contact"  : name@(_:_))      = contactHandler i name

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
    generator <- newStdGen
    let policy = Weight {
          weight    = weight
        , generator = randoms generator
        }
    scheduleHandler i policy

handleArgs _ _ = return
    [ "contact NAME"
    , "schedule [backlog]"
    , "schedule weight"
    , "schedule weight WEIGHT"
    ]

main :: IO a
main =
  let
    interface file = CommandLine
        { dataFile  = file
        , parser    = plaintextParser
        , formatter = plaintextFormatter
        }

    getFile (file : args) = handleArgs (interface file) args
    getFile             _ = Usage
  in
    fmap getFile getArgs
