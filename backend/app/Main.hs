module Main where


import Data.Monoid ((<>))
import Options.Applicative
import qualified Data.Text.IO as T
-- import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON

import qualified Odil.Ancor.IO.Parse as Parse
import qualified Odil.Ancor.IO.Show as Show
import qualified Odil.Server.Types as Odil
import qualified Odil.Penn as Penn


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command
    = Simplify FilePath
      -- ^ Parse and show the sentences in the Ancor XML file
    | Penn2Odil
      -- ^ Convert the Penn file on input to an JSON file


--------------------------------------------------
-- Options
--------------------------------------------------


simplifyOptions :: Parser Command
simplifyOptions = Simplify
  <$> strOption
        ( long "ancor"
       <> short 'a'
       <> metavar "FILE"
       <> help "Ancor .xml file" )


penn2odilOptions :: Parser Command
penn2odilOptions = pure Penn2Odil


opts :: Parser Command
opts = subparser
  ( command "simplify"
    (info (helper <*> simplifyOptions)
      (progDesc "Parse and show the sentences in the Ancor XML file")
    )
  <> command "penn2odil"
    (info (helper <*> penn2odilOptions)
      (progDesc "Convert Penn trees to Odil trees in JSON")
    )
  )


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =
  case cmd of
    Simplify ancorFile -> do
      file <- T.readFile ancorFile
      T.putStrLn . Show.showAncor . Parse.parseTrans $ file
    Penn2Odil -> do
      file <- Penn.convertPennFile . Penn.parseForest <$> T.getContents
      LBS.putStr (JSON.encode file)
      -- T.putStrLn . Show.showAncor . Parse.parseTrans $ file


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Working with Odil files"
      <> header "odil" )
