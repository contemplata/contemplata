module Main where


import Data.Monoid ((<>))
import Options.Applicative
import qualified Data.Text.IO as T

import qualified Odil.Ancor.IO.Parse as Parse
import qualified Odil.Ancor.IO.Show as Show


--------------------------------------------------
-- Commands
--------------------------------------------------


data Command
    = Simplify FilePath
    -- ^ Parse and show the sentences in the Ancor XML file


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


opts :: Parser Command
opts = subparser
  ( command "simplify"
    (info (helper <*> simplifyOptions)
      (progDesc "Parse and show the sentences in the Ancor XML file")
    )
  )


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd =
  case cmd of
    Simplify ancorFile -> do
      file <- T.readFile ancorFile
      T.putStrLn . Show.showAncor . Parse.parseTrans $ file


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Working with Ancor files"
      <> header "ancor" )
