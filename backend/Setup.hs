import Distribution.Simple
import Data.ProtoLens.Setup


-- | A hard-coded parameter which tells whether the setup tool should generate
-- Haskell files from the protobuffer protocol files.
buildProtos :: Bool
buildProtos = True


main :: IO ()
main =
  if buildProtos
  then defaultMainGeneratingProtos "src"
  else defaultMain
