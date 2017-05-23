-- | Pre-processing ancor file for parsing.


module Odil.Ancor.Preprocess (prepare) where


import qualified Data.Text as T


-- | Prepare a given sentence for parsing.
prepare :: T.Text -> T.Text
prepare = id
