module Console where

-- import Prelude
-- import Effect (Effect)
-- import Data.Unit (Unit)

-- | Write a message to the console.
foreign import log
  :: String
  -> Effect Unit
