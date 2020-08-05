module Server.ChatBot where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect.Aff (Aff)

type ChatBotStore m id st
  = { storeState :: id -> st -> m (Either String Unit)
    , retrieveState :: id -> m (Either String st)
    }

type ChatBot m id st outbound
  = { handleMessage :: id -> st -> String -> m (Either String (Tuple st (Array outbound)))
    -- , handleDeliveryStatus :: state -> deliveryStatus -> Tuple state (Array msg)
    -- some function for tick - i.e. generating messages that are not sent to music
    }
