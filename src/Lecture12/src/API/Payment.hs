module API.Payment where

import Servant.API
import DB.Booking (BookingId)

type PaymentAPI
  = "api" :> "checkout" :> Capture "id" BookingId :> Get '[JSON] String
  :<|> ("api" :> "refund" :> Capture "id" BookingId :> Get '[JSON] String)
