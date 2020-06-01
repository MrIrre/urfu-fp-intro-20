{-
  Этот модуль содержит функции для обработки API запросов.
  В MVC паттерне их можно рассматривать как контроллеры.
-}
module Handlers where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import Servant.Server

import App
import DB.MovieSession
import DB.Seat
import DB.Preliminary
import DB.Booking
import Utils

getSessions :: MonadIO m => AppT m [MovieSession]
getSessions = getMovieSessions

getSeats :: MonadIO m => MovieSessionId -> AppT m [Seat]
getSeats = getSeatsBySessionId

postPreliminary :: MonadIO m => MovieSessionId -> SeatId -> AppT m BookingId
postPreliminary msId seatId = do
  bookings <- createPreliminary msId seatId
  case bookings of
    (b:_) -> pure $ bookingId b
    _ -> throwJSONError err404 $ JSONError "booking is not found"

checkout :: MonadIO m => BookingId -> AppT m String
checkout bId = do
  bookings <- getBook bId
  case bookings of
    (b:_) ->
      tryBook b >>= \case
        Nothing       -> return $ "Booking was paid successfully"
        Just errorStr -> throwJSONError err400 $ JSONError $ T.pack errorStr
    _ -> throwJSONError err404 $ JSONError $ T.pack $ "Booking with index = " ++ show (unBookingId bId) ++ " is not found"

refund :: MonadIO m => BookingId -> AppT m String
refund bId = do
  bookings <- getBook bId
  case bookings of
    (b:_) -> do
      deleteBook $ bookingId b
      return $ "Booking was successfully canceled"
    _ -> throwJSONError err404 $ JSONError $ T.pack $ "Booking with index = " ++ show (unBookingId bId) ++ " is not found"