{-# LANGUAGE DeriveAnyClass #-}

module DB.Booking where

import Data.Aeson
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Servant.API
import GHC.Generics

import DB.MovieSession
import DB.Seat (SeatId)
import DB.Internal

{-
  Тип для идентификатора бронирования
-}
newtype BookingId = BookingId
  { unBookingId :: Integer }
  deriving (Eq, Show)
  deriving ToRow via (Only Integer)
  -- ^ этот инстанс позволяет использовать `BookingId` с функциями для базы данных
  -- `via` говорит о том, что `BookingId` нужно использовать как `Integer`.
  -- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html
  deriving (FromHttpApiData, FromField, ToField, FromJSON, ToJSON)
    via Integer
  -- ^ тоже самое для других классов

{-
  Record, описывающий таблицу `bookings`
-}
data Booking = Booking
  { bookingId :: BookingId
  , seatId :: SeatId
  , movieSessionId :: MovieSessionId
  , isPreliminary :: Bool
  , createdAt :: UTCTime
  } deriving (Eq, Show, Generic)
-- Класс Generic отвечает за универсальное кодирование типа, т.е. за  такое представление,
-- в котором используются конструкторы типов из ограниченного набора
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Generics.html
-- Это представление используется при выводе instance'ов других классов

deriving instance FromRow Booking
deriving instance ToRow Booking
-- ^ получаем возможность записывать и читать данные из базы данных с помощью `Booking`

instance ToJSON Booking
instance FromJSON Booking
-- ^ возможность для работы с JSON

expirationTime :: NominalDiffTime
expirationTime = fromInteger 600 -- в секундах

{-
  Booking запрос должен проверить наличие предварительного бронирования.
  Если оно существует и прошло меньше 10 минут от создания, то бронирование
  проходит успешно, иначе необходимо вернуть сообщение об ошибке в JSON формате.
-}
tryBook
  :: DBMonad m
  => Booking
  -> m (Maybe String)
tryBook booking = runSQL $ \conn -> do
  if not (isPreliminary booking)
    then return $ Just "Booking has already been paid"
  else do
    curTime <- getCurrentTime
    if (diffUTCTime curTime (createdAt booking)) < expirationTime
      then do
        execute conn "UPDATE bookings SET is_preliminary = false WHERE id = ?" (bookingId booking)
        execute conn "UPDATE seats SET available = false WHERE id = ?" (seatId booking)
        return Nothing
      else do
        execute conn "DELETE FROM bookings WHERE id = ?" (bookingId booking)
        return $ Just "Booking has been deleted since it has expired"

getBook
  :: DBMonad m
  => BookingId
  -> m [Booking]
getBook bookingId = runSQL $ \conn ->
  query conn "SELECT * from bookings where id = ? " bookingId

deleteBook
  :: DBMonad m
  => BookingId
  -> m ()
deleteBook bookingId = runSQL $ \conn ->
  execute conn "DELETE FROM bookings WHERE id = ?" (bookingId)