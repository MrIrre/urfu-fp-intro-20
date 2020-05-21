module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)
import Text.Format

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

singleMsg :: String
singleMsg = "Разрешите предложить Вам наши услуги."
pluralMsg :: String
pluralMsg = "Разрешите предложить вам наши услуги."

-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
  ps <- ask
  return $ find (\p -> id p == pId) ps

processSingle :: Person -> String
processSingle (Person _ _ name patronymic sex Nothing) = case sex of
  Male -> format "Уважаемый {0} {1}!\n{2}" [name, patronymic, singleMsg]
  Female -> format "Уважаемая {0} {1}!\n{2}" [name, patronymic, singleMsg]
processSingle _ = error "Not single"

processPair :: Person -> Person -> String
processPair (Person hId _ hName hPatronymic _ (Just wId')) (Person wId _ wName wPatronymic _ (Just hId'))
            | wId' == wId && hId' == hId =
  format "Уважаемые {0} {1} и {2} {3}!\n{4}" [hName, hPatronymic, wName, wPatronymic, pluralMsg]
processPair _ _ = error "Not married"

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
  firstPerson <- findById pId
  secondPerson <- case firstPerson of
    Just (Person _ _ _ _ _ (Just secondId)) -> findById secondId
    _ -> return $ Nothing
  return $ case (firstPerson, secondPerson) of
    (Just husband@(Person _ _ _ _ Male _) , Just wife@(Person _ _ _ _ Female _)) -> Just (processPair husband wife)
    (Just wife@(Person _ _ _ _ Female _) , Just husband@(Person _ _ _ _ Male _)) -> Just (processPair husband wife)
    (Just person, _) -> Just (processSingle person)
    _ -> Nothing

processPersons :: [PersonId] -> [Maybe String]
processPersons personIds = do
  pId <- personIds
  return (runReader (processPerson pId) persons)

-- </Задачи для самостоятельного решения>
