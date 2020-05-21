{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), Sex(..), PersonId, persons, processSingle, processPair)
import Text.Format


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT :: (ReaderT [Person] (StateT PersonSearchStats (Writer [String])) a) }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PersonSearchStats
    , MonadReader [Person]
    , MonadWriter [String]
    )

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons p = runWriter . flip runStateT emptyStats . flip runReaderT persons . runPersonsT $ p

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do
  ps <- ask
  let maybeId = find (\p -> id p == pId) ps
  case maybeId of
    Just (Person id _ _ _ _ _) -> tell [format "Found person! FirstId = {0}. SecondId = {1}." [show $ pId, show $ id]]
    _ -> tell [format "Not found person! FirstId = {0}." [show $ pId]]
  return $ maybeId

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
  put emptyStats
  firstPerson <- findById pId
  secondPerson <- case firstPerson of
    Just (Person _ _ _ _ _ (Just secondId)) -> findById secondId
    _ -> return $ Nothing
  case (firstPerson, secondPerson) of
      (Just husband@(Person _ _ _ _ Male _) , Just wife@(Person _ _ _ _ Female _)) -> do
        _ <- modify incMarried
        return $ Just (processPair husband wife)
      (Just wife@(Person _ _ _ _ Female _) , Just husband@(Person _ _ _ _ Male _)) -> do
        _ <- modify incMarried
        return $ Just (processPair husband wife)
      (Just person, _) -> do
        _ <- modify incSingle
        return $ Just (processSingle person)
      _ -> return $ Nothing
  where
    incSingle = \pss@(PersonSearchStats _ count) -> pss {singlePersonsCount=count+1}
    incMarried = \pss@(PersonSearchStats count _) -> pss {marriedPersonsCount=count+1}

{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}
processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  let ((results, stats), logs) = runPersons $ mapM (\person -> do
                                                      processedPerson <- processPerson person
                                                      return $ (person, processedPerson)) personIds

  mapM_ (\(i, res) -> putStrLn ("Found: " ++ show res ++ "; Id: " ++ show i)) results
  putStrLn ("Number of persons: \n" ++ show stats)
  writeFile logFileName (show logs)
  where
    logFileName = "persons.log"

-- </Задачи для самостоятельного решения>
