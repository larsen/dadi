{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Api
    (
      server
    ) where


import Check
import Servant.API
import Servant
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.UUID as UUID
import Network.Wai.Middleware.Cors




type PostCheckRoute = "check" :> "new"
  :> ReqBody '[JSON] NewCheckInput
  :> Post '[JSON] Check

type GetCheckRoute  = "check" :> Capture "id" String :> Get '[JSON] (Maybe Check)
type PutCheckRoute  = "check" :> Capture "id" String :> Put '[JSON] Check
type ListChecksRoute = "checks" :> Get '[JSON] [Check]

type DadiAPI = PostCheckRoute
          :<|> GetCheckRoute
          :<|> PutCheckRoute
          :<|> ListChecksRoute


type DB = TVar Collection


insertCheck :: DB -> Check -> STM ()
insertCheck db check = do
  modifyTVar' db $ M.insert (hash check) check

newCheck :: DB -> Server PostCheckRoute
newCheck db (NewCheckInput { .. }) =
  if trg < 0 || trg > 100 then
    throwError err500 { errBody = "target must be between 0 and 100"}
  else do
    c <- liftIO $ generateCheck desc trg
    liftIO $ atomically $ insertCheck db c
    return c

getCheckFromMap throw db hash =
  let
    succeed = \v -> liftIO $ atomically $ M.lookup v <$> readTVar db
  in
    maybe throw succeed $ UUID.fromString hash


checkNotFound = throwError err404 { errBody = "No Check found" }

getCheck :: DB -> Server GetCheckRoute
getCheck db hash =
  getCheckFromMap checkNotFound db hash


putCheck :: DB -> Server PutCheckRoute
putCheck db hash_ = do
  existing <- getCheckFromMap checkNotFound db hash_
  case existing of
    (Just check) -> do
      solution <- liftIO $ solveCheck check
      liftIO $ atomically $ modifyTVar' db $
        M.updateWithKey (\ _ _ -> Just solution) (hash check)
      return solution
    Nothing -> checkNotFound



listCheck :: DB -> Server ListChecksRoute
listCheck db = do
  m <- liftIO $ atomically $ readTVar db
  return $ fmap snd $ M.toList m


apiServer :: DB -> Server DadiAPI
apiServer db =
  newCheck db
  :<|> getCheck db
  :<|> putCheck db
  :<|> listCheck db


userAPI :: Proxy DadiAPI
userAPI = Proxy

ourCors = cors $ const (Just resourcePolicy)

resourcePolicy =
    CorsResourcePolicy
        { corsOrigins = Nothing -- gives you /*
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
        , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
        , corsExposedHeaders = Nothing
        , corsMaxAge = Nothing
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

server :: IO Application
server = do
  db <- newTVarIO M.empty
  return $ ourCors $ serve userAPI $ apiServer db
