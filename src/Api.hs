{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Api
    (
      server
    ) where


import Check
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import Servant.API
import Servant
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as M
import qualified Data.UUID as UUID
import Network.Wai
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


data DB = DB { unDB :: Collection } deriving (Show, Typeable)
type AcidDB = AcidState DB

$(deriveSafeCopy 0 'base ''DB)
$(deriveSafeCopy 0 'base ''Check)
$(deriveSafeCopy 0 'base ''CheckState)
$(deriveSafeCopy 0 'base ''UUID.UUID)

insertCheck :: Check -> Update DB ()
insertCheck c = modify $ (DB . M.insert (hash c) c . unDB)

getCollection :: Query DB Collection
getCollection = unDB <$> ask

solveCheckAcid :: String -> Int -> Update DB (Maybe Check)
solveCheckAcid hash_ solution = do
  DB m <- get
  maybeCheck <- maybe (return Nothing) (\ k -> return $ M.lookup k m) $ UUID.fromString hash_
  flip traverse maybeCheck $ \ c -> do
    let c' = solveCheck c solution
    modify $ DB . M.updateWithKey (\ _ _ -> Just c') (hash c) . unDB
    return c'

lookupUUID :: UUID.UUID -> Query DB (Maybe Check)
lookupUUID k = ask >>= return . M.lookup k . unDB

$(makeAcidic ''DB ['insertCheck, 'getCollection, 'solveCheckAcid, 'lookupUUID])

newCheck :: AcidDB -> Server PostCheckRoute
newCheck acid (NewCheckInput { .. }) =
  if trg < 0 || trg > 100 then
    throwError err500 { errBody = "target must be between 0 and 100"}
  else do
    c <- liftIO $ generateCheck desc trg
    liftIO $ update acid $ InsertCheck c
    return c

checkNotFound :: ExceptT ServantErr IO a
checkNotFound = throwError err404 { errBody = "No Check found" }

getCheck :: AcidDB -> Server GetCheckRoute
getCheck acid h =
  maybe checkNotFound succeed $ UUID.fromString h where
    succeed = liftIO . query acid . LookupUUID


putCheck :: AcidDB -> Server PutCheckRoute
putCheck acid h = do
  solution <- liftIO pickSolution
  m <- liftIO $ update acid (SolveCheckAcid h solution)
  maybe checkNotFound return m

listCheck :: AcidDB -> Server ListChecksRoute
listCheck acid = do
  m <- liftIO $ query acid GetCollection
  return $ fmap snd $ M.toList m


apiServer :: AcidDB -> Server DadiAPI
apiServer acid =
  newCheck acid
  :<|> getCheck acid
  :<|> putCheck acid
  :<|> listCheck acid


userAPI :: Proxy DadiAPI
userAPI = Proxy

ourCors :: Middleware
ourCors = cors $ const (Just resourcePolicy)

resourcePolicy :: CorsResourcePolicy
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
  acid <- openLocalState $ DB M.empty
  return $ ourCors $ serve userAPI $ apiServer acid
