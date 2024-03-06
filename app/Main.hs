{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

--------------------------------------------------------------------------------

import Control.Monad.Freer
import Control.Monad.Freer.Exception

--------------------------------------------------------------------------------

main :: IO ()
main = do
  result <- runM . runError . runLogger . runBlogPost . runUserModel $ runProgram
  case result of
    Left (BlogPostError err) -> print err
    Right _ -> pure ()

--------------------------------------------------------------------------------
-- Program

runProgram ::
  ( Member UserModel r,
    Member Logger r,
    Member BlogPost r
  ) =>
  Eff r ()
runProgram = do
  user <- getUser (UserId 0)
  updateUser (UserId 1) (const user)
  logger "successfully updated user!"
  pid <- submitPost (UserId 1) (Post "hello world")
  publishPost pid
  logger "successfully published blog post!"

--------------------------------------------------------------------------------
-- User Effect

getUser :: (Member UserModel r) => UserId -> Eff r User
getUser = send . GetUser

updateUser :: (Member UserModel r) => UserId -> (User -> User) -> Eff r ()
updateUser uid f = send (UpdateUser uid f)

newtype UserId = UserId Int

data User = User
  { uName :: String,
    uAge :: Int,
    uAdmin :: Bool
  }

data UserModel a where
  GetUser :: UserId -> UserModel User
  UpdateUser :: UserId -> (User -> User) -> UserModel ()

runUserModel :: (Member IO r) => Eff (UserModel ': r) v -> Eff r v
runUserModel = simpleRelay $ \case
  GetUser uid -> send $ getUserIO uid
  UpdateUser uid f -> send $ updateUserIO uid f

getUserIO :: UserId -> IO User
getUserIO _ = pure (User "Solomon" 38 False)

updateUserIO :: UserId -> (User -> User) -> IO ()
updateUserIO _ _ = pure ()

--------------------------------------------------------------------------------
-- BlogPost Effect

submitPost :: Member BlogPost r => UserId -> Post -> Eff r PostId
submitPost uid post = send (SubmitPost uid post)

publishPost :: Member BlogPost r => PostId -> Eff r ()
publishPost = send . PublishPost

unpublishPost :: Member BlogPost r => PostId -> Eff r ()
unpublishPost = send . PublishPost

newtype Post = Post String

newtype PostId = PostId Int

data BlogPost a where
  SubmitPost :: UserId -> Post -> BlogPost PostId
  PublishPost :: PostId -> BlogPost ()
  Unpublish :: PostId -> BlogPost ()

newtype BlogPostError = BlogPostError String

runBlogPost :: (Member IO r, Member (Exc BlogPostError) r) => Eff (BlogPost ': r) v -> Eff r v
runBlogPost = simpleRelay $ \case
  SubmitPost _uid _post -> pure (PostId 0)
  PublishPost _pid -> throwError (BlogPostError "Failed to publish post")
  Unpublish _pid -> pure ()

--------------------------------------------------------------------------------
-- Logger Effect

logger :: (Member Logger r) => String -> Eff r ()
logger = send . Log

data Logger a where
  Log :: String -> Logger ()

runLogger :: (Member IO r) => Eff (Logger ': r) v -> Eff r v
runLogger = simpleRelay $ \(Log msg) -> send $ print msg

--------------------------------------------------------------------------------
-- Helpers

simpleRelay :: (forall a. t a -> Eff r a) -> Eff (t ': r) v -> Eff r v
simpleRelay f = handleRelay return (\e arr -> arr =<< f e)
