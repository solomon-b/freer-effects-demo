{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

--------------------------------------------------------------------------------

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Data.Variant

--------------------------------------------------------------------------------

main :: IO ()
main = do
  result <- interpreter runProgram
  case result of
    Left err ->
      case toEithers err of
        Left e -> print e
        Right e -> print e
    Right _ -> pure ()

type Errors = '[BlogPostError, UserModelError]

interpreter :: Eff '[UserModel, BlogPost, Logger, Exc BlogPostError, Exc UserModelError, Exc (Variant Errors), IO] a -> IO (Either (Variant Errors) a)
interpreter =
    runM
  . runError
  . injectError @UserModelError @Errors
  . injectError @BlogPostError @Errors
  . runLogger
  . runBlogPost
  . runUserModel
  
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

newtype UserModelError = UserModelError String
  deriving Show

runUserModel :: (Member IO r, Member (Exc UserModelError) r) => Eff (UserModel ': r) v -> Eff r v
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
  deriving Show

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

mapError :: forall e1 e2 r a. Member (Exc e2) r => (e1 -> e2) -> Eff (Exc e1 ': r) a -> Eff r a 
mapError f = simpleRelay $ \(Exc err) -> throwError (f err)

injectError :: forall e es r a . (Member (Exc (Variant es)) r, CouldBe es e) => Eff (Exc e : r) a -> Eff r a
injectError = mapError (throw :: e -> Variant es)

-- data Variant xs where
--   Here  :: x -> Variant (x : xs)
--   There :: Variant xs -> Variant (x : xs)

-- class CouldBe xs x  where
--   inject  :: x -> Variant xs
--   project :: Variant xs -> Maybe x

-- throw :: CouldBe xs x => x -> Variant xs
-- throw = inject

-- instance {-# OVERLAPS #-} CouldBe (x : xs) x where
--   inject :: x -> Variant (x : xs)
--   inject = Here

--   project :: Variant (x : xs) -> Maybe x
--   project = \case
--     Here x -> Just x
--     There _variant -> Nothing

-- instance {-# OVERLAPPABLE #-} CouldBe xs y => CouldBe (x : xs) y where
--   inject :: CouldBe xs y => y -> Variant (x : xs)
--   inject y = There (inject y)

--   project :: CouldBe xs y => Variant (x : xs) -> Maybe y
--   project = \case
--     Here _ -> Nothing
--     There variant -> project variant
