{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The provider-parameterised @LLM@ effect (dynamic dispatch, via @effectful@).
--
-- @LLM 'AnthropicHttp@ and @LLM 'OpenAIHttp@ are DISTINCT effects, so a program can use
-- both in one stack (@(LLM 'AnthropicHttp :> es, LLM 'OpenAIHttp :> es)@). Each call
-- names its provider — via a type application (@ask \@'AnthropicHttp@) or the
-- 'askAnthropic' / 'askOpenAI' aliases. Each provider's interpreter pins @p@
-- (see "LLM.Effect.Anthropic" / "LLM.Effect.OpenAI").
--
-- Context operations live in the effect ('AskWithContext' /
-- 'AskWithContextTyped'); conversation history is carried by the 'Memory'
-- effect ("LLM.Effect.Memory"), which the shared 'runCtx' / 'runCtxTyped' read
-- and write via 'recall' / 'remember'. Provider config + API key are closed
-- over by the interpreter, so they never appear in these signatures.
module LLM.Effect
  ( LLM (..)
    -- * Operations (pin the provider with a type application)
  , ask
  , askTyped
  , askWithContext
  , askWithContextTyped
    -- * Provider aliases
  , askAnthropic
  , askOpenAI
    -- * Tool-use
  , askTools
    -- * Interpreter helpers
  , runCtx
  , runCtxTyped
  ) where

import LLM.Effect.Memory (Memory, recall, remember)
import LLM.LLM (gptReturnType, readTypedAnswer)
import LLM.Types
  ( APIProvider (..)
  , ContentWithRole
  , ConversationHistory
  , ConvoAnswer (..)
  , ConvoError (..)
  , ConvoQuery (..)
  , ConvoQuestion (..)
  , RelevantContext
  , RichMessage
  , Tag
  , ToolDef
  , ToolTurn
  )

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)

-- | The LLM effect, parameterised by provider. The provider @p@ is a phantom;
-- the constructors are shared across providers and each interpreter pins @p@.
data LLM (p :: APIProvider) :: Effect where
  Ask :: [ContentWithRole] -> LLM p m (Either T.Text T.Text)
  AskTyped :: (Typeable a, Read a) => [ContentWithRole] -> LLM p m (Either ConvoError (ConvoAnswer a))
  AskWithContext :: RelevantContext -> (Tag, ConvoQuestion) -> LLM p m (Either ConvoError (ConvoAnswer T.Text))
  AskWithContextTyped :: (Typeable a, Read a) => RelevantContext -> (Tag, ConvoQuestion) -> LLM p m (Either ConvoError (ConvoAnswer a))
  AskTools :: [ToolDef] -> [RichMessage] -> LLM p m (Either T.Text ToolTurn)

type instance DispatchOf (LLM p) = Dynamic

-- | Ask provider @p@ (pin it: @ask \@'AnthropicHttp ...@).
ask :: forall p es. (LLM p :> es) => [ContentWithRole] -> Eff es (Either T.Text T.Text)
ask contents = send (Ask contents :: LLM p (Eff es) (Either T.Text T.Text))

-- | Typed ask against provider @p@ (e.g. @askTyped \@'AnthropicHttp \@Int ...@).
askTyped
  :: forall p a es. (LLM p :> es, Typeable a, Read a)
  => [ContentWithRole] -> Eff es (Either ConvoError (ConvoAnswer a))
askTyped contents = send (AskTyped contents :: LLM p (Eff es) (Either ConvoError (ConvoAnswer a)))

-- | Context-aware ask against provider @p@.
askWithContext
  :: forall p es. (LLM p :> es)
  => RelevantContext -> (Tag, ConvoQuestion) -> Eff es (Either ConvoError (ConvoAnswer T.Text))
askWithContext relCtx q =
  send (AskWithContext relCtx q :: LLM p (Eff es) (Either ConvoError (ConvoAnswer T.Text)))

-- | Typed context-aware ask against provider @p@.
askWithContextTyped
  :: forall p a es. (LLM p :> es, Typeable a, Read a)
  => RelevantContext -> (Tag, ConvoQuestion) -> Eff es (Either ConvoError (ConvoAnswer a))
askWithContextTyped relCtx q =
  send (AskWithContextTyped relCtx q :: LLM p (Eff es) (Either ConvoError (ConvoAnswer a)))

-- | @ask@ pinned to Anthropic.
askAnthropic :: (LLM 'AnthropicHttp :> es) => [ContentWithRole] -> Eff es (Either T.Text T.Text)
askAnthropic = ask @'AnthropicHttp

-- | @ask@ pinned to OpenAI.
askOpenAI :: (LLM 'OpenAIHttp :> es) => [ContentWithRole] -> Eff es (Either T.Text T.Text)
askOpenAI = ask @'OpenAIHttp

-- | Tool-enabled ask against provider @p@: advertise the tool definitions, send
-- the conversation so far, and get back the assistant's turn (text + any tool
-- calls it wants to make). The 'LLM.Effect.Agent.runAgent' combinator drives
-- this in a loop, dispatching tool calls and feeding results back.
askTools
  :: forall p es. (LLM p :> es)
  => [ToolDef] -> [RichMessage] -> Eff es (Either T.Text ToolTurn)
askTools defs msgs = send (AskTools defs msgs :: LLM p (Eff es) (Either T.Text ToolTurn))

-- | Shared context loop for interpreters: recall relevant history, inject it
-- (provider-specific role via @injectHistory@), run the provider's text prim,
-- then append the new turn. Provider-agnostic — it talks to 'Memory', not the
-- 'LLM' effect (the prim is supplied by the interpreter).
runCtx
  :: (Memory :> es)
  => (ConversationHistory -> [ContentWithRole])
  -> ([ContentWithRole] -> Eff es (Either T.Text T.Text))
  -> RelevantContext
  -> (Tag, ConvoQuestion)
  -> Eff es (Either ConvoError (ConvoAnswer T.Text))
runCtx injectHistory prim relCtx (thisTag, ConvoQuestion contents) = do
  histItems <- recall relCtx
  res <- prim (injectHistory histItems <> contents)
  case res of
    Left e -> pure (Left (ConvoError e))
    Right answer -> do
      remember (ConvoQuery thisTag (ConvoQuestion contents) (ConvoAnswer answer))
      pure (Right (ConvoAnswer answer))

-- | Typed shared context loop. Stores the raw text answer in history but
-- returns the decoded @a@.
runCtxTyped
  :: forall a es. (Memory :> es, Typeable a, Read a)
  => (ConversationHistory -> [ContentWithRole])
  -> ([ContentWithRole] -> Eff es (Either T.Text T.Text))
  -> RelevantContext
  -> (Tag, ConvoQuestion)
  -> Eff es (Either ConvoError (ConvoAnswer a))
runCtxTyped injectHistory prim relCtx (thisTag, ConvoQuestion contents) = do
  histItems <- recall relCtx
  let returnT = gptReturnType (Proxy :: Proxy a)
  res <- prim (injectHistory histItems <> contents <> returnT)
  case res of
    Left e -> pure (Left (ConvoError e))
    Right txt -> case readTypedAnswer txt of
      Left e -> pure (Left (ConvoError e))
      Right typed -> do
        remember (ConvoQuery thisTag (ConvoQuestion contents) (ConvoAnswer txt))
        pure (Right (ConvoAnswer typed))
