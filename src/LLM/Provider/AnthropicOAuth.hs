-- | Description: Subscription-OAuth Anthropic backend (Claude Code
-- credentials flow). Umbrella re-export module.
-- Copyright: (c) lazyLambda, 2026
-- License: MIT
-- Maintainer: galen.sprout@gmail.com
--
-- This module re-exports the three submodules that make up the
-- Phase B.2 subscription-OAuth backend:
--
--   * "LLM.Provider.AnthropicOAuth.Credentials" — read / parse /
--     atomically-write the @~/.claude/.credentials.json@ file.
--   * "LLM.Provider.AnthropicOAuth.Refresh" — exchange a
--     @refresh_token@ for a fresh @access_token@ against the
--     Anthropic OAuth2 token endpoint.
--   * "LLM.Provider.AnthropicOAuth.Ask" — POST to @\/v1\/messages@
--     with Bearer auth, refresh-on-401, atomic file rewrite on
--     success.
--
-- Consumers (harnesses, downstream libraries) should import this
-- module rather than the three submodules directly, so the exact
-- surface stays coupled when it's refactored.
--
-- Important: this backend is OFFICIALLY UNSUPPORTED by Anthropic for
-- third-party use. The access token's scope list includes
-- @user:sessions:claude_code@, meaning Anthropic's intended consumer
-- is Claude Code itself — using it from a different client is
-- tolerated but not blessed, and traffic shape differing significantly
-- from Claude Code's may trigger anomaly-detection and account
-- revocation. The default @User-Agent@ string (@claude-cli/\.\.\.@)
-- deliberately mimics Claude Code's prefix to minimise this risk.
--
-- See the parent plan at
-- @~/.claude/plans/hello-so-i-think-linked-squirrel.md@ for the
-- cost rationale (parity with the $230/mo Max subscription vs.
-- the $95,000/mo direct-API price at Galen's measured volume).
module LLM.Provider.AnthropicOAuth
  ( -- * Credentials
    module LLM.Provider.AnthropicOAuth.Credentials
    -- * OAuth refresh
  , module LLM.Provider.AnthropicOAuth.Refresh
    -- * Ask
  , module LLM.Provider.AnthropicOAuth.Ask
  ) where

import LLM.Provider.AnthropicOAuth.Credentials
import LLM.Provider.AnthropicOAuth.Refresh
import LLM.Provider.AnthropicOAuth.Ask
