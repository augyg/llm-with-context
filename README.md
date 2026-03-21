# llm-with-context

Typed, multi-turn conversations with LLMs in Haskell.

Uses `StateT` to accumulate conversation history and `Proxy`/`Typeable` to
enforce response types at the call site. Supports OpenAI, Anthropic, DeepSeek
(Ollama), and a CLI backend, all behind a single provider-agnostic interface.

## Features

- **Provider-agnostic monad transformers** — `LLMT` (stateless) and `ConvoT`
  (stateful conversation with history)
- **Pluggable backends** — OpenAI, Anthropic Claude API, DeepSeek/Ollama, Claude
  CLI, or a pure mock for testing
- **Context selection strategies** — `LastN`, `Relevants` (by tag), or
  `LastNRelevant` to control which history items get injected as context
- **JSON extraction** — `askLLMJSON` scrapes JSON from noisy LLM output via
  [scrappy-json](https://github.com/TypifyDev/scrappy-json) (no aeson required
  on the response type)
- **Parsec extraction** — `askLLMParsec` and the `ReadLLM` typeclass let you
  write a Parsec parser for your response type and extract it from prose
- **Generic JSON examples** — `JsonExample` generates example JSON strings from
  Haskell types via GHC Generics, useful for few-shot prompts
- **Compile-time URI validation** — `staticURI` ensures endpoint URLs are valid
  at compile time via Template Haskell

## Quick start

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import LLM.Types
import LLM.Provider
import LLM.Provider.Backends
import Network.HTTP.Client.TLS (newTlsManager)

main :: IO ()
main = do
  mgr <- newTlsManager
  let backend = mkOpenAI (APIKey "sk-...") mgr "gpt-4o" Nothing
      env     = LLMEnv backend
  result <- runLLM env $ askLLM [cwr User "What is 2+2?"]
  print result
```

### Multi-turn conversation

```haskell
import LLM.Provider (runConvo, ConvoT)
import LLM.LLM (askWithContext)

convo :: ConvoT IO ()
convo = do
  _ <- askWithContext (LastN 10) (Tag "intro", ConvoQuestion [cwr User "My name is Alice"])
  r <- askWithContext (LastN 10) (Tag "q1",    ConvoQuestion [cwr User "What is my name?"])
  liftIO $ print r

main = do
  mgr <- newTlsManager
  let env = LLMEnv (mkOpenAI (APIKey "sk-...") mgr "gpt-4o" Nothing)
  runConvo env convo
```

### JSON extraction with scrappy-json

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Scrappy.JSON.Value (FromJValue(..))
import LLM.Provider (askLLMJSON)

data Movie = Movie { title :: String, year :: Int } deriving Generic
instance FromJValue Movie

-- askLLMJSON will scrape the first JSON object from the LLM response
-- and parse it into a Movie, even if surrounded by prose
result <- runLLM env $ askLLMJSON [cwr User "Tell me about Inception as JSON"]
-- result :: Either LLMError (Maybe Movie)
```

### Parsec extraction with ReadLLM

```haskell
import LLM.ReadLLM (ReadLLM(..))
import LLM.Provider (askLLMParsec)

-- For simple types, instances are provided: Int, Double, Bool, String, [a]
result <- runLLM env $ askLLMParsec [cwr User "How many days in a year?"]
-- result :: Either LLMError (Maybe Int)
```

### Mock backend for tests

```haskell
let mock = LLMBackend "mock" (APIMock (\_ -> pure (Right "42")))
    env  = LLMEnv mock
result <- runLLM env $ askLLM [cwr User "anything"]
-- Right "42"
```

## Modules

| Module | Purpose |
|--------|---------|
| `LLM.Types` | Core types: request/response bodies, roles, conversation history, context strategies |
| `LLM.Provider` | Provider-agnostic `LLMT`/`ConvoT` monads, `askLLM`, `askLLMJSON`, `askLLMParsec` |
| `LLM.Provider.Backends` | Backend constructors: `mkOpenAI`, `mkDeepSeek`, `mkClaudeAPI`, `mkClaudeCLI` |
| `LLM.LLM` | Legacy direct OpenAI/DeepSeek calls + provider-agnostic `askWithContext` |
| `LLM.ReadLLM` | `ReadLLM` typeclass for Parsec-based extraction from LLM output |
| `LLM.JsonExample` | Generate JSON example strings from Haskell types via Generics |
| `LLM.ScrubPrefix` | Aeson `Options` that strip record field prefixes |
| `LLM.StaticURI` | Compile-time URI validation via Template Haskell |

## Building

```bash
nix-shell --run "cabal build"
nix-shell --run "cabal test"
```

## License

MIT
