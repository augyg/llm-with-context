# llm-with-context Test Plan

## 1. Type Construction Validity

Verify that construction of types in this package are valid — round-trip
JSON encoding/decoding, Eq instances, smart constructors, etc.

- `GPTRole`, `ContentWithRole`, `GPTRequestBody`, `AnthropicContent`,
  `AnthropicResponse`, `DeepSeekModel`, `ResponseFormat`
- `APIKey`, `APIProvider`, `ConvoQuery`, `ConvoAnswer`, `ConvoQuestion`,
  `ConvoError`
- `LLMBackend`, `LLMAPI` (APIWeb, APICLI, APIMock)
- `RelevantContext` (LastN, Relevants, LastNRelevant)

## 2. Generic-Based Prompt Generation (JsonExample)

Verify that prompts generated via the `JsonExample` class (using GHC
Generics to derive type information) produce output that can be parsed
back into the expected Haskell type.

- `genericJsonExample` produces valid JSON for arbitrary types
- `jsonResponsePrompt` creates a prompt that, when fed to an LLM (or
  mock), yields parseable JSON
- `stripFieldPrefix` correctly removes record field prefixes
- Round-trip: type → JsonExample prompt → JSON string → FromJSON → value

## 3. Scrappy / scrappy-json Response Extraction

Verify that `scrappy` and `scrappy-json` (`FromJValue`, `search`) can
locate and extract the response type from noisy LLM output (markdown
fences, prose surrounding JSON, etc.).

- `search` finds valid JSON embedded in surrounding text
- `FromJValue` instances correctly parse extracted JSON
- Code-fenced responses (`\`\`\`json ... \`\`\``) are handled
- Partial / malformed JSON is rejected cleanly

## 4. Context-Informed LLM Responses

Verify that past Q+A (conversation history) effectively informs the LLM
generating the response value — i.e. grabbing relevant information and
general past info.

- `ConvoT` accumulates conversation history across multiple exchanges
- `getRelevantCtx` with `LastN` returns the N most recent exchanges
- `getRelevantCtx` with `Relevants` filters to keyword-relevant history
- `getRelevantCtx` with `LastNRelevant` combines both strategies
- Context is correctly serialized into the prompt sent to the backend
- Multi-turn conversations produce responses informed by prior context
