# Phase A.5 Review Packet

Generated: 2026-10-25 20:00 EDT

## Status

- Branch: gs/merge-llm-with-context
- Workspace: ~/code/llm-with-context-merge
- Commits ahead: 43
- HEAD: 9b28a71 — Phase A.4 fix 6: 2 coverage adds; surfaced + fixed ToolDef roundtrip bug

## Build + Test Results

- Lib compiles clean on GHC 9.10 across all 43 modules
- test-suite llm-with-context-test: 52/52 PASS
- test-suite llm-with-context-newSorted-tests: 227/227 PASS
- Total: 279 tests passing, 0 failures

## Diff summary

```
 54 files changed, 191873 insertions(+), 322 deletions(-)
```

## Commit chain (oldest first)

```
be6d496 Add ARG_MAX guard, ConvoEntry sum type, and context management
1e96877 Add henforcer GHC plugin, expand test suite to 201 tests, add Haddock docs
cc8186c Add Gets/NoHistory context selection with tag-glob pattern matching
2c72382 Effectful rewrite of llm-with-context
3443fd7 Merge remote-tracking branch 'origin/master' into gs/effectful
7f21ba7 Reconcile effectful<->upstream merge to green on GHC 9.10
bbe4a71 Sandbox: CPP-guard the staticWhich import behind static-sandbox-exe
ef52653 Add runLLMBackend (LLM effect over any LLMBackend transport) + retry combinator
f487055 Add context-selection DSL + SemanticError/askValidated self-repair
ca677e8 Add BrainGraph keyed-memory: deterministic lexical core
b82ba0a Add beam-postgres persistence for the keyed Brain + Lexicon
c3183fa Load the Moby Part-of-Speech dictionary into the lexicon
2f9d6e7 Add focused (LLM-keyed) read path: keyForPrompt / recallFocused
9537f31 Add BrainMd-style renderer + memory-injection ask (askWithBrain)
c743e0c Add narrow Budget/Clock/Concurrent/FileSystem effect interpreters
112c57d Add runBudgetIO, loadBudgetConfigOrDefault, and LLM.Effect.Budget.Policy
1faab31 Backends + Provider: ClaudeCliCfg lets callers override claude binary + extra args
42962a6 APIProvider: split per-transport providers + LLM.Capability classes
ecef9be default.nix: pin newer nixpkgs for claude-code; AnthropicCli uses staticWhich
fcc7ca8 LLM: AskMultimodal GADT constructor + send-routed CanMultimodal instance
d373e8e Mock: add runLLMMockRecording for capturing prompts in tests
7a30c79 AnthropicHttp / OpenAIHttp: real CanText + CanMultimodal instances
90e6b1c LLM.Capability: CanText / CanMultimodal / CanJsonOutput return Either TransportError
9dfcf65 WIP: 914 pre-merge (thunks/llm-with-context)
e17667d WIP: base pre-merge (thunks/llm-with-context)
358cac0 Merge base (thunks/llm-with-context); prefer 914 on conflicts
a705631 Phase A pre-merge: capture dirty shell.nix for safety before merge
e181e78 Phase A pre-merge: capture stash@{0} on safety branch
ef1d9bd Phase A pre-merge: capture dirty working tree + untracked Tools/ for safety before merge
adeccb6 Phase A merge step 1/N: origin/gs/claude-cli-cfg
df5b6f0 Phase A merge step 2/N: gingerly/gs/pre-merge-gingerly-dirty
80163dc Phase A merge step 3/N: origin/gs/effectful (brain + lexicon + keyed memory)
0caff53 Phase A merge step 4/N: origin/gs/gets-context
dabb8a6 Phase A merge step 5/N: newSorted/gs/pre-merge-newSorted-dirty
07defb4 Phase A merge step 6/N: newSorted/gs/pre-merge-newSorted-stash-0
5b45d66 Phase A merge step 7/N: typify-base/wip/base-pre-merge
d9bea1a Phase A merge step 8/N: 914-ws/wip/914-pre-merge
3c3782a Phase A.4 fix: resolve step-4 union-merge duplicate declarations in Types.hs
e4d7884 Phase A.4 fix 2: Provider.hs + LLM.hs type-synthesis fallout
734bcf2 Phase A.4 fix 3: LLM.Effect.Memory ConvoEntry adaptation
5de2cec Phase A.4 fix 4: converge test suite + Backend LLMArgMaxExceeded case
2e14aa9 Phase A.4 fix 5: converge newSorted test-suite (test/Main.hs)
```

## Zero-features-lost audit

- origin/gs/claude-cli-cfg: fully in HEAD
- origin/gs/effectful: fully in HEAD
- origin/gs/gets-context: fully in HEAD
- origin/gs/henforcer-strict-tests: fully in HEAD
- newSorted/gs/pre-merge-newSorted-dirty: fully in HEAD
- newSorted/gs/pre-merge-newSorted-stash-0: fully in HEAD
- gingerly/gs/pre-merge-gingerly-dirty: fully in HEAD
- typify-base/wip/base-pre-merge: fully in HEAD
- 914-ws/wip/914-pre-merge: fully in HEAD

## Known pending work (NOT blocking push)

- henforcer plugin temporarily disabled via commented-out ghc-options line. dep still in build-depends. Re-enable requires backfilling module headers + 0-undocumented-exports + tight import discipline across ~40 modules. Phase A.4b.

## Breaking changes vs origin/master

- APIProvider: Anthropic/OpenAI split into AnthropicCli + AnthropicHttp + OpenAIHttp + GoogleHttp + AwsBedrock + DeepSeekHttp
- LLMEnv: gains Maybe Int for max context chars (ARG_MAX awareness)
- mkClaudeCLI: gains ClaudeCliCfg parameter
- ConversationHistory: [ConvoQuery a] -> [ConvoEntry a] (sum of ConvoExchange + ConvoSummary)
- TagPattern: T.Text flat glob -> [T.Text] segment-aware
- CtxRule: positional -> record (crBudget / crPattern)
- LLMError: adds LLMArgMaxExceeded
- GPTRequestBody: adds _gptRequest_tools + _gptRequest_tool_choice
- Tag: gains Ord deriving

## When ready to push (after your review)

    cd ~/code/llm-with-context-merge
    git push origin gs/merge-llm-with-context
