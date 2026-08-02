# The Interaction-Tree Agent Core

Cosmobot does not encode its agent as one hard-wired
`model -> tools -> model` loop. Instead, it represents the rest of an agent
run as an observable, composable `Program`, then gives that program to a
`Runtime` for interpretation.

This separation lets tool limits, continuations, steering, observation, and
context compaction remain ordinary middleware. They can change how a program
is interpreted without becoming special cases in the core recursion.

## 1. The core types

### 1.1 `Program`

```haskell
newtype Program m result = Program
  { observe :: Stream (Of Output) m (Step m result)
  }
```

`Program m result` describes the remainder of an agent computation.

| Parameter | Meaning |
|---|---|
| `m` | The carrier used while observing the program |
| `result` | The value returned when the whole computation finishes |

Calling `observe` exposes one layer of the computation. That observation may
first yield zero or more `Output` values, then returns one `Step`.

The recursive part is stored inside `Step`, rather than constructed eagerly.
Consequently a `Program` can describe an arbitrary number of model/tool turns,
or even a non-terminating computation, without first building the whole tree.

### 1.2 `Step` and `AgentEvent`

```haskell
data Step m result where
  Finished
    :: result
    -> Step m result

  Continues
    :: Program m result
    -> Step m result

  Visible
    :: AgentEvent response
    -> (response -> Program m result)
    -> Step m result

data AgentEvent response where
  RunModel
    :: TurnState
    -> AgentEvent (TurnState, LLM.ChatAnswer)

  RunTools
    :: ToolRequest
    -> AgentEvent TurnState
```

Each constructor describes one possible structural observation:

| Constructor | Meaning |
|---|---|
| `Finished result` | The computation has returned `result` |
| `Continues next` | Silently continue with `next` |
| `Visible event continue` | Ask the interpreter to handle `event`, then pass its typed response to `continue` |

`AgentEvent` is a GADT: each event constructor determines its own response
type.

- `RunModel state` returns the effective `TurnState` together with the model's
  `ChatAnswer`. Model middleware may rewrite the state before making the
  request, so the continuation must receive the state that was actually used.
- `RunTools request` returns the `TurnState` produced after executing and
  recording all requested tools.

The continuation is existentially typed with the event response:

```haskell
response -> Program m result
```

The event handler does not need to know which model function to call next. It
only produces the response promised by the event; the continuation contains
the remainder of the computation.

`trigger` lifts one event into a program:

```haskell
trigger :: Monad m => AgentEvent response -> Program m response
```

For example, the model step is ordinary monadic composition:

```haskell
trigger (RunModel state) >>= \(modelState, answer) ->
  modelDecision runtime continue modelState answer
```

This is also why continuation middleware can save actual control flow. It
stores a resumption function, not a symbolic instruction saying where the
loop should restart.

### 1.3 `TurnState`

```haskell
data TurnState = TurnState
  { transcript :: !Transcript
  , nextModelTranscript :: !(Maybe Transcript)
  , turn :: !Int
  , modelTokenUsage :: !(Maybe LLM.TokenUsage)
  }
```

`TurnState` contains state that must survive from one model/tool turn to the
next:

| Field | Meaning |
|---|---|
| `transcript` | The canonical conversation |
| `nextModelTranscript` | An optional one-shot view for the next model turn |
| `turn` | The current model/tool turn number |
| `modelTokenUsage` | Token usage from the most recent model request |

It is deliberately not a general middleware state bag. Private middleware
state belongs in lexical closures. Temporary values passed between
middleware belong in the typed middleware context described later.

### 1.4 `ToolRequest`

```haskell
data ToolRequest = ToolRequest
  { agentState :: !TurnState
  , answered :: !Transcript
  , toolContent :: !Text
  , toolCalls :: !(NonEmpty LLM.ToolCall)
  }
```

`ToolRequest` is the payload of a `RunTools` event.

`answered` already includes the assistant message containing the tool calls.
The tool phase must append one result for every call before the transcript is
sent to the model again. Keeping that payload explicit makes this protocol
invariant available to tool middleware without putting tool execution in the
core calculus.

### 1.5 `Output` and `Result`

```haskell
data Output
  = ContentDelta !Text
  | ToolCallNotification !(NonEmpty LLM.ToolCall)
  | ReplyBoundary
```

`Output` represents observations made while the program is still running:

- `ContentDelta` carries incremental assistant text.
- `ToolCallNotification` announces that the model has requested tools.
- `ReplyBoundary` tells the consumer to finish the current visible reply
  segment.

The final value travels through the return channel of the stream:

```haskell
data Result = Result
  { runId :: !Text
  , transcript :: !Transcript
  , status :: !Text
  , finalText :: !Text
  , turnsUsed :: !Int
  , tokenUsage :: !(Maybe LLM.TokenUsage)
  }
```

A caller can therefore process `Output` values immediately and still receive
the complete `Result` when the stream ends.

## 2. Composing programs

`Program` has structural `Functor`, `Applicative`, and `Monad` instances. The
`Monad` instance defines what it means to continue after a program returns:

```haskell
Program action >>= next =
  Program do
    action >>= \case
      Finished result ->
        let Program nextAction = next result
        in nextAction
      Continues program ->
        pure (Continues (program >>= next))
      Visible event continue ->
        pure (Visible event ((>>= next) . continue))
```

Read one constructor at a time:

- `Finished result` proceeds directly to `next result`.
- `Continues` keeps the silent step and appends `next` to its successor.
- `Visible` keeps the event and appends `next` to its typed continuation.

Ordinary monadic composition therefore works across returns, silent steps,
and tool interactions. Code composing agent computations does not have to
repeat the model/tool recursion.

## 3. `Runtime`: the interpretation environment

The complete definition contains run metadata, tools, and a set of
middleware hooks:

```haskell
data Runtime (context :: [Type]) m = Runtime
  { runId :: !Text
  , toolCallMetadata :: !ToolCallMetadata
  , context :: Context
  , tools :: [Tool m]
  , exposedTools :: [Tool m]
  , runningTools :: [RunningTool m]
  , maxTurns :: !Int
  , modelInputTranscript
      :: HList context -> TurnState -> m Transcript
  , aroundProgram
      :: Runtime '[] m -> Program m Result -> Program m Result
  , aroundAgentRun
      :: HList context
      -> Stream (Of Output) m Result
      -> Stream (Of Output) m Result
  , aroundModelTurn :: ...
  , aroundToolTurn :: ...
  , aroundToolCall :: ...
  }
```

The two type parameters have separate roles:

| Parameter | Meaning |
|---|---|
| `context` | The typed middleware context still required by the runtime |
| `m` | The carrier shared by the runtime, program, and tools |

The type-level `context` must not be confused with the value-level `Context`
field. The value-level `Context` contains request information such as the
incoming message, permissions, and system context. The type-level context is
an HList through which outer middleware provides temporary typed values to
inner middleware.

`startRuntime` creates a base runtime whose hooks are identities. Middleware
then installs behavior by updating one or more fields.

Before execution, all typed context requirements must be discharged, leaving
a `Runtime '[] m`.

### 3.1 `Program` and the `Agent` effect

`Program m` describes what an agent does: its model/tool control flow. The
`Agent` effect opens the scope in which one such agent runs:

```haskell
Agent.withRun ... \runtime ->
  consume (Agent.agentStream runtime transcript)
```

The function after `withRun` is the scoped body: it receives the prepared
runtime and performs the actual run. This lets the surrounding scope supply
execution context such as origin identity and resource ownership before the
body starts.

Root agents and child agents therefore have the same shape. A child simply
opens another scope with locally adjusted context; it is not a different kind
of runner. `Program` remains the computation being interpreted, while the
`Agent` effect supplies the scope in which it is interpreted.

## 4. Middleware design

Most middleware is a runtime transformation:

```haskell
Runtime context m -> Runtime context m
```

A middleware that provides a typed context value may instead remove a
requirement:

```haskell
Runtime (Provided ': context) m -> Runtime context m
```

### 4.1 Choose the narrowest hook

`Runtime` provides one hook for each useful boundary:

| Requirement | Hook |
|---|---|
| Select or rewrite the next model transcript | `modelInputTranscript` |
| Transform the whole coinductive computation | `aroundProgram` |
| Wrap the lifetime of one complete run | `aroundAgentRun` |
| Wrap one model request and its resulting `Step` | `aroundModelTurn` |
| Wrap one complete tool phase | `aroundToolTurn` |
| Wrap one individual tool call | `aroundToolCall` |

For example:

- [`withTypingNotification`](../lib/Bot/Agent/Middleware/Typing.hs) uses
  `aroundAgentRun`, because typing notification has the same lifetime as the
  run.
- [`withToolFailureRecovery`](../lib/Bot/Agent/Middleware/Tools.hs) uses
  `aroundToolCall`, because it classifies failures from one call.
- [`withToolLimit`](../lib/Bot/Agent/Middleware/Tools.hs) uses
  `aroundProgram`, because it must inspect `Visible (RunTools ...)` and may
  replace it with `Finished`.

Using the narrowest hook keeps each middleware independent of unrelated
agent phases.

### 4.2 Delegate to the previous hook

A minimal model-turn middleware looks like this:

```haskell
withModelTurnObserver
  :: Monad m
  => (TurnState -> m ())
  -> (Step m Result -> m ())
  -> Runtime context m
  -> Runtime context m
withModelTurnObserver before after runtime =
  runtime
    { aroundModelTurn = \context continue state action -> do
        lift (before state)
        step <- runtime.aroundModelTurn context continue state action
        lift (after step)
        pure step
    }
```

Two rules matter:

1. Delegate to `runtime.aroundModelTurn`, not directly to `action`.
2. Delegate exactly once.

The first rule preserves middleware already installed in the chain. The
second prevents the model turn from running more than once.

### 4.3 Preserve recursive continuations

Middleware using `aroundProgram` must recursively wrap every successor:

```haskell
go (Program action) =
  Program do
    action >>= \case
      Finished result ->
        pure (Finished result)
      Continues next ->
        pure (Continues (go next))
      Visible event continue ->
        pure (Visible event (go . continue))
```

A middleware can add policy in any branch, but it must preserve both
`Continues` and every `Visible` continuation. Otherwise it only applies to the
first observed layer.

`aroundProgram` also receives the final `Runtime '[] m`. Structural
middleware that needs to execute tools or inspect final tool exposure can use
the complete middleware chain instead of bypassing middleware installed
later.

### 4.4 Typed middleware context

[`withToolResultCompaction`](../lib/Bot/Agent/Middleware/ToolResultCompaction.hs)
is a provider:

```haskell
withToolResultCompaction
  :: Media.Media :> es
  => Runtime (ToolResultObservation es ': context) (Eff es)
  -> Runtime context (Eff es)
```

It constructs a `ToolResultObservation` and pushes it onto the HList whenever
it delegates to inner hooks. Observation middleware can require that value
with `HList.Has`.

The type checker consequently verifies middleware context wiring. If no
middleware provides a required value, the chain cannot become
`Runtime '[] (Eff es)` and cannot be passed to `agentStream`.

### 4.5 Middleware composition

The default middleware stack is ordinary function composition:

```haskell
defaultRuntime observer threshold =
  ( withContinuations
  . withToolLimit isResumeTransfer
  . withTypingNotification
  . withToolResultCompaction
  . withObservation observer
  . withToolMessage
  . withContextCompactionNotice threshold
  . withToolFailureRecovery
  )
```

The runner does not recognize tool limits or continuations specially. Every
entry in this chain is a `Runtime` transformation, and the final result has
type `Runtime '[] (Eff es)`.

## 5. Adding middleware

Use the following procedure:

1. Decide whether the behavior belongs around the run, a model turn, a tool
   turn, a tool call, transcript selection, or the `Program` structure.
2. Add a module under `Bot.Agent.Middleware.*`.
3. Update only the narrowest suitable hook.
4. Delegate to the previous hook exactly once.
5. If traversing `Program`, recursively preserve every continuation.
6. If middleware must exchange temporary data, use a typed HList
   provider/consumer instead of adding fields to `Context` or `TurnState`.
7. Add the transformation to the appropriate runtime composition.
8. Add a focused test for ordering, typed-context provision, or structural
   transformation.

Middleware that only wraps an existing hook usually needs no new core type.
Changing `Step` is reserved for a genuinely new kind of control interaction.

## 6. Correspondence with Interaction Trees

A conventional ITree has the following shape:

```haskell
data ITree event result
  = Ret result
  | Tau (ITree event result)
  | forall response.
      Vis (event response) (response -> ITree event result)
```

Cosmobot uses different names but the same control structure:

| ITree | Cosmobot |
|---|---|
| `Ret result` | `Finished result` |
| `Tau next` | `Continues next` |
| `Vis event continue` | `Visible event continue` |
| Event signature | `AgentEvent` |
| Handler/interpreter | `runProgram` and structural middleware |

The agent event signature is:

```haskell
data AgentEvent response where
  RunModel
    :: TurnState
    -> AgentEvent (TurnState, LLM.ChatAnswer)

  RunTools :: ToolRequest -> AgentEvent TurnState
```

`RunModel` and `RunTools` are both visible interactions. Each constructor
selects the response type accepted by its continuation, exactly as an ITree
event signature does. Cosmobot's `trigger` is the usual ITree operation that
constructs one visible event and returns its response.

Cosmobot does not literally define `data ITree`. Its representation fuses two
practical layers into each observation:

1. `Stream` can emit several `Output` values before exposing the next
   structural step.
2. Observation runs in the carrier `m`, rather than reifying every infrastructure
   effect into the event GADT.

If `Output` is treated as another visible event and `m` as the base
interpretation capability, `Program` has the same recursive semantics as an
ITree. Its `Monad` instance is the standard ITree bind: continue after
`Finished`, preserve `Continues`, and compose after every `Visible`
continuation.

## 7. Summary

The design reduces to five relationships:

1. `Program` describes the remaining agent computation.
2. `Step` exposes return, silent continuation, or a typed `AgentEvent`.
3. Middleware composes narrow `Runtime` hooks around that computation.
4. The `Agent` effect opens the execution scope for root or nested runs.
5. `runProgram` interprets the program into a stream with a final `Result`.

Most new agent policy should therefore be a middleware transformation, not a
change to the core recursion. The core calculus only needs to change when the
agent gains a genuinely new kind of control interaction.
