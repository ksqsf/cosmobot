# Agent Programs, Python, and the Future DSL

> **Status:** The program calculus, Haskell entry point, and `orchestrate_tools`
> tool-composition frontend are implemented. A general DSL, Python APIs that
> construct arbitrary agent programs, and foreign continuation handles remain
> future work.

## The central observation

An agent run is part of a `Program`.

`Program m result` is the unit of composition. A program can enter an agent
run, receive its result, and continue with more computation:

```haskell
program = do
  prepare
  first <- agent initialTranscript
  inspect first
  second <- agent (followUp first)
  finish first second
```

Here `agent` stands for an entry point that returns a `Program`. The agent run
occupies one region of the surrounding computation. Its final `Result` is an
ordinary program value available to the continuation.

The current Haskell function `agentProgram` is such an entry point. It starts
with a `TurnState`, emits `RunModel` and `RunTools` events recursively, and
returns a `Result`:

```haskell
agentProgram
  :: Runtime context (Eff es)
  -> TurnState
  -> Program (Eff es) Result
```

`agentStream` currently chooses this entry point and immediately folds the
resulting program:

```haskell
runProgram runtime (agentProgram runtime initialState)
```

The phrase “Haskell agent” therefore names the current entry into the program
calculus. The surrounding `Program` is the larger abstraction.

The deeper property is that a running program can produce its own next
program. `Visible` stores a continuation:

```haskell
response -> Program m result
```

The next `Program` comes into existence after the response arrives. A model
answer, a tool result, interpreter policy, or another agent run can therefore
determine the program that follows. The interaction tree grows while it is
being interpreted.

This makes agent programs generative. Programs compose statically with monadic
bind, and they also extend themselves dynamically from runtime values.

```text
run Program
    |
    v
observe event ---> model / tool / interpreter / nested agent
    ^                              |
    |                              v
    `-------- newly produced Program
```

## Composition boundary

The pieces have distinct roles:

| Piece | Role |
|---|---|
| `Program m result` | The complete composable computation |
| `agentProgram` | The current Haskell entry point for one agent run |
| `AgentEvent` | Observable operations emitted during that run |
| `Runtime` | Interpretation and middleware environment |
| `runProgram` | Fold from `Program` to streamed output and a final value |

This arrangement permits computations before and after an agent run, multiple
agent runs in sequence, and decisions based on returned results. Ordinary
carrier actions enter through `MonadTrans`; model and tool interactions enter
through typed `AgentEvent` nodes.

The continuation of each `Visible` node represents the rest of the whole
program. During an agent run, that continuation contains the rest of the run
and any computation composed after it. Structural middleware therefore sees
the actual surrounding control flow.

A runtime value can itself describe a program. Once that value is decoded into
the program calculus, ordinary monadic composition splices it into execution:

```haskell
generated <- produceProgram
runGenerated generated
continue
```

The producer may be the current model, the interpreter handling an event, or a
tool that invokes another agent.

## Implemented `orchestrate_tools` frontend

`orchestrate_tools` is a special agent tool implemented as structural middleware. It
recognizes a sole exposed `orchestrate_tools` call at a `RunTools` node and replaces
that node with a program that:

1. starts a fresh sandboxed Python worker;
2. lets Python call one or several exposed ordinary tools through `tools.run`;
3. routes every nested call through the normal per-tool middleware and runner;
4. turns Python completion or failure into the outer tool result; and
5. resumes the original `RunTools` continuation exactly once.

The model can therefore use Python as control flow for real multi-tool
composition. Calls within one `tools.run` batch execute concurrently, while
successive batches can depend on earlier results. The configured wall time,
CPU, memory, and nested-tool-call limits are included in the tool description.

The frontend is intentionally narrower than a general foreign program API.
Each invocation receives a fresh worker and `/work`, and Python cannot retain
a Haskell continuation or construct arbitrary `AgentEvent` nodes. Agent core
has no Python-specific branch: application wiring installs
`withPythonMiddleware`, so root agents and managed subagents share the same
program transformation.

The naming boundary is deliberate. `interpretPython` transforms the relevant
agent-program structure. The sandbox/protocol callback runs Python and is
called a runner, not an interpreter. Internal type aliases describe these
roles without enlarging the public API.

## Future DSL and general Python API

A DSL provides another way to construct `Program` values. A Python binding
provides a foreign-language frontend to the same construction boundary.

```text
Haskell entry points ---\
DSL forms ---------------+--> Program --> Runtime --> output and result
Python frontend --------/
```

An `agent` form in that frontend can enter the existing recursive agent
program. Sequencing, result binding, conditionals, and other eventual language
forms compose around the returned program fragment.

The compelling Python interaction is dynamic generation:

```python
import cosmobot

response = cosmobot.ask_agent("write a dsl program")
cosmobot.run_code(response.code)
```

`ask_agent` runs one agent fragment inside the surrounding program. Its model
produces DSL code as data. `run_code` decodes that data into another `Program`
and continues interpretation under the same runtime capabilities and
middleware.

The same shape supports a tool-mediated path:

```text
Program
  -> tool call
  -> nested agent
  -> generated DSL code
  -> decoded Program
  -> continued execution
```

A future DSL may also express ordinary composition directly:

```text
let first = agent(initial_transcript)
inspect(first)
let second = agent(follow_up(first))
finish(first, second)
```

The exact syntax and foreign protocol will follow the needs of the first real
consumer. The semantic target is already clear: frontends construct program
fragments, running programs can produce further fragments, and the existing
runtime interprets the resulting computation.

## Code as the portable form

The DSL source or syntax tree is the portable representation of a generated
program. Decoding gives it the live semantics of `Program m result`. This
separates two useful forms:

| Form | Role |
|---|---|
| DSL code or syntax tree | Model-visible, serializable program data |
| `Program m result` | Live computation with typed continuations and runtime effects |

Models and foreign processes can produce the first form. The host validates
and decodes it into the second. Runtime capabilities then define the tools and
effects available during execution.

## Continuations across a foreign boundary

Inside Haskell, a visible continuation has type:

```haskell
response -> Program m result
```

A Haskell closure is ephemeral runtime state. A Python or cross-process
frontend can refer to a live continuation through a host-owned opaque handle.
The handle belongs to one execution session, accepts the typed response for its
event, and resumes the surrounding program once.

This preserves the current logical one-shot semantics and keeps middleware
interposition at the same `Visible` boundary.

## Implementation status

The repository currently provides:

- the `Program` calculus and its monadic composition;
- `agentProgram` as the standard agent-run entry point;
- `Runtime`, middleware, and `runProgram` interpretation;
- streamed `Output` and returned `Result` values;
- `orchestrate_tools` structural interception and resumption;
- fresh sandboxed Python execution with configured resource limits; and
- real single- and multi-tool dispatch from Python through normal tool
  middleware.

Future implementation work covers:

- an execution entry point accepting a caller-constructed `Program`;
- the DSL surface and its decoding into `Program`;
- the Python `ask_agent` and `run_code` boundary;
- dynamic insertion of generated program fragments;
- lifecycle, validation, streaming, and cancellation for foreign continuation
  handles.

[`Agent.md`](Agent.md) describes the implemented calculus and runtime.
