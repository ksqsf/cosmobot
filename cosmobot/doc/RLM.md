# Recursive Transcript

Cosmobot implements an RLM-inspired context strategy for long-running agent
conversations. It keeps the full transcript as canonical state, sends only a
small working view to the root model, and lets models inspect or recursively
analyze hidden history.

## Motivation

Long contexts cost more and are not always used reliably; relevant facts can be
"lost in the middle" [1]. Cosmobot's compaction strategy reduces context by
replacing old turns with a summary, but that is lossy.

Recursive transcript keeps exact history recoverable:

- **Canonical transcript**: complete history used by persistence, audit, and
  tools.
- **Model view**: leading system messages, an externalization notice, and the
  current user turn.

Compaction and recursive transcript are mutually exclusive strategies.

## Background

The original Recursive Language Model (RLM) stores an arbitrary prompt in a
persistent REPL variable. The root model writes programs that inspect the
prompt, keep intermediate values, and invoke models over programmatically
constructed slices [2].

The paper distinguishes recursion depths:

- depth 0: REPL without model sub-calls;
- depth 1: programs call ordinary sub-LLMs;
- depth greater than 1: programs call sub-RLMs with their own REPLs.

The essential idea is not merely retrieval. Model calls can be launched inside
loops and their results combined symbolically.

## Literature

- [Recursive Language Models][rlm] introduces external prompts, symbolic
  decomposition, and recursive model calls.
- [Lost in the Middle][litm] shows that nominal context size does not guarantee
  reliable use of information throughout that context.
- [RAG][rag] uses non-parametric retrieval, but normally lets a retriever select
  chunks instead of letting the model program the decomposition.
- [MemGPT][memgpt] treats limited model context as a working-memory tier backed
  by larger retained state.
- [ReAct][react] provides the reasoning/action loop used when a model retrieves
  evidence and continues.

## Implementation

```toml
[handler.ask]
context_strategy = "recursive_transcript"
context_compaction_threshold_ktokens = 1000
```

`defaultRuntimeWithStrategy` installs either compaction or recursive transcript
projection. Tool-result compaction remains active in both modes.

When old history crosses the threshold, `withRecursiveTranscript` changes only
`modelInputTranscript`. It never rewrites `TurnState.transcript`, so storage
and tools retain the complete history.

The always-visible `transcript` tool operates on canonical history:

| Operation | Result |
|---|---|
| `info` | Message, character, and estimated-token counts |
| `search` | POSIX-regex matches with message indexes and snippets |
| `read` | Structured message range, capped at 200 messages |
| `query` | Recursive analysis of a selected snapshot |

`query` runs:

```text
root agent
  -> transcript.query(selected range)
     -> synchronous child agent
        tools = [transcript bound to selected snapshot]
        -> optional nested transcript.query(...)
           -> terminal tool-free LLM call
```

Children inherit the root origin id, use the normal agent/audit lifecycle, and
expose no side-effecting tools. Limits are depth 3, 32 recursive queries, 200
messages per snapshot, and 200,000 encoded characters at the terminal leaf.
They are synchronous and are not persistent `subagent` resources.

The root `py` tool also provides symbolic decomposition: generated Python can
loop, branch, batch `transcript` calls, retain intermediate values, and
aggregate child answers.

### Difference from the reference RLM

| Cosmobot does more | Cosmobot does less |
|---|---|
| Separates canonical state from model views | Does not expose the raw prompt as a REPL variable |
| Integrates persistence, audit, cancellation, metadata, and tool-result compaction | `py` state is fresh per call, not persistent across root turns |
| Preserves structured roles and tool-call/result pairing | Recursive children receive `transcript`, not their own Python REPL |
| Enforces read-only snapshots and shared budgets | Uses fixed depth, input, and call limits |
| Reuses the production agent runtime for children | Has not reproduced the paper's benchmarks or RLM training |

Both systems support code-driven symbolic decomposition. The reference RLM is
more general; Cosmobot is more constrained but more deeply integrated with a
persistent chat-agent runtime.

## Related Work

- **Compaction** is cheaper but lossy.
- **Long-context prompting** sends everything on every turn.
- **RAG** adds an index and automatic retrieval; Cosmobot directly searches the
  exact transcript.
- **MemGPT-style memory** manages mutable memory tiers; recursive transcript is
  read-only and does not replace persistent user/chat memory.
- **Subagents** are durable, user-manageable resources; recursive transcript
  children are synchronous calls over one immutable snapshot.

## References

1. Nelson F. Liu et al. [*Lost in the Middle: How Language Models Use Long Contexts*][litm]. TACL, 2024.
2. Alex L. Zhang, Tim Kraska, and Omar Khattab. [*Recursive Language Models*][rlm]. 2025.
3. Alex L. Zhang et al. [Official RLM implementation](https://github.com/alexzhang13/rlm).
4. Patrick Lewis et al. [*Retrieval-Augmented Generation for Knowledge-Intensive NLP Tasks*][rag]. 2020.
5. Charles Packer et al. [*MemGPT: Towards LLMs as Operating Systems*][memgpt]. 2023.
6. Shunyu Yao et al. [*ReAct: Synergizing Reasoning and Acting in Language Models*][react]. ICLR, 2023.

[rlm]: https://arxiv.org/abs/2512.24601
[litm]: https://aclanthology.org/2024.tacl-1.9/
[rag]: https://arxiv.org/abs/2005.11401
[memgpt]: https://arxiv.org/abs/2310.08560
[react]: https://arxiv.org/abs/2210.03629
