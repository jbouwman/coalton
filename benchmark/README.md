# Coalton per-phase baseline harness

A benchmark harness, independent of `coalton/tests` and the existing
benchmarks, that drives the compiler over a fixed corpus and reports,
per compiler phase, both correctness (output pinned and diffed) and
cost (wall-clock and allocation).

This is the regression oracle for the Coalton compiler simplification
(kreisler GOAL-025; the harness itself is GOAL-025 step 1 / PLAN-308).
Every later step of that work is measured against the baseline captured
here.

## Running

Inside `nix develop` (which puts the Coalton dependencies on
`CL_SOURCE_REGISTRY`):

```
make bench        # run the corpus, print the report, (re)write the baseline
make bench-diff   # run the corpus, compare correctness to the pinned baseline
```

`make bench-diff` exits non-zero if any phase output changed. The
baseline path defaults to `benchmark/baselines/baseline.sexp`; override
with `BASELINE=...`.

## Phases

The pipeline (from `src/entry.lisp`) is split into three measured phases:

1. **parse** -- `parser:read-program`: read (eclector) plus parse into
   the parser `program` AST. Read and parse are interleaved inside this
   call, so they are measured together for now (PLAN-308 stage 6 splits
   them).
2. **typecheck** -- the body of `entry:entry-point` through
   `analysis:analyze-translation-unit`, ending at a `translation-unit`
   and the inferred environment. `benchmark/phases.lisp:%typecheck` is a
   faithful copy of that body with the boundary exposed; it is the one
   place coupled to compiler internals and changes in lockstep with the
   later GOAL-025 steps.
3. **codegen** -- `codegen:compile-translation-unit`: the
   `translation-unit` to Lisp forms.

`entry-point` threads the environment functionally and never assigns
`*global-environment*`, so typechecking a corpus file against the
post-stdlib global environment is non-destructive and repeatable.

## Correctness oracles

Per corpus file (`benchmark/oracle.lisp`):

- **parse summary** -- counts per top-level form kind (full parser-AST
  pinning is PLAN-308 stage 7).
- **inferred signatures** -- each top-level value's inferred scheme
  rendered with `tc:type-to-string`. The primary inference-regression
  guard.
- **codegen text** -- the generated Lisp forms, with compilation
  artifacts (uninterned gensyms and gentemp-renamed locals) normalized
  to stable placeholders so the artifact is deterministic across runs
  while still diffing on real codegen changes.

## Cost

Per phase: median and min wall-clock and bytes consed over N iterations
(`*default-iterations*`). Each iteration re-runs the untimed prefix
(parse for typecheck; parse+typecheck for codegen) so the timed phase
sees fresh, unmutated input -- `rename-variables` / `resolve-control-flow`
mutate the `program` in place. Cost is reported but never fails the
diff.

## Corpus

`benchmark/corpus/*.coal`, `:file`-mode Coalton importing the prelude,
spanning small/medium/large and exercising the constructs the later
steps touch (heavy inference, many instances, large top-level forms).
Each file is validated through the harness as it is added.
