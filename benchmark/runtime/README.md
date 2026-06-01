# Runtime field-access micro-benchmark

Quantifies the struct field-access overhead in Coalton's generated code and
isolates where it comes from, to inform GOAL-025 codegen work. Run inside
`nix develop`:

```
sbcl --script benchmark/runtime/run-runtime.lisp
```

`point-loop.coal` defines a two-field `Point` and a tail loop that
destructures it each iteration, summing its fields. The runner compiles
that through the real Coalton compiler and times it against two
hand-written variants that progressively remove overhead. All three do the
same (generic-integer) arithmetic, so the difference isolates field access.

## Result (100,000,000 iterations, min of 3)

| variant | ns/iter | what it models |
|---------|--------:|----------------|
| v-coalton (current) | 6.53 | current codegen: `typep` guard + field reads through the global-lexical reader functions |
| v-typed (open-coded read) | 4.38 | concrete type declared, direct defstruct accessor, no `typep` |
| v-ideal (no struct) | 3.73 | the two values passed directly (scalar-replacement result) |

- current vs typed: **1.49x**
- current vs ideal: **1.75x**

(Absolute numbers are machine-specific; the ratios are the point.)

## Where the overhead is

The match in the current codegen (see a dump via `entry:codegen`) emits:

```lisp
(let ((#:m p))
  (declare (type point #:m))             ; the abstract PARENT type
  (cond
    ((and (typep #:m 'point/point) t t)  ; runtime dispatch, every iteration
     (let ((x (point/point-_0 #:m))      ; read via the global-lexical reader
           (y (point/point-_1 #:m)))
       ...))))
```

Two costs, neither being `freeze-type`/`inline`:

1. **A `typep` on every match**, even though `Point` has exactly one
   constructor, so it is provably always true.
2. **Field reads go through the global-lexical reader** (the `point/point-_0`
   *variable*, funcalled) rather than the defstruct accessor directly, so
   SBCL sees an opaque call and cannot open-code it to an instance-ref --
   even though the matched value is statically a `Point`.

The global-lexical indirection is the redefinability mechanism: redefining
a type updates the reader the callers funcall, without recompiling them.

## Avenues (in decreasing payoff, increasing effort)

1. **Open-coded typed access at known match sites** (recovers the ~33% in
   `v-typed`). Coalton knows the matched value's concrete type, so codegen
   can declare it and call the defstruct accessor directly; SBCL open-codes
   it. This recovers release-level access speed *without* `freeze-type` or
   `inline` -- it relies on static type knowledge, not global sealing. It
   does bake in the slot layout at the call site, so it requires
   recompiling dependents on type redefinition -- which the compiler
   already tracks (`src/redef-detection/`). Net: fast access *and*
   redefinable, paying recompilation latency on redefine rather than
   indirection on every access.

   Sequencing: this safety net is only as good as redef-detection's
   reverse-dependency graph, which `dependencies.lisp` builds by calling
   `lookup-value-type` against the environment to tell globals from
   locals. The current environment answers through the mutable
   `tc-env`/`partial-type-env` front-ends, so the graph can be
   timing-dependent; a missed dependency here would mean a stale
   slot-offset (a silent wrong-layout bug), not just slower code. So this
   codegen change must land *after* the GOAL-025 environment redesign
   (step 5, which gates on the step-4 replay story), once a single
   functional binding map makes those lookups consistent -- not merely
   after the AST/codegen CLOS work (stages 3-4).
2. **Skip the constructor `typep` for single-constructor types** (part of
   the same ~33%, and trivial -- the constructor count is known at
   codegen).
3. **Scalar replacement / deforestation** (the further ~15% in `v-ideal`):
   when a value is constructed and immediately destructured, avoid the
   allocation and thread the fields directly. `match-dynamic-extent-lift`
   (stack allocation) is a partial step; full SRA removes the aggregate.
