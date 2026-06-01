# Runtime representation and the dev/release split

This note records why Coalton carried two compilation modes for the runtime
representation of user types, why that split is avoidable, and the mode-free
design that replaces it. The account of the original motivation is
reconstructed from the code, not from the original authors; read it as
informed speculation.

## What the split was

`coalton-release-p` selected between two codegen paths for every user
`define-type` and every typeclass dictionary:

- **Development** (default): emit a CLOS `defclass` per constructor, with
  reader functions defined as `slot-value` wrappers.
- **Release** (`COALTON_ENV=release` or the `:coalton-release` feature):
  emit a `defstruct` per constructor with `:read-only` slots, `(declaim
  (inline ...))` on the constructor and readers, and `(declaim
  (sb-ext:freeze-type ...))` on the type. Release mode also enabled
  struct-only optimizations elsewhere -- stack allocation of non-escaping
  `match` arguments (`match-dynamic-extent-lift`) and the elision of
  fallback/branch code in exhaustive matches.

A load-time prologue asserted that a fasl compiled in one mode was not
loaded under the other.

## The speculative motivation

The two modes serve two goals that look, at first, like they need different
representations:

- **A REPL workflow wants redefinable types.** Redefining a `defclass` is
  routine; existing instances pick up the new layout and methods recompile.
  A `defstruct` whose type is `freeze-type`d cannot be redefined at all, and
  inlined accessors bake the slot layout into every call site, so even an
  unfrozen struct goes stale at its callers on redefinition. CLOS is the
  natural choice for interactive development.

- **Production wants fast field access.** A struct slot read is a fixed-offset
  fetch; CLOS `slot-value` goes through effective-slot lookup. `inline`
  turns reader calls into the fetch directly; `freeze-type` lets SBCL fold
  `typep` and discrimination; stack allocation cuts GC traffic. Structs win
  decisively on hot, data-structure-heavy code.

Given those framed as opposing, a global mode toggle is the obvious way to
have both: choose the representation per build. The cost is the one GOAL-025
is unwinding -- two parallel codegen paths, the type-encrustation that the
frozen-struct path drags in (`:read-only` defstructs, `satisfies` element
types, `freeze-type` declaims), and the mode-mismatch prologue.

## Why the split is avoidable

The framing above conflates two separable axes: the *representation*
(struct vs CLOS) and the *redefinability mechanism* (how a caller survives a
type changing under it).

Measuring a hot `match` loop (`benchmark/runtime/`) separates them. The
development path's cost is not "CLOS instead of struct" in the abstract; it
is the indirection that path used *to get redefinability*:

- field reads route through a global-lexical reader function (an opaque
  indirect call) rather than the accessor directly, so SBCL cannot
  open-code the read even when the value is statically typed; and
- every `match` performs a `typep`, even for single-constructor types where
  it is provably always true.

On that loop the current codegen is ~1.5x slower than open-coded, typed
field access -- and that 1.5x is exactly the indirection and the redundant
`typep`, not the struct representation. The representation itself (struct
over CLOS) is a clean win that does not require a mode.

The release path's speed, in turn, did not actually require sealing. It
required the compiler to know the concrete type at the access site. Coalton
*always* knows that -- it is a typed language. `freeze-type` and `inline`
were a way to communicate "this layout is fixed" globally; static type
information communicates the same thing locally, per access.

## The mode-free design

One representation, no toggle:

1. **User types and dictionaries are always (redefinable) structs.** No
   `freeze-type`, no `inline`, no CLOS path. Done; the dev/release split is
   removed.

2. **Field access is open-coded at the statically-known type.** At a `match`
   site the matched value's concrete type (and, for single-constructor
   types, its sole constructor) is known, so codegen can declare the type,
   call the struct accessor directly, and drop the `typep`. SBCL open-codes
   the read to an instance-ref. This recovers release-level access speed
   without sealing.

3. **Redefinability becomes recompilation, not indirection.** Open-coded
   access bakes the slot layout into the caller, so redefining a type must
   recompile its dependents. The compiler already tracks this
   (`src/redef-detection/`): it builds a reverse-dependency graph and can
   recompile exactly the affected definitions. The cost moves from an
   indirect call on *every* field access to a recompilation on the *rare*
   event of a redefinition -- the right trade for both a REPL and
   production.

The result keeps both properties the split was juggling -- redefinable
*and* fast -- on a single codegen path, and it is strictly better than the
old release mode, which bought the same speed by forbidding redefinition
outright.

Scalar replacement (eliminating the struct when a value is constructed and
immediately destructured) is a further, independent win;
`match-dynamic-extent-lift` is a partial form of it.

## Dependency

Step 3 above is a correctness claim: a missed dependency in the
reverse-dependency graph would leave a caller open-coded against a stale
layout -- a silent wrong-answer bug, not merely slower code. That graph is
built by querying the type environment (`lookup-value-type`, to tell a
global reference from a local binding), so it is only as consistent as
environment lookups are. The current environment answers through the
mutable `tc-env` / `partial-type-env` front-ends layered over the persistent
map, which makes the graph timing-dependent. Open-coded access therefore
sequences *after* the environment is rebuilt as a single functional binding
map (GOAL-025's environment step), which makes those lookups consistent --
not merely after the struct/CLOS and AST work.
