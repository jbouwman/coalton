# AST Representation: Design Issues and Alternatives

## Current State

The Coalton compiler maintains **three separate AST representations**:

1. **Parser AST** - `src/parser/expression.lisp` (1784 lines)
2. **Typechecker AST** - `src/typechecker/expression.lisp` (812 lines)
3. **Codegen AST** - `src/codegen/ast.lisp` (435 lines)

## The Problem

### Issue 1: Massive Duplication

The Typechecker AST file literally states at the top:

```lisp
;;;;
;;;; Mirror of expression nodes in src/parser/expression.lisp with
;;;; types attached.
;;;;
```

**Analysis:**
- Parser AST: ~40 node types, no type information
- Typechecker AST: ~38 node types (same structure), adds `type` field
- Codegen AST: ~25 node types (different structure)

**Duplication metrics:**
- 812 lines of near-identical struct definitions
- Same accessor names with different packages
- Same traversal patterns duplicated
- Same validation logic duplicated

### Issue 2: No Clear Transformation Specification

**Nodes that disappear** between Parser and Typechecker ASTs:
- `node-the` - Explicit type annotations
- `node-let-declare` - Type declarations in let bindings

**From the code** (`src/typechecker/expression.lisp:319`):
```lisp
;; node-the does not exist in this AST!
```

**Question:** Where is this transformation documented? **Answer:** It's not.

**Nodes that disappear** between Typechecker and Codegen ASTs:
- `node-or` - Boolean OR
- `node-and` - Boolean AND
- `node-if` - Conditional
- `node-when` - When guard
- `node-unless` - Unless guard
- `node-cond` - Multi-way conditional
- `node-do` - Do notation

**Question:** What are the desugaring rules? **Answer:** Must read translation code.

### Issue 3: Inconsistent Abstractions

**Parser/Typechecker AST pattern:**
```lisp
(defstruct (node-variable (:include node))
  (name :type identifier :read-only t))
```

**Codegen AST pattern:**
```lisp
(defstruct (node-variable (:include node))
  (value :type parser:identifier :read-only t))
```

**Different field names for same concept!**
- Parser: `name`
- Codegen: `value`

**Different base structs:**
- Parser/Typechecker: `(defstruct node (location ...))`
- Codegen: `(defstruct node (type ...))`

No location information in Codegen AST!

### Issue 4: Type Representation Inconsistency

**Typechecker AST:**
```lisp
(defstruct (node ...)
  (type :type tc:qualified-ty ...))
```

**Codegen AST:**
```lisp
(defstruct (node ...)
  (type :type tc:ty))
```

**Why the difference?**
- Qualified types include type class predicates
- Simple types do not
- Predicates converted to dictionary parameters during codegen

**Is this documented?** No.

## Why Three ASTs?

### Potential Rationale (Undocumented)

**Separation of Concerns:**
- Parser AST: Represents concrete syntax
- Typechecker AST: Represents typed terms
- Codegen AST: Represents executable IR

**Different Information:**
- Parser: Source locations, concrete syntax
- Typechecker: Types, constraints, qualified types
- Codegen: Simplified types, low-level constructs

**Phase Independence:**
- Each phase can evolve independently
- No coupling between representations
- Clear boundaries between phases

**But:**
- These benefits are theoretical
- In practice: high maintenance cost
- No actual independence (changes ripple through all ASTs)
- Boundaries are NOT clear (transformations undocumented)

## Design Alternatives

### Alternative 1: Single Parameterized AST

**Idea:** Use type parameters to distinguish AST phases.

```lisp
;; Generic AST parameterized by type info
(defstruct (node (:include typed-node))
  (location :type source:location))

;; Untyped nodes (parser phase)
(deftype untyped-node ()
  '(node nil))

;; Typed nodes (typechecker phase)
(deftype typed-node ()
  '(node tc:qualified-ty))

;; IR nodes (codegen phase)
(deftype ir-node ()
  '(node tc:ty))
```

**Pros:**
- Single source of truth for AST structure
- Type system enforces phase boundaries
- Transformations explicitly typed

**Cons:**
- More complex type system
- Harder to understand for newcomers
- May not work well in Common Lisp

### Alternative 2: Single AST with Optional Fields

**Idea:** Use one AST with optional type information.

```lisp
(defstruct node
  (location :type source:location)
  (type :type (or null tc:qualified-ty)))

;; Parser produces nodes with type = nil
;; Typechecker fills in type field
```

**Pros:**
- Simplest approach
- Easy to understand
- Natural transformation

**Cons:**
- No static guarantees about type presence
- Runtime checks needed
- Codegen still needs different structure

### Alternative 3: Typed Transformations

**Idea:** Keep separate ASTs but add typed transformation functions.

```lisp
;; Explicit transformation types
(defun parser->typechecker (parser-node)
  (declare (type parser:node parser-node)
           (values tc:node))
  ...)

(defun typechecker->codegen (tc-node)
  (declare (type tc:node tc-node)
           (values codegen:node))
  ...)
```

**Pros:**
- Clear transformation boundaries
- Type-checked transformations
- Easier to test phases independently

**Cons:**
- Still have duplication
- Need to maintain separate ASTs
- Documentation still required

### Alternative 4: Macro-Generated ASTs

**Idea:** Use macros to generate AST structs from single specification.

```lisp
(define-ast-node variable
  :fields ((name identifier))
  :in-parser t
  :in-typechecker t
  :in-codegen t)

;; Expands to appropriate defstructs for each phase
```

**Pros:**
- Single source of truth
- Less duplication
- Can generate traversals automatically

**Cons:**
- Complex macro machinery
- Debugging harder
- Less flexible for phase-specific nodes

### Alternative 5: Keep Current Design, Add Documentation

**Idea:** Accept current design but thoroughly document it.

**What to add:**
- Explicit transformation tables
- Rationale for each AST
- Node lifecycle documentation
- Desugaring rules specification

**Pros:**
- No code changes needed
- Backward compatible
- Can be done incrementally

**Cons:**
- Doesn't fix fundamental issues
- Duplication remains
- Maintenance burden remains

## Specific Problems with Current Design

### Problem: Parser → Typechecker Transformation

**Missing documentation:**

| Parser Node | Typechecker Node | Transformation |
|-------------|------------------|----------------|
| `node-the` | *removed* | Type annotation extracted, applied to inner expression |
| `node-let-declare` | *removed* | Type constraint added to environment |
| `node-variable` | `node-variable` | Type inferred and attached |
| `node-application` | `node-application` | Type inferred via unification |

**Questions:**
- How is `node-the` type information propagated?
- Where are `node-let-declare` constraints stored?
- What happens if inference conflicts with declarations?

### Problem: Typechecker → Codegen Transformation

**Missing documentation:**

| Typechecker Node | Codegen Node | Desugaring Rule |
|------------------|--------------|-----------------|
| `node-or` | `node-match` | `(or a b)` → `(match a (True True) (False b))` |
| `node-and` | `node-match` | `(and a b)` → `(match a (True b) (False False))` |
| `node-if` | `node-match` | `(if c t e)` → `(match c (True t) (False e))` |
| `node-when` | `node-match` | `(when c b)` → `(match c (True b) (False Unit))` |
| `node-unless` | `node-match` | `(unless c b)` → `(match c (False b) (True Unit))` |
| `node-cond` | `node-match` | Nested matches |
| `node-do` | `node-bind` | Desugared to bind chain |
| `node-application` | `node-direct-application` | When function is known and saturated |

**Questions:**
- Are these desugaring rules correct?
- Are there edge cases?
- How are source locations preserved?
- What about type preservation?

### Problem: Field Name Inconsistency

**Parser AST:**
```lisp
(defstruct (node-variable (:include node))
  (name :type identifier))
```

**Codegen AST:**
```lisp
(defstruct (node-variable (:include node))
  (value :type parser:identifier))
```

**Why `name` vs `value`?** No documented reason.

**Impact:**
- Can't write generic traversals
- Easy to make mistakes during transformation
- Confusing for maintainers

### Problem: No Location Information in Codegen

**Parser/Typechecker:**
```lisp
(defstruct (node ...)
  (location :type source:location))
```

**Codegen:**
```lisp
(defstruct (node ...)
  (type :type tc:ty))
```

**No location field!**

**Impact:**
- Can't generate good error messages in codegen phase
- Optimization passes can't report source locations
- Debugging generated code is harder

**Question:** Where did location information go?

## Maintenance Burden

### Cost of Adding New Node Type

**Current approach:**
1. Add struct definition to Parser AST
2. Add parsing code to `parse-expression`
3. Add struct definition to Typechecker AST (duplicate)
4. Add typechecking code to `infer-expression-type`
5. Add substitution method
6. Decide if node should exist in Codegen AST
7. If yes: add struct definition to Codegen AST
8. Add translation code
9. Add codegen code
10. Update documentation (if it exists)

**Estimated:** 200-500 lines of code across 5+ files

### Cost of Changing Existing Node

**Example:** Add field to `node-application`

**Required changes:**
1. Update Parser AST struct
2. Update parser code to populate field
3. Update Typechecker AST struct
4. Update typechecker code to transform field
5. Update substitution method
6. Update Codegen AST struct (maybe)
7. Update translation code
8. Update codegen code

**Risk:** Easy to miss one and create subtle bugs

### Cost of Refactoring

**Scenario:** Want to change how let bindings work

**Impact:**
- `node-let` in Parser AST
- `node-let-binding` in Parser AST
- `node-let-declare` in Parser AST
- Same structures in Typechecker AST
- `node-let` in Codegen AST (different structure)
- `node-bind` in Codegen AST
- All transformation code
- All validation code
- All codegen code

**Estimated effort:** Days to weeks

## Questions for Design Review

1. **Is the three-AST design intentional or accidental?**
   - Was this explicitly chosen or did it evolve?
   - What were the design constraints?

2. **What are the actual requirements?**
   - Do we need source locations in codegen?
   - Do we need type information in parser?
   - What phase boundaries are essential?

3. **What transformations are guaranteed?**
   - Type preservation?
   - Semantics preservation?
   - Location preservation?

4. **Can we reduce duplication?**
   - Would parameterized AST work?
   - Can we generate ASTs from specification?
   - Is macro approach viable?

5. **What would migration look like?**
   - Can we refactor incrementally?
   - What's the minimum viable improvement?
   - What's the ideal end state?

## Recommendations

### Immediate (Documentation)

1. **Document all transformations**
   - Parser → Typechecker mapping table
   - Typechecker → Codegen mapping table
   - Desugaring rules with examples
   - Type preservation properties

2. **Add transformation tests**
   - Test each node transformation
   - Test type preservation
   - Test location preservation
   - Test edge cases

3. **Document design rationale**
   - Why three ASTs?
   - Why these specific representations?
   - What alternatives were considered?
   - What constraints drive the design?

### Short-term (Improvements)

4. **Standardize field names**
   - Use `name` or `value` consistently
   - Document naming conventions
   - Add linter rules

5. **Add location tracking to Codegen AST**
   - Preserve source locations
   - Better error messages
   - Easier debugging

6. **Create traversal utilities**
   - Generic AST visitors
   - Reduce code duplication
   - Make transformations easier

### Long-term (Redesign)

7. **Evaluate AST design alternatives**
   - Prototype parameterized AST
   - Try macro-generated approach
   - Measure complexity vs current

8. **Plan migration strategy**
   - Identify high-value improvements
   - Incremental refactoring plan
   - Backward compatibility strategy

9. **Improve tooling**
   - AST visualization
   - Transformation validation
   - Automated testing

## Conclusion

The current three-AST design has significant issues:

1. **Massive duplication** (812 lines of near-identical code)
2. **Undocumented transformations** (nodes appear/disappear mysteriously)
3. **Inconsistent conventions** (name vs value, location handling)
4. **High maintenance burden** (changes ripple across multiple files)

**However:** The design may have good reasons that are simply undocumented.

**Next steps:**
1. Document current design thoroughly
2. Gather requirements and constraints
3. Evaluate alternatives
4. Plan incremental improvements

**The goal:** Make the compiler easier to understand, maintain, and evolve.
