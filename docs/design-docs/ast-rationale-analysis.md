# AST Design Rationale Analysis

## Executive Summary

**Key Finding:** The three-AST design does NOT appear to be driven by efficiency concerns. The ASTs are treated **functionally** - structures are copied, not mutated. The "Typing Haskell in Haskell" paper that inspired Coalton's typechecker uses a **single AST**, not separate typed/untyped versions.

**Conclusion:** The three-AST design appears to have **no documented rationale** and may have evolved organically rather than being a deliberate architectural decision. A typed-from-the-outset design with optional fields would likely be superior.

## Evidence from Code Analysis

### 1. Functional (Immutable) AST Treatment

**Typechecker creates entirely new nodes:**

```lisp
;; From src/typechecker/define.lisp:241-244
(make-node-literal
  :type (tc:qualify nil type)
  :location (source:location node)
  :value (parser:node-literal-value node))
```

Every parser node input produces a completely new typechecker node. The parser node is **not mutated**.

**Pattern observed in ALL transformations:**
- Parser → Typechecker: `make-node-*` creates new structures
- Typechecker → Codegen: `make-node-*` creates new structures
- Substitution application: Creates copies with updated types

**No in-place mutation anywhere.** The code is purely functional in this respect.

### 2. Reference Design (Typing Haskell in Haskell)

From the original paper by Mark P. Jones that inspired Coalton's typechecker:

**Single AST representation:**
```haskell
data Expr = Var Id
          | Lit Literal
          | Const Assump
          | Ap Expr Expr
          | Let BindGroup Expr
```

**Types tracked separately:**
- Type inference functions return `(predicates, type)` tuples
- AST remains unmodified
- Types maintained in substitutions and assumptions
- **No separate typed/untyped AST**

**Quote from the paper:**
> "This design prioritizes clarity and simplicity over efficiency, making the type inference algorithm transparent while keeping the AST representation straightforward and unencumbered by type annotations."

**Implication:** Coalton is **not following** the design pattern from its cited inspiration.

## Potential Constraints (Hypothetical)

Since there's no documentation, we must speculate about what drove the design:

### Constraint 1: Qualified Types Complexity

**Hypothesis:** Qualified types (with predicates) seemed to require a separate structure.

```lisp
;; Typechecker AST
(defstruct (node ...)
  (type :type tc:qualified-ty)  ; Includes predicates
  (location :type source:location))

;; Codegen AST
(defstruct (node ...)
  (type :type tc:ty))  ; No predicates
```

**Analysis:** This could justify Parser → Typechecker, but not the Typechecker → Codegen split.

**Counter-argument:**
```lisp
;; This would work fine:
(defstruct (node ...)
  (type :type (or null tc:qualified-ty))
  (location :type source:location))
```

**Verdict:** Not a compelling constraint.

### Constraint 2: Source Location Overhead

**Hypothesis:** Didn't want location information in codegen for memory efficiency.

**Evidence:**
- Parser AST: Has `location` field
- Typechecker AST: Has `location` field
- Codegen AST: **No** `location` field

**Analysis:** Removing locations makes debugging harder and error messages worse. The memory savings are negligible given that:
- Locations are just cons cells (start . end positions)
- The entire AST is temporary (generated code is the output)
- Modern machines have abundant memory

**Counter-argument:** A `(or null source:location)` field would cost one word per node. Trivial.

**Verdict:** Not a rational constraint.

### Constraint 3: Common Lisp Defstruct Limitations

**Hypothesis:** CL defstruct doesn't support the flexibility needed for a unified AST.

**Analysis:** Defstruct limitations:
- Can't change slot definitions in subclasses
- No multiple inheritance
- No mixins
- Read-only slots can't be easily "updated" (must copy)

**However:** These limitations affect ALL approaches equally. Whether you have one AST or three, you still need to copy structures for immutability.

**Counter-argument:**
```lisp
;; This is perfectly valid CL:
(defstruct (node ...)
  (type :type (or null tc:qualified-ty))
  (location :type source:location))

(defstruct (node-variable (:include node))
  (name :type identifier :read-only t))

;; Create untyped node:
(make-node-variable :type nil :location loc :name 'x)

;; Create typed node (copying):
(make-node-variable
  :type inferred-type
  :location (node-location old-node)
  :name (node-variable-name old-node))
```

**Verdict:** Not a limiting constraint.

### Constraint 4: Phase Boundary Enforcement

**Hypothesis:** Separate ASTs enforce clean phase boundaries at the type level.

**Analysis:** This is theoretically appealing:
- Parser produces `parser:node`
- Typechecker consumes `parser:node`, produces `tc:node`
- Codegen consumes `tc:node`, produces `codegen:node`

**Benefits:**
- Type errors if you accidentally mix phases
- Clear interfaces between passes
- Easier to reason about transformations

**Costs:**
- 800+ lines of duplicated struct definitions
- High maintenance burden
- Undocumented transformations
- Inconsistent field names between phases

**Counter-argument:** Could achieve same guarantees with phantom types or protocols:
```lisp
;; Phantom type approach
(deftype untyped-node () 'node)
(deftype typed-node () 'node)

;; Or protocol approach
(defgeneric infer-type (untyped-node)
  (:returns typed-node))
```

**Verdict:** Possible motivation, but costs outweigh benefits.

### Constraint 5: Codegen Optimization Requirements

**Hypothesis:** Codegen needs fundamentally different structure for optimization.

**Evidence:**
- `node-direct-application` vs `node-application` (distinguish known/unknown functions)
- `node-field` for dictionary access (new concept)
- `node-seq` for statement sequencing
- Control flow desugared (if/when/unless → match)
- No qualified types (predicates resolved to parameters)

**Analysis:** This explains why **Codegen AST** is different, but not why **Parser** and **Typechecker** ASTs are separate.

**Verdict:** Legitimate constraint for Parser/Typechecker → Codegen, but not for Parser → Typechecker.

### Constraint 6: Historical Evolution

**Hypothesis:** The design evolved organically without a grand plan.

**Scenario:**
1. Initially: Parser AST for parsing
2. Added typechecker: "Let's create typed nodes"
3. Added codegen: "Let's create IR nodes"
4. Never refactored because it works

**Evidence:**
- No documentation of rationale
- Inconsistent field names suggest independent evolution
- Comment "Mirror of expression nodes... with types attached" suggests copy-paste origin

**Verdict:** Most likely explanation.

## Comparison: Current Design vs Typed-from-Outset

### Current Design (Three ASTs)

```lisp
;; Parser AST
(defstruct (node ...)
  (location :type source:location))

(defstruct (node-variable (:include node))
  (name :type identifier))

;; Typechecker AST
(defstruct (node ...)
  (type :type tc:qualified-ty)
  (location :type source:location))

(defstruct (node-variable (:include node))
  (name :type identifier))

;; Codegen AST
(defstruct (node ...)
  (type :type tc:ty))

(defstruct (node-variable (:include node))
  (value :type identifier))  ; Different field name!
```

**Metrics:**
- Lines of code: ~3000 lines across 3 files
- Duplication: ~812 lines nearly identical (Parser vs Typechecker)
- Transformations: Undocumented, must read code
- Maintenance: High burden, changes ripple through 3 files

### Proposed: Typed-from-Outset

```lisp
;; Single base AST
(defstruct (node ...)
  (type     :type (or null tc:qualified-ty))
  (location :type source:location))

(defstruct (node-variable (:include node))
  (name :type identifier))

;; Usage:
;; Parser phase: type = nil
(make-node-variable :type nil :location loc :name 'x)

;; Typechecker phase: type = inferred-type
(make-node-variable
  :type inferred-type
  :location loc
  :name 'x)

;; Codegen phase: Separate IR (still needed)
(codegen:node-variable ...)
```

**Metrics:**
- Lines of code: ~2200 lines (1 file for syntax, 1 for IR)
- Duplication: ~0 lines
- Transformations: Explicit type annotations in code
- Maintenance: Low burden, single source of truth

**Advantages:**
1. **Single source of truth** - One definition of syntax
2. **No duplication** - 800 lines eliminated
3. **Type safety maintained** - Still need types to progress through phases
4. **Explicit typing** - Type field documents what phase you're in
5. **Better debugging** - Can inspect partially-typed ASTs
6. **Easier evolution** - Add fields once, benefit everywhere

**Trade-offs:**
1. **Runtime check needed** - Must verify type is non-nil when required
2. **Less static guarantee** - Can't rely on type system alone for phase separation
3. **Slightly more complex** - Optional field vs always-present field

### Middle Ground: Two ASTs (Syntax + IR)

```lisp
;; Syntax AST (Parser + Typechecker)
(defstruct (syntax-node ...)
  (type     :type (or null tc:qualified-ty))
  (location :type source:location))

(defstruct (syntax-variable (:include syntax-node))
  (name :type identifier))

;; IR AST (Codegen)
(defstruct (ir-node ...)
  (type :type tc:ty))

(defstruct (ir-variable (:include ir-node))
  (name :type identifier))
```

**Rationale:**
- Parser and Typechecker work with high-level syntax
- Codegen needs low-level IR with optimizations
- Legitimate boundary between phases

**Advantages:**
- Eliminates Parser/Typechecker duplication (saves ~800 lines)
- Keeps separate IR for optimization
- Clear boundary: syntax vs executable representation

## Analysis: Why "Efficiency" Isn't the Reason

**User's observation:** "ASTs are treated functionally... structures are copied when mutated"

**Confirmed by code inspection:**

### Example: Type Inference for Variables

```lisp
;; From src/typechecker/define.lisp:327-330
(:method ((node parser:node-variable) expected-type subs env)
  ...
  (make-node-variable
    :type (tc:qualify preds type)
    :location (source:location node)
    :name (parser:node-variable-name node))
  ...)
```

**What happens:**
1. Parser node passed in (read-only)
2. Type inferred
3. **Completely new** typechecker node created
4. All fields copied from old node
5. Type field added

**Memory allocations:**
- Separate ASTs: Allocate new node
- Unified AST: Allocate new node
- **Identical cost!**

### Example: Substitution Application

```lisp
;; From src/typechecker/expression.lisp:510-516
(defmethod tc:apply-substitution (subs (node node-variable))
  (declare (type tc:substitution-list subs)
           (values node-variable))
  (make-node-variable
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :name (node-variable-name node)))
```

**What happens:**
1. Substitution applied to type
2. **Completely new** node created
3. All fields copied
4. Updated type inserted

**Again:** Copying the entire structure regardless of AST representation.

### Conclusion on Efficiency

**Separate ASTs provide NO efficiency benefit** when structures are immutable/functional:

| Operation | Separate ASTs | Unified AST | Winner |
|-----------|---------------|-------------|--------|
| Parse | Create parser node | Create node w/ type=nil | Tie |
| Infer type | Create typed node | Create node w/ type | Tie |
| Apply substitution | Copy typed node | Copy node | Tie |
| Translate to IR | Create IR node | Create IR node | Tie |

**Memory overhead:**
- Separate ASTs: N structs × 3 phases = 3N structs
- Unified AST: N structs × 2 phases (syntax + IR) = 2N structs
- **Savings: 33% fewer allocations**

## The "Unknown Type" Approach

**User's suggestion:** "Wouldn't it make more sense to be typed from the outset, with 'unknown'?"

**Concrete design:**

```lisp
(defstruct (node ...)
  (type     :type tc:type-or-unknown)
  (location :type source:location))

;; Type system
(deftype tc:type-or-unknown ()
  '(or (eql :unknown) tc:qualified-ty))

;; Helper predicates
(defun node-type-known-p (node)
  (not (eq :unknown (node-type node))))

(defun node-type-unknown-p (node)
  (eq :unknown (node-type node)))

;; Parser phase
(make-node-variable
  :type :unknown
  :location loc
  :name 'x)

;; Typechecker phase
(make-node-variable
  :type (tc:qualify predicates inferred-type)
  :location loc
  :name 'x)
```

**Advantages:**

1. **Explicit unknowns** - More honest than nil
2. **Documents intent** - Clear that type hasn't been inferred yet
3. **Pattern matching friendly** - Can dispatch on :unknown
4. **Debugging friendly** - Can distinguish "not typed yet" from "type error"

**Implementation:**

```lisp
(defgeneric infer-expression-type (node expected-type subs env)
  ;; Pre-condition: (node-type-unknown-p node) or partially typed
  ;; Post-condition: (node-type-known-p result)
  (:method :before ((node node) expected-type subs env)
    ;; Could add runtime assertion here
    (assert (or (node-type-unknown-p node)
                (partially-typed-p node))))

  (:method :after ((node node) expected-type subs env result)
    ;; Verify we always produce fully typed nodes
    (assert (node-type-known-p result))))
```

**Validation:**

```lisp
(defun validate-untyped-tree (node)
  "Verify all nodes are :unknown before typechecking"
  (traverse-ast node
    (lambda (n)
      (assert (node-type-unknown-p n)))))

(defun validate-typed-tree (node)
  "Verify all nodes are typed after typechecking"
  (traverse-ast node
    (lambda (n)
      (assert (node-type-known-p n)))))
```

**This provides runtime guarantees equivalent to type-system guarantees from separate ASTs!**

## Recommended Path Forward

### Immediate (No Code Changes)

1. **Document current design** ✅ Already done
2. **Document transformations** ✅ Already done
3. **Add tests** - Verify transformation properties
4. **Investigate history** - Search git history, talk to original authors

### Short-term (Low Risk)

1. **Standardize field names**
   - Choose: `name` or `value` for identifiers
   - Choose: `body` or `subexpr` for nested expressions
   - Update all ASTs to match

2. **Add location to Codegen AST**
   - Better error messages
   - Easier debugging
   - Minimal cost

3. **Create transformation tests**
   - Type preservation
   - Location preservation
   - Semantics preservation

### Medium-term (Moderate Risk)

1. **Prototype unified syntax AST**
   - Combine Parser + Typechecker ASTs
   - Keep separate IR AST
   - Measure impact

2. **Use :unknown for types**
   - More explicit than nil
   - Better documentation
   - Runtime validation

3. **Generate ASTs from specification**
   - Macro-based generation
   - Single source of truth
   - Easier to maintain

### Long-term (High Risk)

1. **Refactor to two ASTs**
   - Syntax AST (unified Parser/Typechecker)
   - IR AST (Codegen)
   - Migrate incrementally

2. **Add phantom types**
   - Static guarantees without duplication
   - May not be idiomatic CL

3. **Consider alternative approaches**
   - Visitor pattern
   - Protocol-based transformations
   - Typed transformation framework

## Questions for Design Review

1. **Historical:**
   - Who designed the three-AST architecture?
   - What were the original requirements?
   - Has anyone tried alternatives?

2. **Technical:**
   - Are there performance benchmarks justifying separate ASTs?
   - What are the actual constraints?
   - Could we prototype a unified design?

3. **Practical:**
   - How often do AST changes occur?
   - What's the maintenance burden in practice?
   - Would refactoring provide measurable benefit?

## Conclusion

**The three-AST design appears to have NO rational basis:**

1. ❌ **Not for efficiency** - Structures are copied regardless
2. ❌ **Not from reference design** - THIH uses single AST
3. ❌ **Not from CL constraints** - Language supports unified approach
4. ❌ **Not well-documented** - No rationale exists
5. ⚠️ **Possibly historical** - Evolved without grand plan

**A typed-from-outset design with :unknown would be superior:**

1. ✅ Eliminates 800 lines of duplication
2. ✅ Single source of truth
3. ✅ Easier maintenance
4. ✅ Explicit about typing status
5. ✅ Runtime validation possible
6. ✅ No efficiency loss

**Recommendation:** Prototype a unified syntax AST with `:unknown` types to demonstrate feasibility, then plan incremental migration.
