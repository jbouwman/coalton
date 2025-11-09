# Coalton Compiler Internals

This document describes the internal architecture of the Coalton compiler, including its multi-stage compilation pipeline, data structures, and transformation phases.

## Table of Contents

1. [Overview](#overview)
2. [Compilation Pipeline](#compilation-pipeline)
3. [The Three AST Problem](#the-three-ast-problem)
4. [Parser Phase](#parser-phase)
5. [Typechecker Phase](#typechecker-phase)
6. [Analysis Phase](#analysis-phase)
7. [Codegen Phase](#codegen-phase)
8. [Type System](#type-system)
9. [Known Issues and Design Concerns](#known-issues-and-design-concerns)

## Overview

The Coalton compiler is a multi-stage compiler written in Common Lisp that transforms Coalton source code into optimized Common Lisp code. The compilation process involves:

- **Input**: Coalton source code
- **Output**: Common Lisp code
- **Intermediate representations**: Three distinct AST representations
- **Major phases**: Parsing, Typechecking, Analysis, Code Generation

## Compilation Pipeline

```
Coalton Source Code
        ↓
    [PARSER]
        ↓
   Parser AST (untyped)
        ↓
  [TYPECHECKER]
        ↓
Typechecker AST (typed with qualified types)
        ↓
   [ANALYSIS]
        ↓
(Same Typechecker AST, validated)
        ↓
   [CODEGEN]
        ↓
 Codegen AST (Lisp IR)
        ↓
[OPTIMIZATION]
        ↓
 Common Lisp Code
```

**Main entry point:** `src/entry.lisp:entry-point`

## The Three AST Problem

**CRITICAL ISSUE**: The compiler maintains **three separate AST representations**, each defined independently with significant duplication.

### 1. Parser AST (Pre-typechecking)

**Location:** `src/parser/expression.lisp` (1784 lines, ~67KB)

**Purpose:** Represents Coalton source code structure without type information.

**Key characteristics:**
- No type information attached to nodes
- Direct mapping to Coalton surface syntax
- Includes `node-the` for explicit type annotations
- Includes `node-let-declare` for type declarations in let bindings
- All control flow constructs present

**Example nodes:**
```lisp
(defstruct (node
  (:constructor nil))
  (location :type source:location))

(defstruct (node-variable (:include node))
  (name :type identifier))

(defstruct (node-application (:include node))
  (rator :type node)
  (rands :type node-list))
```

### 2. Typechecker AST (Post-typechecking)

**Location:** `src/typechecker/expression.lisp` (812 lines, ~32KB)

**Purpose:** Represents fully type-checked Coalton code with qualified type information.

**Key characteristics:**
- **File comment explicitly states**: "Mirror of expression nodes in src/parser/expression.lisp with types attached" (line 2-4)
- Every node includes `(type :type tc:qualified-ty)` field
- **`node-the` does not exist** (removed during typechecking, comment at line 319)
- **`node-let-declare` does not exist** (declarations consumed during typechecking)
- Otherwise structurally identical to Parser AST
- Includes methods for `tc:apply-substitution` (type unification)

**Example nodes:**
```lisp
(defstruct (node
  (:constructor nil))
  (type     :type tc:qualified-ty)
  (location :type source:location))

(defstruct (node-variable (:include node))
  (name :type parser:identifier))

(defstruct (node-application (:include node))
  (rator :type node)
  (rands :type node-list))
```

**Qualified types** include:
- The actual type (`tc:ty`)
- Type class predicates/constraints
- Quantified type variables

### 3. Codegen AST (Lisp IR)

**Location:** `src/codegen/ast.lisp` (435 lines, ~18KB)

**Purpose:** Intermediate representation optimized for code generation and translation to Common Lisp.

**Key characteristics:**
- **File comment**: "Compiler Backend IR" (line 150)
- Types are simpler: `tc:ty` instead of `tc:qualified-ty`
- **Significantly different structure** from Parser/Typechecker ASTs
- Many high-level nodes removed (desugared)
- New nodes for low-level constructs

**Nodes that exist ONLY in Codegen AST:**
- `node-direct-application` - Fully saturated function calls to known functions
- `node-field` - Typeclass dictionary field access
- `node-seq` - Sequential statement execution
- `node-block` / `node-return-from` - Explicit return targets
- `node-bind` - Non-recursive single binding (different from parser version)
- `node-dynamic-extent` - Stack-allocated bindings

**Nodes that DISAPPEAR in Codegen AST** (present in Parser/Typechecker):
- `node-the` - Type annotations (already consumed)
- `node-or` - Desugared to if/match
- `node-and` - Desugared to if/match
- `node-if` - Desugared to match
- `node-when` - Desugared to match
- `node-unless` - Desugared to match
- `node-cond` - Desugared to match
- `node-do` - Desugared to bind operations

**Example nodes:**
```lisp
(defstruct (node (:conc-name %node-))
  (type :type tc:ty))

(defstruct (node-variable (:include node))
  (value :type parser:identifier))

(defstruct (node-direct-application (:include node))
  (properties :type list)
  (rator-type :type tc:ty)
  (rator      :type parser:identifier)  ; Known at compile time!
  (rands      :type node-list))
```

### AST Comparison Table

| Node Type | Parser AST | Typechecker AST | Codegen AST | Notes |
|-----------|------------|-----------------|-------------|-------|
| `node-variable` | ✓ | ✓ | ✓ | Name vs value field |
| `node-literal` | ✓ | ✓ | ✓ | Present in all |
| `node-application` | ✓ | ✓ | ✓ | Unapplied calls |
| `node-direct-application` | ✗ | ✗ | ✓ | Applied, known function |
| `node-the` | ✓ | ✗ | ✗ | Removed after parsing |
| `node-let-declare` | ✓ | ✗ | ✗ | Removed after parsing |
| `node-or` | ✓ | ✓ | ✗ | Desugared in codegen |
| `node-and` | ✓ | ✓ | ✗ | Desugared in codegen |
| `node-if` | ✓ | ✓ | ✗ | Desugared in codegen |
| `node-when` | ✓ | ✓ | ✗ | Desugared in codegen |
| `node-unless` | ✓ | ✓ | ✗ | Desugared in codegen |
| `node-cond` | ✓ | ✓ | ✗ | Desugared in codegen |
| `node-do` | ✓ | ✓ | ✗ | Desugared in codegen |
| `node-field` | ✗ | ✗ | ✓ | Dictionary access |
| `node-seq` | ✗ | ✗ | ✓ | Statement sequencing |
| `node-block` | ✗ | ✗ | ✓ | Return targets |

## Parser Phase

**Location:** `src/parser/` (~250 KB across 13 files)

**Entry point:** `src/parser/toplevel.lisp:parse-form`

**Key files:**
- `toplevel.lisp` (90KB) - Parse top-level definitions
- `expression.lisp` (67KB) - Parse expressions → Parser AST
- `types.lisp` (13KB) - Parse type signatures
- `pattern.lisp` (7.9KB) - Parse patterns
- `renamer.lisp` (29KB) - Variable name resolution
- `collect.lisp` (12KB) - Collect definitions and dependencies

**Input:** Concrete syntax trees (CST) from `concrete-syntax-tree` library

**Output:** `parser:program` containing:
- Type definitions
- Class definitions
- Instance definitions
- Value definitions
- All as Parser AST nodes

**Process:**
1. Tokenize and parse Coalton forms using CST
2. Build Parser AST nodes (no types)
3. Perform variable renaming (hygiene)
4. Collect and organize definitions

**Parser AST EBNF** (from `src/parser/expression.lisp:200-288`):

```ebnf
expression := node-variable
            | node-accessor
            | node-literal
            | node-abstraction
            | node-let
            | node-lisp
            | node-match
            | node-progn
            | node-the
            | node-return
            | node-application
            | node-or
            | node-and
            | node-if
            | node-when
            | node-unless
            | node-cond
            | node-do
            | node-while
            | node-while-let
            | node-loop
            | node-for
            | node-break
            | node-continue

node-abstraction := "(" "fn" "(" pattern* ")" body ")"
node-let := "(" "let" "(" (binding | declare)+ ")" body ")"
node-match := "(" "match" expression branch+ ")"
node-the := "(" "the" type expression ")"
```

## Typechecker Phase

**Location:** `src/typechecker/` (~500 KB across 30 files)

**Entry point:** `src/typechecker/define.lisp:infer-expression-type`

**Key files:**
- `define.lisp` (115KB) - **Expression type inference** (LARGEST FILE)
- `environment.lisp` (79KB) - Type environment and symbol resolution
- `expression.lisp` (32KB) - Typechecker AST definitions
- `types.lisp` (20KB) - Type representation
- `define-instance.lisp` (21KB) - Instance typechecking
- `define-class.lisp` (22KB) - Class typechecking
- `define-type.lisp` (28KB) - Type definition typechecking
- `unify.lisp` - Type unification
- `substitutions.lisp` - Type variable substitutions
- `context-reduction.lisp` (15KB) - Constraint simplification

**Input:** Parser AST (untyped)

**Output:** Typechecker AST (typed with qualified types) + `translation-unit`

**Type representation** (`src/typechecker/types.lisp`):
```
ty (abstract type)
  ├─ tyvar (type variables, e.g., :A, :B)
  ├─ tycon (type constructors, e.g., List, Maybe)
  ├─ tapp (type applications, e.g., (List Int))
  └─ tgen (generated types)

qualified-ty = { ty: ty, predicates: [predicate] }
```

**Process:**
1. Traverse Parser AST
2. Generate fresh type variables
3. Collect type constraints
4. Unify types
5. Solve constraints (context reduction)
6. Apply substitutions
7. Build Typechecker AST with types attached
8. Create `translation-unit`

**Translation Unit** (`src/typechecker/translation-unit.lisp`):
```lisp
(defstruct translation-unit
  types        ; Type definitions
  definitions  ; Toplevel value definitions (Typechecker AST)
  instances    ; Type class instances
  lisp-forms   ; Embedded Lisp forms
  classes      ; Type class definitions
  package)     ; Target Lisp package
```

**Transformations during typechecking:**
- `node-the` → type information attached to wrapped expression, node removed
- `node-let-declare` → type constraints added, node removed
- Type inference assigns types to all nodes
- Type class constraints become predicates in qualified types

## Analysis Phase

**Location:** `src/analysis/` (~35 KB across 5 files)

**Entry point:** `src/analysis/analysis.lisp:analyze-translation-unit`

**Key files:**
- `pattern-exhaustiveness.lisp` (15KB) - Check match coverage
- `unused-variables.lisp` - Find unused variable warnings
- `underapplied-values.lisp` - Detect partial applications
- `analysis.lisp` - Main driver

**Input:** `translation-unit` with Typechecker AST

**Output:** Same `translation-unit`, but with validation and warnings

**Checks performed:**
- **Pattern exhaustiveness**: Ensure all cases in `match` are covered
- **Unused variables**: Warn about unused bindings
- **Underapplied values**: Detect functions called with too few arguments
- **Pattern variable warnings**: Match against constructor names

**Note:** This phase does NOT transform the AST; it only validates and produces warnings.

## Codegen Phase

**Location:** `src/codegen/` (~275 KB across 25 files)

**Entry point:** `src/codegen/program.lisp:compile-translation-unit`

**Key files:**

**Translation:**
- `translate-expression.lisp` (45KB) - **Typechecker AST → Codegen AST**
- `ast.lisp` (18KB) - Codegen AST definitions
- `traverse.lisp` (24KB) - AST traversal

**Code generation:**
- `codegen-expression.lisp` (25KB) - Codegen AST → Lisp code
- `codegen-type-definition.lisp` (8KB) - Compile type definitions
- `codegen-class.lisp` (4KB) - Compile type classes
- `translate-instance.lisp` (4KB) - Compile instances

**Optimizations:**
- `monomorphize.lisp` (19KB) - **Specialize polymorphic functions**
- `optimizer.lisp` (22KB) - General optimizations
- `constant-propagation.lisp` (8KB) - Constant folding
- `inliner.lisp` (16KB) - Function inlining
- `hoister.lisp` (4KB) - Loop-invariant code motion
- `specializer.lisp` (3KB) - Function specialization

**Utilities:**
- `program.lisp` (12KB) - Compile translation units
- `resolve-instance.lisp` (7KB) - Instance dictionary resolution
- `typecheck-node.lisp` (8KB) - Additional typechecking for Lisp interop

**Process:**

1. **Translation** (`translate-expression.lisp`):
   - Typechecker AST → Codegen AST
   - Desugar high-level constructs:
     - `node-or` → `node-match` with boolean patterns
     - `node-and` → `node-match` with boolean patterns
     - `node-if` → `node-match` with boolean patterns
     - `node-when` → `node-match`
     - `node-unless` → `node-match`
     - `node-cond` → nested `node-match`
     - `node-do` → chain of `node-bind`
   - Qualified types → simple types (constraints already resolved)

2. **Instance Resolution** (`resolve-instance.lisp`):
   - Type class constraints → dictionary passing
   - Build instance dictionaries
   - Insert dictionary parameters

3. **Optimization**:
   - **Monomorphization**: Specialize polymorphic functions to concrete types
   - **Inlining**: Inline small functions
   - **Constant propagation**: Fold constant expressions
   - **Hoisting**: Move loop-invariant code out of loops

4. **Code Generation** (`codegen-expression.lisp`):
   - Codegen AST → Lisp forms
   - Generate optimized Common Lisp code

**Example transformation:**

```coalton
;; Coalton source
(define (map f xs)
  (match xs
    ((Cons x rest) (Cons (f x) (map f rest)))
    (Nil Nil)))
```

**Parser AST:**
```lisp
(node-abstraction
  params: [(pattern-var 'f) (pattern-var 'xs)]
  body: (node-match
          expr: (node-variable 'xs)
          branches: [...]))
```

**Typechecker AST:**
```lisp
(node-abstraction
  type: (qualified-ty
          ty: (a -> List b -> List c)
          predicates: [(Functor f)])
  params: [(pattern-var 'f type: (a -> b))
           (pattern-var 'xs type: (List a))]
  body: (node-match
          type: (List b)
          expr: (node-variable 'xs type: (List a))
          branches: [...]))
```

**Codegen AST** (simplified):
```lisp
(node-abstraction
  type: (a -> List b -> List c)
  vars: ['f 'xs]
  subexpr: (node-match
            type: (List b)
            expr: (node-variable 'xs)
            branches: [...]))
```

**Generated Lisp** (simplified):
```lisp
(defun map (f xs)
  (match-value xs
    ((Cons x rest) (Cons (funcall f x) (map f rest)))
    (Nil Nil)))
```

## Type System

**Core type representation** (`src/typechecker/types.lisp`):

```lisp
;; Base type
(deftype ty () ...)

;; Type variables (e.g., :A, :B)
(defstruct tyvar
  id      ; Unique ID
  kind)   ; Kind (*, * -> *, etc.)

;; Type constructors (e.g., Int, List, Maybe)
(defstruct tycon
  name    ; Constructor name
  kind)   ; Kind

;; Type applications (e.g., (List Int), (Maybe String))
(defstruct tapp
  from    ; Function type
  to)     ; Argument type

;; Generated types
(defstruct tgen
  id)
```

**Qualified types** (`src/typechecker/scheme.lisp`):
```lisp
(defstruct qualified-ty
  predicates   ; Type class constraints
  type)        ; Actual type

;; Example: (Num a, Show a) => a -> String
;; predicates: [(Num :A) (Show :A)]
;; type: (:A -> String)
```

**Type schemes** (polymorphic types):
```lisp
(defstruct ty-scheme
  kinds        ; Kind constraints
  predicates   ; Type class constraints
  type)        ; Polymorphic type

;; Example: ∀a. Num a => a -> a -> a
;; kinds: [:A => *]
;; predicates: [(Num :A)]
;; type: (:A -> :A -> :A)
```

**Kinds** (types of types):
```
* - Type of ordinary types (Int, String, etc.)
* -> * - Type of type constructors (List, Maybe, etc.)
* -> * -> * - Type of binary type constructors ((,), Either, etc.)
```

## Known Issues and Design Concerns

### 1. **Massive AST Duplication**

**Problem:** Parser AST and Typechecker AST are nearly identical, with only type information added.

**Evidence:**
- Comment in `src/typechecker/expression.lisp:2-4`: "Mirror of expression nodes in src/parser/expression.lisp with types attached"
- 812 lines duplicated with minimal changes

**Impact:**
- Maintenance burden: Changes to AST structure require updates in multiple places
- Bug risk: Easy to get ASTs out of sync
- Confusion: Not clear why duplication is necessary

**Potential solutions:**
- Parameterize AST by type information (e.g., `Node<TypeInfo>`)
- Use single AST with optional type fields
- Use Lisp macros to generate both ASTs from single definition

### 2. **Undocumented Node Transformations**

**Problem:** Nodes disappear between phases with no clear documentation of when/why.

**Examples:**
- `node-the` removed after parsing (comment in typechecker AST)
- `node-let-declare` removed after parsing
- All control flow nodes removed during codegen translation

**Impact:**
- Hard to understand compilation pipeline
- Difficult to debug transformation issues
- No single source of truth for transformations

**What should be documented:**
- Exact mapping from Typechecker AST → Codegen AST
- Desugaring rules for each construct
- When and why each node is removed

### 3. **Type Representation Changes**

**Problem:** Type representation changes between Typechecker and Codegen ASTs.

**Details:**
- Typechecker AST: `tc:qualified-ty` (includes predicates)
- Codegen AST: `tc:ty` (no predicates)

**Why this happens:**
- Type class constraints resolved during codegen
- Predicates become dictionary parameters
- No longer need qualified types in backend

**Impact:**
- Not immediately obvious why types differ
- Could cause confusion when debugging
- Requires understanding of dictionary passing transformation

### 4. **No Shared Abstractions**

**Problem:** Each AST is completely separate with no shared code.

**Impact:**
- Traversal code duplicated for each AST
- Transformation utilities duplicated
- No type-safe guarantees about transformations

**Potential solutions:**
- Visitor pattern for AST traversal
- Shared base types/traits
- Typed transformation framework

### 5. **Entry Point Complexity**

**Problem:** Compilation entry point (`src/entry.lisp:entry-point`) orchestrates many steps without clear abstraction boundaries.

**Current flow:**
1. Rename variables
2. Process type definitions
3. Process class definitions
4. Process instance definitions
5. Process value definitions
6. Typecheck instances
7. Specialize generics
8. Create translation unit
9. Analyze translation unit
10. Generate code

**Issues:**
- No clear phase boundaries
- Error recovery unclear
- Difficult to test individual phases
- Hard to add new phases

### 6. **Codegen AST Needs Documentation**

**Problem:** Codegen AST is significantly different from Parser/Typechecker ASTs but poorly documented.

**Missing documentation:**
- Why `node-direct-application` vs `node-application`?
- When are functions "known" vs "unknown"?
- What are "properties" for in application nodes?
- How does `node-field` work for typeclass dictionaries?
- When is `node-dynamic-extent` used?

### 7. **Translation Unit is Central but Opaque**

**Problem:** `translation-unit` is the central data structure but its role is not well documented.

**Questions:**
- When is it created?
- What invariants does it maintain?
- How do different phases modify it?
- What guarantees does it provide?

## Recommendations

### High Priority

1. **Document AST transformations**
   - Create explicit mapping tables for each phase
   - Document desugaring rules
   - Add examples of transformations

2. **Add architecture diagrams**
   - Data flow diagrams
   - Phase interaction diagrams
   - Type flow diagrams

3. **Document design rationale**
   - Why three ASTs?
   - Why these specific representations?
   - What alternatives were considered?

### Medium Priority

4. **Improve code organization**
   - Clear phase boundaries
   - Consistent naming conventions
   - Better module separation

5. **Add validation**
   - AST invariant checking
   - Phase boundary validation
   - Type preservation checks

### Low Priority

6. **Consider refactoring**
   - Reduce AST duplication
   - Shared traversal utilities
   - Typed transformation framework

7. **Performance documentation**
   - Complexity analysis
   - Profiling results
   - Optimization strategies

## Related Documentation

- [How Typeclasses Are Compiled](how-typeclasses-are-compiled.md) - Dictionary passing implementation
- [Type Class Design](design-docs/typeclasses.md) - Type class design decisions
- Source code comments in:
  - `src/entry.lisp` - Main compilation entry point
  - `src/parser/expression.lisp` - Parser AST EBNF grammar
  - `src/typechecker/expression.lisp` - Typechecker AST notes
  - `src/codegen/ast.lisp` - Codegen IR notes
