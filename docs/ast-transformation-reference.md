# AST Transformation Quick Reference

This document provides quick reference tables for how AST nodes transform between compilation phases.

## Three ASTs

| Phase | Location | Type Info | Purpose |
|-------|----------|-----------|---------|
| **Parser** | `src/parser/expression.lisp` | None | Concrete syntax representation |
| **Typechecker** | `src/typechecker/expression.lisp` | `tc:qualified-ty` | Typed syntax with constraints |
| **Codegen** | `src/codegen/ast.lisp` | `tc:ty` | Low-level IR for code generation |

## Parser → Typechecker Transformations

### Direct Mappings (Structure Preserved)

Most nodes map directly with type information added:

| Parser Node | Typechecker Node | Type Added | Notes |
|-------------|------------------|------------|-------|
| `node-variable` | `node-variable` | Yes | Type inferred |
| `node-literal` | `node-literal` | Yes | Type from literal value |
| `node-integer-literal` | `node-integer-literal` | Yes | Polymorphic integer type |
| `node-abstraction` | `node-abstraction` | Yes | Function type inferred |
| `node-let` | `node-let` | Yes | Type of let body |
| `node-application` | `node-application` | Yes | Result type inferred |
| `node-match` | `node-match` | Yes | Branch types unified |
| `node-progn` | `node-progn` | Yes | Type of final expression |
| `node-return` | `node-return` | Yes | Return type |
| `node-throw` | `node-throw` | Yes | Exception type |
| `node-or` | `node-or` | Yes | Boolean type |
| `node-and` | `node-and` | Yes | Boolean type |
| `node-if` | `node-if` | Yes | Branch types unified |
| `node-when` | `node-when` | Yes | Unit type |
| `node-unless` | `node-unless` | Yes | Unit type |
| `node-cond` | `node-cond` | Yes | Branch types unified |
| `node-do` | `node-do` | Yes | Monadic type |
| `node-while` | `node-while` | Yes | Unit type |
| `node-while-let` | `node-while-let` | Yes | Unit type |
| `node-loop` | `node-loop` | Yes | Unit type (or diverges) |
| `node-for` | `node-for` | Yes | Unit type |
| `node-break` | `node-break` | Yes | Bottom type |
| `node-continue` | `node-continue` | Yes | Bottom type |

### Nodes Removed During Typechecking

| Parser Node | Transformation | Where Type Info Goes |
|-------------|----------------|---------------------|
| `node-the` | **REMOVED** | Type applied to wrapped expression |
| `node-let-declare` | **REMOVED** | Type constraint added to environment |

**Example: `node-the` transformation**
```
Parser:     (the Int (+ 1 2))
Typechecker: (node-application type: Int ...)
```

The type annotation is consumed and used to constrain type inference, but the `node-the` wrapper is removed.

**Example: `node-let-declare` transformation**
```
Parser:     (let ((declare x Int)
                  (x (+ 1 2)))
              x)
Typechecker: (node-let
               bindings: [(x type: Int ...)]
               body: (node-variable x type: Int))
```

The `declare` becomes a type constraint on the binding.

## Typechecker → Codegen Transformations

### Direct Mappings (Structure Preserved)

| Typechecker Node | Codegen Node | Changes |
|------------------|--------------|---------|
| `node-variable` | `node-variable` | Field `name` → `value` |
| `node-literal` | `node-literal` | Same |
| `node-abstraction` | `node-abstraction` | Field `body` → `subexpr` |
| `node-let` | `node-let` | Field `body` → `subexpr` |
| `node-match` | `node-match` | Same |
| `node-lisp` | `node-lisp` | Field `body` → `form` |
| `node-progn` | `node-seq` | Renamed |
| `node-return` | `node-return-from` | Different structure |
| `node-throw` | `node-throw` | Same |
| `node-while` | `node-while` | Same |
| `node-while-let` | `node-while-let` | Same |
| `node-loop` | `node-loop` | Same |
| `node-for` | *desugared* | Becomes while-let + iterator |
| `node-break` | `node-break` | Same |
| `node-continue` | `node-continue` | Same |

### Application Nodes: Two Variants

| Typechecker Node | Codegen Node | When Used |
|------------------|--------------|-----------|
| `node-application` | `node-application` | Unapplied or unknown function |
| `node-application` | `node-direct-application` | Fully saturated, known function |

**Direct application** is an optimization for when:
1. The function being called is known at compile time (not a variable function)
2. The function is fully saturated (all arguments provided)
3. The function type is monomorphic (after specialization)

### Nodes Removed (Desugared)

| Typechecker Node | Codegen Equivalent | Desugaring Rule |
|------------------|-------------------|-----------------|
| `node-or` | `node-match` | `(or a b)` → `(match a ((True) True) ((False) b))` |
| `node-and` | `node-match` | `(and a b)` → `(match a ((True) b) ((False) False))` |
| `node-if` | `node-match` | `(if c t e)` → `(match c ((True) t) ((False) e))` |
| `node-when` | `node-match` | `(when c b)` → `(match c ((True) b) ((False) Unit))` |
| `node-unless` | `node-match` | `(unless c b)` → `(match c ((False) b) ((True) Unit))` |
| `node-cond` | `node-match` | Nested matches (see below) |
| `node-do` | `node-bind` chain | Do notation desugared (see below) |

**Cond desugaring:**
```
(cond
  (c1 b1)
  (c2 b2)
  (c3 b3))

→

(match c1
  ((True) b1)
  ((False) (match c2
             ((True) b2)
             ((False) (match c3
                        ((True) b3)
                        ((False) Unit))))))
```

**Do notation desugaring:**
```
(do
  (x <- m1)
  (y <- m2)
  (return (+ x y)))

→

(bind m1 (fn (x)
  (bind m2 (fn (y)
    (return (+ x y))))))
```

### New Nodes in Codegen AST

These nodes only exist in the Codegen AST:

| Codegen Node | Purpose | Created From |
|--------------|---------|--------------|
| `node-direct-application` | Optimized function call | `node-application` when function is known |
| `node-field` | Typeclass dictionary field access | Instance resolution |
| `node-seq` | Sequential statements | `node-progn` |
| `node-block` | Named return target | Added for explicit returns |
| `node-return-from` | Return from block | `node-return` |
| `node-bind` | Non-recursive single binding | Various sources |
| `node-dynamic-extent` | Stack-allocated binding | Optimization |
| `node-locally` | Local declarations | Optimizer |

## Type Information Changes

### Parser AST (No Types)
```lisp
(defstruct (node ...)
  (location :type source:location))
```

### Typechecker AST (Qualified Types)
```lisp
(defstruct (node ...)
  (type     :type tc:qualified-ty)
  (location :type source:location))

;; qualified-ty includes:
;; - base type (tc:ty)
;; - predicates (type class constraints)
```

### Codegen AST (Simple Types)
```lisp
(defstruct (node ...)
  (type :type tc:ty))

;; No location!
;; No predicates (resolved to dictionary parameters)
```

## Field Name Changes

**Inconsistency warning:** Field names change between ASTs!

### node-variable
- Parser: `name` field
- Typechecker: `name` field
- Codegen: `value` field

### node-abstraction
- Parser: `body` field (type: `node-body`)
- Typechecker: `body` field (type: `node-body`)
- Codegen: `subexpr` field (type: `node`)

### node-let
- Parser: `body` field (type: `node-body`)
- Typechecker: `body` field (type: `node-body`)
- Codegen: `subexpr` field (type: `node`)

### node-lisp
- Parser: `body` field
- Typechecker: `body` field
- Codegen: `form` field

## Location Information

**Critical difference:**

- **Parser AST:** Has `location` field on all nodes
- **Typechecker AST:** Has `location` field on all nodes
- **Codegen AST:** **NO** `location` field!

**Impact:** Error messages and debugging information from codegen/optimization phases cannot reference source locations directly.

## Pattern Matching Nodes

### Match Branches

All three ASTs have similar match branch structures:

**Parser:**
```lisp
(defstruct node-match-branch
  (pattern  :type pattern)
  (body     :type node-body)
  (location :type source:location))
```

**Typechecker:**
```lisp
(defstruct node-match-branch
  (pattern  :type pattern)
  (body     :type node-body)
  (location :type source:location))
```

**Codegen:**
```lisp
(defstruct match-branch  ; Different name!
  (pattern :type pattern)
  (body    :type node))  ; Not node-body!
```

Note: Codegen uses `node` for body, not `node-body`.

## Exception Handling Nodes

### Catch Branches

Similar structure across all ASTs:

- `node-catch` - Main catch expression
- `node-catch-branch` - Individual catch handler
- `node-throw` - Throw exception

### Resumable Branches

For resumable exceptions (like Common Lisp's restarts):

- `node-resumable` - Resumable expression
- `node-resumable-branch` - Resumption handler
- `node-resume-to` - Resume to handler

## Loop Constructs

All loop nodes mostly preserved across phases:

- `node-while` - While loop
- `node-while-let` - While with pattern match
- `node-loop` - Infinite loop
- `node-for` - For loop (desugared in codegen)
- `node-break` - Break statement
- `node-continue` - Continue statement

**Labels:** All loops have keyword labels for break/continue.

## Transformation Code Locations

Where to find transformation implementations:

| Transformation | File | Function |
|----------------|------|----------|
| Parser → Typechecker | `src/typechecker/define.lisp` | `infer-expression-type` |
| Typechecker → Codegen | `src/codegen/translate-expression.lisp` | `translate-expression` |
| Codegen → Lisp | `src/codegen/codegen-expression.lisp` | `codegen-expression` |

## Common Transformation Patterns

### Adding Type Information

Most Parser → Typechecker transformations follow this pattern:

```lisp
;; Parser node (no type)
(make-node-foo
  :location loc
  :field1 val1
  :field2 val2)

;; Typechecker node (with type)
(make-node-foo
  :type inferred-type
  :location loc
  :field1 (infer val1)
  :field2 (infer val2))
```

### Simplifying Type Information

Typechecker → Codegen removes predicates:

```lisp
;; Typechecker
;; type: (qualified-ty
;;         predicates: [(Num a)]
;;         type: (a -> a))

;; Codegen
;; type: (a -> a)
;; Extra parameter added: dict-num
```

### Desugaring Control Flow

High-level constructs → match expressions:

```lisp
;; Typechecker
(node-if
  expr: condition
  then: true-branch
  else: false-branch)

;; Codegen
(node-match
  expr: condition
  branches: [
    (match-branch
      pattern: (pattern-constructor 'True)
      body: true-branch)
    (match-branch
      pattern: (pattern-constructor 'False)
      body: false-branch)])
```

## Validation Checklist

When implementing a transformation, verify:

- [ ] Type information preserved or correctly transformed
- [ ] Source location preserved (if applicable)
- [ ] Semantics preserved
- [ ] All fields properly transformed
- [ ] Pattern matching exhaustive
- [ ] Error cases handled

## Related Files

- Parser AST: `src/parser/expression.lisp`
- Typechecker AST: `src/typechecker/expression.lisp`
- Codegen AST: `src/codegen/ast.lisp`
- Parser logic: `src/parser/*.lisp`
- Typechecker logic: `src/typechecker/define.lisp`
- Translation logic: `src/codegen/translate-expression.lisp`
- Codegen logic: `src/codegen/codegen-expression.lisp`

## Notes and Gotchas

1. **Field names are inconsistent** - `name` vs `value`, `body` vs `subexpr`
2. **No location in Codegen AST** - Can't generate good errors from backend
3. **node-the removed early** - Type annotations consumed during typechecking
4. **Control flow desugared** - Most control flow becomes `match` in codegen
5. **Two application types** - Direct vs indirect application in codegen
6. **Qualified vs simple types** - Predicates removed during codegen translation
