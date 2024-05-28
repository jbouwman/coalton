# The Coalton `(package ...)` Form

In Coalton, the `package` form is used to define a package by
specifying which dependencies to import, and which definitions to
export.

The form also establishes the current package: any definitions that
follow `(package <name>)` will be contained within that package.

## Defining a Package

A package in Coalton is defined using the `package` form. The basic
syntax is:

``` coalton
(package <package-name>
  (import <other-package-name>
          (<other-package-name2 as <other2>)
          ...)
  (import-from <other-package-name>
          <symbol1>
          <symbol2>
          ...)
  (export <symbol3>
          <symbol4>
          ...))
```

Here's an example:

``` coalton
(package my-package
  (import coalton-library/list)
  (export sum-even-numbers))
```

## Importing Dependencies

You can import dependencies for your package using the `import` and
`import-from` forms.

The `import` form allows you to specify which packages your package
depends on. You can also rename imported packages using the `as`
keyword, as shown in the example below:

``` coalton
(package my-package
  (import coalton-library/list as list)
  (export sum-even-numbers))
```

The `import-from` form allows you to specify which symbols from an
imported package should be made available within your package:

``` coalton
(package my-package
  (import-from coalton-library/list
    filter)
  (export sum-even-numbers))
```

## Exporting Definitions

You can export definitions from your package using the `export`
form. This allows other packages to use to these definitions.

For example, let's say you have a function `sum-even-numbers` that you
want to make available for use by other packages:

``` coalton
(package my-package
  (import coalton-library/list)
  (export sum-even-numbers))
```

In this case, the `sum-even-numbers` function will be exported from
your package and can be used by other packages that import your
package.

## Lisp Interoperability

Packages defined by package are fundamentally structurally compatible
with Lisp packages, as the import, import-from, and export forms
evaluate unambiguously to Lisp defpackage counterparts `use`,
`local-nicknames`, and `export`.

- Any given Coalton package can be imported into any Lisp package.
- Any given Lisp package be imported into any Coalton package.
- The name of a coalton-defined package be the same whether importing into Coalton or Lisp.
- Coalton packages are compatible with utilities such as uiop:define-package's `:mix-reexport` clause.
