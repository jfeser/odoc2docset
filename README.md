# odoc2docset

Converts [Odoc](https://github.com/ocaml-doc/odoc) generated documentation into [Dash](https://kapeli.com/dash) (or [Zeal](https://zealdocs.org/)) docsets.

## Usage

To generate documentation for all installed packages, run:

```
odoc2docset Opam.docset
```

To generate documentation for a subset of packages, run:

```
odoc2docset Opam.docset pkgA pkgB pkgC
```
