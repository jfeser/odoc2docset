# odoc2docset [![Build Status](https://travis-ci.org/jfeser/odoc2docset.png?branch=master)](https://travis-ci.org/jfeser/odoc2docset)


Converts [Odoc](https://github.com/ocaml-doc/odoc) generated documentation into
[Dash](https://kapeli.com/dash) (or [Zeal](https://zealdocs.org/)) docsets.

## Installation

```
opam pin add odoc2docset https://github.com/jfeser/odoc2docset.git
opam install odoc2docset
```

## Usage

To generate documentation for all installed packages, run:

```
odoc2docset Opam.docset
```

To generate documentation for a subset of packages, run:

```
odoc2docset Opam.docset pkgA pkgB pkgC
```

### Compression

By default, `odoc2docset` will create uncompressed docsets. Compressed docsets
can be created using the `--compress` option. This mode requires
[tarix](https://github.com/fastcat/tarix/) and GNU tar.

## Known bugs

Running this tool to update an existing docset while Dash is running can fail,
because the Sqlite database does not support concurrent accesses. If the tool
crashes with a Sqlite BUSY exception, quit Dash and try again.
