# Static Analyzer of Error Propagation for Rust

By Grace Stok, building on a previous version by Thomas Kas

## Usage

First, ensure rustup is installed, any version more recent that 1.23.0. Rustup
will automatically install the required toolchain components. Because of its
usage of internal Rustc features, the program could not be compiled to an
executable. It can instead be run using cargo, using the following command:

`cargo run -r -- <MANIFEST> [GRAPH_TYPES]`

The `<MANIFEST>` parameter should be a path to the Cargo.toml file of the crate
to be analysed. The `[GRAPH_TYPES]` parameter is a list of graph types to
output, possible values are:

- `call`, this outputs a callgraph of the project
- `error-chain`, this outputs an Result-error graph of the project, currently
  broken
- `panic-chain`, this outputs a panic propagation graph of the project

Regardless of passed parameters, statistics for the given project will be
printed to stdout, however, the Result-error analysis statistics are currently
broken.

A help command is also available, by passing `--help` or `-h` after the `--`.
