# spice
A tool to find and manage duplicate files

## How to build it
Install [OPAM](https://opam.ocaml.org/doc/Install.html), on Debian run:
```bash
# apt-get install opam
```
Then install dune and Jane Street's base libraries via OPAM by running:
```bash
$ opam install dune base stdio
```
Clone or download this repository, and build by running
```bash
$ dune build src/spice.exe
```
