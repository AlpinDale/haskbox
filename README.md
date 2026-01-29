# haskbox

Unix utilities in Haskell (busybox-style).

## Build

```bash
cabal build
```

## Install

```bash
cabal install --installdir=$HOME/.local/bin
```

## Usage

```bash
haskbox <command> [args...]
```

### Available commands

| Command | Description |
|---------|-------------|
| cat | Concatenate and print files |

Run `haskbox <command> --help` for command-specific help.

The rest of the commands will be added as they are implemented.

## Benchmarks

macOS, Apple M4 Pro (cat command):

| Test | haskbox | gcat | Ratio |
|------|---------|------|-------|
| Empty file | 16.1ms | 1.9ms | 8.5x |
| 100MB file | 17.7ms | 8.3ms | 2.1x |

The performance gap is due to GHC's runtime system initialization (~15ms overhead). This is fundamental to how GHC works; it must set up the garbage collector, scheduler, and I/O subsystem before any Haskell code runs. GNU cat, written in C, has no such runtime. Once running, throughput is comparable.
