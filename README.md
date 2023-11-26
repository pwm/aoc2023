## (ﾉ ◕ ヮ ◕)ﾉ\*:･ﾟ ✧ [Advent of Code 2023](https://adventofcode.com/2023) (◕‿◕✿)

## Setup

We'll need 2 env vars:

1. `AOC_SESSION` will hold your personal AoC session cookie
2. `AOC_INPUT_PATH` is the path to the `input` dir within this repo, eg. `~/code/aoc2023/input`

```
echo 'export AOC_SESSION=<my-aoc-session>' >> .envrc.private
echo 'export AOC_INPUT_PATH=<my-aoc-input-path>' >> .envrc.private
direnv allow .
```

## Use

The project is using [Nixkell](https://github.com/pwm/nixkell)

### Creating day template

Use `mkday.sh` to fetch the input file and generate a template module and spec for it.

```
scripts/mkday.sh # current date
scripts/mkday.sh 01 # current year, specific day
scripts/mkday.sh 2023 01 # specific date
```

### Compile and Run

```
scripts/comp.sh
scripts/solve.sh # current date
scripts/solve.sh 01 # specific day
scripts/solve.sh 2023 01 # specific date
```

Notes:

- `optimization` in `cabal.project` is set to `0` for fast dev UX. Setting it to `2` can significantly speed up solvers
- For profiling flip the relevant flags in `nixkell.toml` (for GHC profiling overrides) and in `cabal.project` (for profiling builds)

### Testing

```
scripts/test.sh
```

### Formatting, Linting, etc...

```
treefmt
hlint .
scripts/shellcheck.sh
```

### Nix

```
build
aoc fetch --year 2023 --day 01
aoc solve --year 2023 --day 01
```

### CI with Cachix

Configure `CACHIX_SIGNING_KEY` as a repository secret (see: https://nix.dev/tutorials/continuous-integration-github-actions)
