## Local development

### Setup
From the root of the repo:
```bash
opam switch create . ocaml-variants.4.13.1+options --no-install
opam install . --deps-only -t

git clone --branch v0.146.0 --depth 1 git@github.com:facebook/flow.git flow
ln -s "$(pwd)/flow/src/parser" src/flow_parser

# MacOS
sed -i '' 's/Pervasives/Stdlib/g' flow/src/parser/parser_flow.ml
# Linux
sed -i 's/Pervasives/Stdlib/g' flow/src/parser/parser_flow.ml
```

### MacOS - Build & Run
```bash
DUNE_PROFILE=release dune build src/cli/strings.exe && cp _build/default/src/cli/strings.exe strings.mac && strip strings.mac

./strings.mac ../group-income-simple/
```

### Docker (Linux) - Build & Run
```bash
docker build . -t strings:latest
STRINGS_CID="$(docker create strings:latest)"
docker cp "$STRINGS_CID":/app/strings.exe strings.linux
docker rm "$STRINGS_CID"

# Trying it on Ubuntu 18.04
docker run -it --rm \
  -v "$(pwd):/app" \
  -v "$(realpath "$(pwd)/../group-income-simple"):/repo" \
  -w /repo \
  ubuntu:18.04
  # Then run: '/app/strings.linux .'
```
