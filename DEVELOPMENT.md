## Local development

### Setup
From the root of the repo:
```bash
opam switch create . ocaml-variants.4.13.1+options --no-install
opam install . --deps-only -t

# Remove old Flow version
rm -rf flow && unlink src/flow_parser && unlink src/sedlex && unlink src/collections

# Install Flow
git clone --branch v0.183.1 --depth 1 git@github.com:facebook/flow.git flow
ln -s "$(pwd)/flow/src/parser" src/flow_parser
ln -s "$(pwd)/flow/src/third-party/sedlex" src/sedlex
ln -s "$(pwd)/flow/src/hack_forked/utils/collections" src/collections
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
  ubuntu:20.04
  # Then run:
  ## apt-get update && apt-get install musl
  ## /app/strings.linux frontend
```
