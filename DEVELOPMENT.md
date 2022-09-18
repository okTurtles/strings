## Local development

### Setup
From the root of the repo:
```sh
brew install opam libomp mold

opam switch create . ocaml-variants.4.14.0+options --no-install
opam install . --deps-only -t

# Remove old Flow version
rm -rf flow && unlink src/flow_parser && unlink src/sedlex && unlink src/collections

# Install Flow
git clone --branch v0.183.1 --depth 1 git@github.com:facebook/flow.git flow
ln -s "$(pwd)/flow/src/parser" src/flow_parser
ln -s "$(pwd)/flow/src/third-party/sedlex" src/sedlex
ln -s "$(pwd)/flow/src/hack_forked/utils/collections" src/collections

# JS dependencies
npm install --no-save typescript browserify pug-lexer pug-parser pug-walk

# Install QuickJS
curl https://bellard.org/quickjs/quickjs-2021-03-27.tar.xz > quickjs.tar.xz
tar xvf quickjs.tar.xz && rm quickjs.tar.xz
mv quickjs-2021-03-27 quickjs
cd quickjs && make && cd -
```

### MacOS - Build & Run
```sh
# Don't forget to update the version number in [strings.ml]

rm -f strings.mac && dune clean && DUNE_PROFILE=release dune build src/cli/strings.exe && cp _build/default/src/cli/strings.exe strings.mac && chmod 755 strings.mac && strip strings.mac

./strings.mac ../group-income-simple/
```

### Docker (Linux) - Build & Run
```sh
# Don't forget to update the version number in [strings.ml]

docker build . -t strings:latest

STRINGS_CID="$(docker create strings:latest)" \
&& rm -rf strings.linux strings.linux.tar.gz lib \
&& docker cp "$STRINGS_CID":/app/strings.exe strings.linux \
&& docker cp "$STRINGS_CID":/app/lib lib \
&& docker rm "$STRINGS_CID" \
&& tar czvf strings.linux.tar.gz strings.linux lib

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
