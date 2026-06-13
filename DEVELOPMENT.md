## Local development

### Setup
From the root of the repo:
```sh
brew install opam libomp

opam switch create . ocaml-variants.5.0.0+options --no-install
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

### MacOS - Development
```sh
# Build
dune build src/cli/strings.exe -w

# Run (separate terminal)
cp _build/default/src/cli/strings.exe strings.mac && ./strings.mac
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

# Trying it on Ubuntu 20.04
docker run -it --rm \
  -v "$(pwd):/app" \
  -v "$(realpath "$(pwd)/../group-income-simple"):/repo" \
  -w /repo \
  ubuntu:20.04
  # Then run:
  ## apt-get update && apt-get install musl
  ## /app/strings.linux frontend
```

### Testing

To run the automated tests, ensure your opam environment is initialized:

```sh
eval $(opam env)
dune runtest tests/
```

This builds the project, runs the inline unit tests, and executes the integration test (which verifies extraction and translation preservation in an isolated temporary directory).

## Releases

Releases are automated by [`.github/workflows/release.yml`](.github/workflows/release.yml). Pushing any tag starting with `v` (e.g. `v2.3.0`) triggers the workflow, which:

1. Builds `strings.linux.tar.gz` (via the `Dockerfile`, same as the manual Docker flow above).
2. Builds `strings.mac` on a `macos-latest` runner. **The macOS binary is arm64-only (Apple silicon); Intel Macs are not supported.**
3. Publishes a GitHub Release titled after the tag, with both binaries attached. The release body contains SHA256 checksums, setup/usage instructions, and a `## Changes` section with release notes generated from the commit messages since the previous release.

To cut a release:

```sh
# 1. Update `let version = "x.y.z"` in src/cli/strings.ml (the workflow warns on mismatch)
# 2. Commit, then:
git tag vx.y.z && git push origin vx.y.z
```

The workflow can also be run manually from the Actions tab (`workflow_dispatch`) by entering an existing tag — useful for retrying a failed release; it updates the existing release idempotently.

Repository configuration:

- **Secret `ZAI_API_KEY`** (required for release notes): a [Z.ai](https://z.ai) API key used by `scripts/generate-release-notes.mjs`. If missing or the API call fails, the release still succeeds, just without the `## Changes` section.
- **Variable `RELEASE_MODEL`** (optional): the Z.ai model used for release notes; defaults to `glm-5.1`.
