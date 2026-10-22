# Plain Alpine instead of asemio/mountain-caravan, which is published for amd64 only.
# This lets the same file build natively on amd64 and arm64 hosts.
# The toolchain setup copies mountain-caravan 2.4.0: Alpine 3.20 and an opam switch with OCaml 5.1.1+options.
# Stay on Alpine 3.20: opam-repository marks core_unix v0.17 as unavailable on Alpine 3.21 and newer.
FROM alpine:3.20
# Keep the package index from `apk update` in the image: opam reads it to check system packages (depexts).
RUN apk update \
  && apk upgrade \
  && apk add --no-cache \
    bash build-base ca-certificates cmake curl git libev-dev linux-headers m4 make \
    npm opam openssl patchelf perl pkgconfig sudo tar unzip xz \
  && adduser -D opam \
  && echo 'opam ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER opam
SHELL ["/bin/bash", "-c"]
RUN opam init -y --bare --disable-sandboxing -a \
  && opam switch create 5.1.1+options --packages=ocaml-variants.5.1.1+options,ocaml-option-flambda

WORKDIR /app

COPY strings.opam .

ENV DUNE_PROFILE release

RUN opam update \
  && OPAMYES=1 opam install . --deps-only

RUN echo '=== Installing QuickJS ===' \
  && curl https://bellard.org/quickjs/quickjs-2021-03-27.tar.xz > quickjs.tar.xz \
  && tar xvf quickjs.tar.xz && rm quickjs.tar.xz \
  && mv quickjs-2021-03-27 quickjs \
  && cd quickjs && make && cd -

RUN echo '=== Installing Flow ===' \
  && git clone --branch v0.183.1 --depth 1 https://github.com/facebook/flow.git flow

RUN echo '=== Installing JS dependencies ===' \
  && npm install --no-save typescript@6 browserify pug-lexer pug-parser pug-walk

COPY src src
COPY dune dune
COPY dune-project dune-project

RUN sudo chown -R opam /app \
  && ln -s "$(pwd)/flow/src/parser" src/flow_parser \
  && ln -s "$(pwd)/flow/src/third-party/sedlex" src/sedlex \
  && ln -s "$(pwd)/flow/src/hack_forked/utils/collections" src/collections

RUN echo '=== Building ===' \
  && opam exec -- dune build src/cli/strings.exe \
  && cp /app/_build/default/src/cli/strings.exe . \
  && chmod 755 strings.exe \
  && strip strings.exe \
  && mkdir lib \
  && ldd strings.exe | awk '$2 == "=>" && $3 !~ /ld-musl/ {print $1, $3}' | sort | uniq | awk '{print $2}' | xargs -n1 -I{} -- cp {} lib/ \
  && patchelf --set-rpath '$ORIGIN/lib' strings.exe
