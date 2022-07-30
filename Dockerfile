FROM asemio/mountain-caravan:1.1.1
WORKDIR /app
RUN sudo apk update \
  && sudo apk upgrade \
  && sudo apk add --no-cache perl cmake

COPY strings.opam .

ENV DUNE_PROFILE release

RUN opam update \
  && OPAMYES=1 opam install . --deps-only

COPY . .

RUN sudo chown -R opam /app \
  && git clone --branch v0.183.1 --depth 1 https://github.com/facebook/flow.git flow \
  && ln -s "$(pwd)/flow/src/parser" src/flow_parser \
  && ln -s "$(pwd)/flow/src/third-party/sedlex" src/sedlex \
  && ln -s "$(pwd)/flow/src/hack_forked/utils/collections" src/collections \
  && opam exec -- dune build src/cli/strings.exe \
  && cp /app/_build/default/src/cli/strings.exe . \
  && chmod 755 strings.exe \
  && strip strings.exe
