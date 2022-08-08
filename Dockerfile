FROM asemio/mountain-caravan:2.0.0
WORKDIR /app
RUN sudo apk update \
  && sudo apk upgrade \
  && sudo apk add --no-cache perl cmake npm xz

COPY strings.opam .

ENV DUNE_PROFILE release

RUN opam update \
  && OPAMYES=1 opam install . --deps-only

RUN echo '=== Installing Flow ===' \
  && git clone --branch v0.183.1 --depth 1 https://github.com/facebook/flow.git flow

RUN echo '=== Installing TypeScript ===' \
  && npm install --no-save typescript browserify

RUN echo '=== Installing QuickJS ===' \
  && curl https://bellard.org/quickjs/quickjs-2021-03-27.tar.xz > quickjs.tar.xz \
  && tar xvf quickjs.tar.xz && rm quickjs.tar.xz \
  && mv quickjs-2021-03-27 quickjs \
  && cd quickjs && make && cd -

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
  && strip strings.exe
