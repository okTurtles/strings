####################
### build_shared ###
####################
FROM asemio/mountain-caravan:2.1.0 AS build_shared
WORKDIR /app
RUN sudo apk update \
  && sudo apk upgrade \
  && sudo apk add --no-cache perl cmake npm xz patchelf

#####################
### build_quickjs ###
#####################
FROM build_shared AS build_quickjs
RUN echo '=== Installing QuickJS ===' \
  && curl https://bellard.org/quickjs/quickjs-2021-03-27.tar.xz > quickjs.tar.xz \
  && tar xvf quickjs.tar.xz && rm quickjs.tar.xz \
  && mv quickjs-2021-03-27 quickjs \
  && cd quickjs && make

#############
### build ###
#############
FROM build_shared AS build
COPY strings.opam .

ENV DUNE_PROFILE release

RUN opam update \
  && OPAMYES=1 opam install . --deps-only

RUN echo '=== Installing Flow ===' \
  && git clone --branch v0.183.1 --depth 1 https://github.com/facebook/flow.git flow

RUN echo '=== Installing JS dependencies ===' \
  && npm install --no-save typescript browserify pug-lexer pug-parser pug-walk

COPY --from=build_quickjs /app/quickjs/libquickjs.a /app/quickjs/libquickjs.a
COPY --from=build_quickjs /app/quickjs/quickjs.h /app/quickjs/quickjs.h
COPY --from=build_quickjs /app/quickjs/qjsc /app/quickjs/qjsc

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
