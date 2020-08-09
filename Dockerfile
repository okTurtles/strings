FROM ocaml/opam2:debian-8-opam
WORKDIR /home/opam/opam-repository

RUN sudo apt-get update \
  && sudo apt-get install -y m4 pkg-config libev-dev \
  && git pull origin master \
  && opam-sandbox-disable \
  && opam init -k git -a /home/opam/opam-repository --bare \
  && opam init --bare \
  && opam switch create 4.10 4.10.0+flambda \
  && opam switch 4.10 \
  && opam install -y opam-depext

RUN opam repository set-url default https://opam.ocaml.org \
  && opam update \
  && OPAMYES=1 opam install core \
  && opam clean

WORKDIR /app

COPY strings.opam .

RUN OPAMYES=1 opam install . --deps-only

RUN sudo chown -R opam /app \
  && git clone --branch v0.120.1 --depth 1 https://github.com/facebook/flow.git flow

COPY . .

ENV DUNE_PROFILE release

RUN sudo chown -R opam /app \
  && ln -s "$(pwd)/flow/src/parser" src/flow_parser \
  && sed -i 's/Pervasives/Stdlib/g' flow/src/parser/parser_flow.ml \
  && opam exec -- dune build src/cli/strings.exe \
  && cp /app/_build/default/src/cli/strings.exe . \
  && chmod 755 strings.exe \
  && strip strings.exe
