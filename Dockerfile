FROM haskell:8.8.1 AS builder

ENV HOME=/home/app
WORKDIR $HOME

COPY package.yaml $HOME/
COPY Setup.hs $HOME/
COPY stack.yaml $HOME/
COPY stack.yaml.lock $HOME/

RUN stack build --dependencies-only

COPY app $HOME/app
COPY test $HOME/test
COPY src $HOME/src

RUN stack build

RUN cp $(stack exec -- which cl8w-exe) cl8w

FROM ubuntu:18.04

ENV HOME=/home/app
WORKDIR $HOME

COPY --from=builder /home/app/cl8w /usr/local/bin/cl8w
