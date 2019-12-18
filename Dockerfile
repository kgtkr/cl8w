FROM haskell:8.8.1 AS builder

ENV HOME=/home/app
WORKDIR $HOME

COPY . $HOME
RUN stack build
RUN cp $(stack exec -- which cl8w-exe) cl8w

FROM ubuntu:18.4

ENV HOME=/home/app
WORKDIR $HOME

COPY --from=builder /home/app/cl8w $HOME/cl8w
