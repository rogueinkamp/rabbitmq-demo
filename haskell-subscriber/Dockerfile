# ===============================================================
# Build stage
# ===============================================================
FROM ubuntu:18.04 as haskell

# Tell `stack` where to find its content (not in $HOME)
ENV STACK_ROOT /home/stackage/.stack

# Install stack and dependencies
RUN apt-get update
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh

# Install deps via stack
RUN stack install amqp

WORKDIR /opt/build
COPY ./app/Main.hs .
RUN stack ghc --package process -- Main.hs

# ===============================================================
# Run stage
# ===============================================================
FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y dnsutils netcat

COPY --from=haskell /opt/build/Main /Main
ENTRYPOINT ["/Main"]
