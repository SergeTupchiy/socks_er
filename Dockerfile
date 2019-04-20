# https://github.com/tsloughter/vonnegut/blob/7dd7fd0bcad3437a2ca231e066eae9710c9bce03/Dockerfile
FROM erlang:21.2.2-alpine as builder

RUN apk add --no-cache --update tar curl git bash make libc-dev gcc g++

RUN set -xe \
    && curl -fSL -o rebar3 "https://s3.amazonaws.com/rebar3/rebar3" \
    && chmod +x ./rebar3 \
    && ./rebar3 local install \
    && rm ./rebar3

ENV PATH "$PATH:/root/.cache/rebar3/bin"

RUN mkdir -p /x/priv
WORKDIR /x

# build and cache dependencies
COPY rebar.config rebar.lock /x/
RUN rebar3 compile

# copy in the source and build release
COPY . /x/
RUN rebar3 as prod release

FROM alpine:3.8

RUN apk add --no-cache openssl-dev ncurses

COPY --from=builder /x/_build/prod/rel/socks_er /a

WORKDIR /a

EXPOSE 1080 1080

CMD [ "/a/bin/socks_er", "foreground" ]