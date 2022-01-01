FROM commercialhaskell/stackage:lts14 AS builder

WORKDIR /app

RUN curl --silent --show-error --location https://github.com/sass/dart-sass/releases/download/1.45.2/dart-sass-1.45.2-linux-x64.tar.gz | tar --strip-components 1 --extract --gzip --file - dart-sass/sass 

COPY blog.hs .
COPY blog.cabal .
COPY stack.yaml .
COPY stack.yaml.lock . 
COPY LICENSE . 
RUN stack install --local-bin-path /app

FROM alpine:latest

WORKDIR /app

COPY --from=builder /app/blog .
COPY --from=builder /app/sass .

ENV PATH=/app:/usr/sbin:/usr/bin:/sbin:/bin
ENTRYPOINT ["/app/blog"]
CMD ["build"]

