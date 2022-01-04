FROM fpco/stack-build AS haskell

# ENV STACK_ROOT /home/stackage/.stack

WORKDIR /build

COPY blog.hs .
COPY blog.cabal .
COPY stack.yaml .
COPY stack.yaml.lock . 
COPY LICENSE . 

RUN stack install --local-bin-path /build

FROM dart:stable AS dart

WORKDIR /build

RUN curl --silent --show-error --location https://github.com/sass/dart-sass/releases/download/1.45.2/dart-sass-1.45.2-linux-x64.tar.gz \
  | tar --strip-components 1 --extract --gzip --file - dart-sass/sass 

FROM alpine:latest

WORKDIR /tmp

WORKDIR /haskell/lib64

COPY --from=haskell /lib/x86_64-linux-gnu/ld-2.* ld-linux-x86-64.so.2

WORKDIR /haskell/lib/x86_64-linux-gnu

COPY --from=haskell /lib/x86_64-linux-gnu/libc.so* ./
COPY --from=haskell /usr/lib/x86_64-linux-gnu/libgmp.so* ./
COPY --from=haskell /lib/x86_64-linux-gnu/libpthread.so* ./
COPY --from=haskell /lib/x86_64-linux-gnu/libutil.so* ./
COPY --from=haskell /lib/x86_64-linux-gnu/libdl.so* ./
COPY --from=haskell /lib/x86_64-linux-gnu/librt.so* ./
COPY --from=haskell /usr/lib/x86_64-linux-gnu/libz.so* ./
COPY --from=haskell /lib/x86_64-linux-gnu/libm.so* ./

WORKDIR /usr/lib/locale

COPY --from=haskell /usr/lib/locale/ ./

WORKDIR /app

COPY --from=haskell /build/blog .
COPY --from=dart /build/sass .
COPY --from=dart /runtime /

ENV PATH=/app:/usr/sbin:/usr/bin:/sbin:/bin
ENV LANG=C.UTF-8

ENTRYPOINT ["/haskell/lib64/ld-linux-x86-64.so.2", "--library-path", "/haskell/lib/x86_64-linux-gnu", "/app/blog"]
CMD ["build"]
