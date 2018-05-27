FROM fpco/stack-build:lts-11 as builder

ARG INSTALL="env DEBIAN_FRONTEND=noninteractive apt-get install -y"
ARG STACK="stack --local-bin-path /sbin --system-ghc"

# Update to used statically linked libraries
# https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack#crtbegint-swap
WORKDIR /usr/lib/gcc/x86_64-linux-gnu/5/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o

# Install llvm-5.0-dev
RUN apt-get update && $INSTALL llvm-5.0-dev

# Build all dependencies
WORKDIR /opt/app
COPY package.yaml .
COPY stack.yaml .
RUN $STACK build --only-dependencies --ghc-options '-optl-static -optl-pthread -optl-lm -fPIC' --flag llvm-hs:-shared-llvm

# Build executable
COPY src src/
COPY app app/
COPY test test/
COPY README.md .
COPY ChangeLog.md .
RUN  $STACK install --ghc-options '-optl-static -optl-pthread -optl-lm -fPIC' --flag llvm-hs:-shared-llvm

# Copy statically linked executable to new fresh container
FROM alpine:3.7
COPY --from=builder /sbin/mixed-signals-exe /usr/local/bin/
CMD mixed-signals-exe
