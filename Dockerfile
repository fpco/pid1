FROM fpco/alpine-haskell-stack:9.2.5

COPY . /app

RUN apk add upx \
    && env STACK_YAML=/app/stack.yaml stack --system-ghc install --local-bin-path /app --flag pid1:static \
    && upx --best /app/pid1
