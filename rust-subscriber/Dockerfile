# NOTE: Do not use latest, pin your versions!
FROM rust:1.85 AS builder

WORKDIR /app

# Install openssl development libraries
RUN apt-get update && apt-get install -y libssl-dev

# Copy Cargo files and build dependencies
COPY Cargo.toml Cargo.lock ./

# Copy source code
COPY src ./src

# Build the application
RUN cargo build --release

# Runtime image
# FROM debian:bullseye-slim
FROM rust:1.85

ARG APP=/usr/src/app

RUN apt-get update && \
    apt-get install -y ca-certificates tzdata && \
    rm -rf /var/lib/apt/lists/*

ENV TZ=Etc/UTC \
    APP_USER=appuser

RUN groupadd $APP_USER && \
    useradd -g $APP_USER $APP_USER && \
    mkdir -p ${APP}

COPY --from=builder /app/target/release/rust-subscriber ${APP}/rust-subscriber

RUN chown -R $APP_USER:$APP_USER ${APP}

USER $APP_USER
WORKDIR ${APP}

CMD ["./rust-subscriber"]
