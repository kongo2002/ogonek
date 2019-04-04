#
# BASE BUILD IMAGE
#
FROM alpine:3.9

RUN apk add --no-cache build-base autoconf openssl git openssl-dev && \
    wget http://erlang.org/download/otp_src_20.1.tar.gz && \
    tar xvf otp_src_20.1.tar.gz && \
    rm otp_src_20.1.tar.gz && \
    cd otp_src_20.1 && \
    ./otp_build autoconf && \
    ./configure --disable-hipe --without-termcap --without-javac && \
    make -j8 && \
    make install && \
    cd .. && \
    rm -rf otp_src_20.1

ADD https://s3.amazonaws.com/rebar3/rebar3 /usr/bin/rebar3
RUN chmod a+x /usr/bin/rebar3

WORKDIR /build/server
COPY server .

RUN rebar3 get-deps
RUN rebar3 as docker release -o /output

#
# RUNTIME IMAGE
#
FROM alpine:3.9

RUN apk add --no-cache libstdc++ ca-certificates
WORKDIR /opt
COPY --from=0 /output/ogonek .

CMD ["./bin/ogonek-1.0.0"]
