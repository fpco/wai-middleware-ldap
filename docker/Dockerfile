FROM ubuntu:16.04
MAINTAINER Alexey Kuleshevich <alexey@fpcomplete.com>
ARG PID1_VERSION=0.1.0.1
VOLUME /config
RUN apt-get update && \
    apt-get install -y libgmp10 libldap2-dev libsasl2-dev curl && \
    curl -L "https://github.com/fpco/pid1/releases/download/pid1%2F$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar -xz -C /usr/local && \
    chown -R root:root /usr/local/sbin && \
    apt-get remove -y curl
ADD artifacts/wai-ldap /usr/local/bin/wai-ldap
ENTRYPOINT ["/usr/local/sbin/pid1"]
CMD ["sh", "-c", "update-ca-certificates && wai-ldap --config-file=/config/wai-ldap.yaml"]
