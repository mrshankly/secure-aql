FROM erlang:23

LABEL maintainer="jb.marques@campus.fct.unl.pt"

ENV AQL_REPOSITORY "https://github.com/mrshankly/secure-aql.git"
ENV AQL_BRANCH "master"

RUN git clone -b "${AQL_BRANCH}" --single-branch --depth 1 "${AQL_REPOSITORY}" /aql-src
RUN cd /aql-src && make release
RUN cp -r /aql-src/_build/default/rel/aql /aql/
RUN rm -rf aql-src

# Erlang/OTP configuration.
ENV NODE_NAME "antidote_aql@127.0.0.1"
ENV COOKIE "secret"

# AntidoteDB configuration.

ENV HANDOFF_PORT "8099"
ENV LOGREADER_PORT "8085"
ENV PBSUB_PORT "8086"
ENV METRICS_PORT "3001"
ENV PB_PORT "8087"
ENV PB_IP "0.0.0.0"

ENV RING_SIZE "16"

ENV DEBUG_LOGGER_LEVEL "info"

ENV ROOT_DIR_PREFIX "/antidote-data/"
ENV DATA_DIR_PREFIX "data/"
ENV LOGGER_DIR_PREFIX "log/"

ENV ANTIDOTE_TXN_CERT "true"
ENV ANTIDOTE_TXN_PROT "clocksi"
ENV ANTIDOTE_RECOVER_FROM_LOG "true"
ENV ANTIDOTE_META_DATA_ON_START "true"
ENV ANTIDOTE_SYNC_LOG "false"
ENV ANTIDOTE_ENABLE_LOGGING "true"
ENV ANTIDOTE_AUTO_START_READ_SERVERS "true"

# Expose AQL protocol port.
EXPOSE 8321

ENTRYPOINT /aql/bin/env foreground
