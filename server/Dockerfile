

# docker build --build-arg BUILD_ENV=production --build-arg APP_VERSION=1.8.1 --build-arg WORKDIR=/opt/docker --build-arg DEMON_USER=ttubu --build-arg DEMON_USERID=1010 -t trny-server .

# Start with a base image for Java
FROM jorolich/ubuntu20-openjdk11-unzip-wkhtmltopdf:latest

ARG BUILD_ENV
ARG APP_VERSION
ARG WORKDIR
ARG DEMON_USER
ARG DEMON_USERID

ENV VERSION=${APP_VERSION}
ENV DEMON_USER=${DEMON_USER}
ENV DEMON_USERID=${DEMON_USERID}

USER root

# Set working directory
WORKDIR ${WORKDIR}

# Copy the distribution zip file to the container
# Assuming that the dist .zip file is named like server-x.x.x.zip
COPY ./target/universal/server-${VERSION}.zip .

# Unzip the distribution and remove the zip file afterward
RUN apt-get update && apt-get install -y unzip 
RUN unzip server-${VERSION}.zip && \
    mv server-${VERSION}/* .    && \
    rmdir server-${VERSION}     && \
    rm server-${VERSION}.zip    && \
    rm -rf conf/messages        && \
    rm -rf db                   && \
    rm -rf public               && \
    apt-get remove -y unzip && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN if [ "$BUILD_ENV" = "production" ]; then \
    # Production-specific commands
    echo "PRODUCTION ENVIRONMENT" ; \ 
    cp conf/routes.prod conf/routes; \
    cp conf/logback.prod.xml  conf/logback.xml; \
else \
    # Default (development) commands
    echo "DEVELOPMENT ENVIRONMENT" ; \ 
fi


# create daemon user 
RUN id -u ${DEMON_USER} 1>/dev/null 2>&1 || (( getent group 0 1>/dev/null 2>&1 || ( type groupadd 1>/dev/null 2>&1 && groupadd -g 0 root || addgroup -g 0 -S root )) && ( type useradd 1>/dev/null 2>&1 && useradd --system --create-home --uid ${DEMON_USERID} --gid 0 ${DEMON_USER} || adduser -S -u ${DEMON_USERID} -G root ${DEMON_USER} ))

# Create a volume for logs with owner
RUN mkdir logs && chown ${DEMON_USER} logs && touch logs/application.log
VOLUME ["${WORKDIR}/logs"]

# Create a volume for database
RUN mkdir ${WORKDIR}/db && chown ${DEMON_USER} ${WORKDIR}/db
VOLUME ["${WORKDIR}/db"]

# Create a volume for public content
RUN mkdir ${WORKDIR}/public && chown ${DEMON_USER} ${WORKDIR}/public
VOLUME ["${WORKDIR}/public"]

USER ${DEMON_USERID}:0

# for testing
# CMD exec /bin/bash -c "trap : TERM INT; sleep infinity & wait"
ENTRYPOINT ["./bin/server", "-Dapplication.home=${WORKDIR}" ]
