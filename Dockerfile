FROM docker.io/archlinux:latest
RUN pacman -Syu nodejs npm git --noconfirm
RUN npm i -g purescript spago
RUN mkdir isvalidjson-purs
WORKDIR isvalidjson-purs
COPY packages.dhall packages.dhall
COPY spago.dhall spago.dhall
RUN spago install
COPY persona3-hacker persona3-hacker
RUN rm persona3-hacker/src/Main.purs
COPY src src
COPY test test
RUN spago build
RUN spago test
CMD ["spago", "test"]
