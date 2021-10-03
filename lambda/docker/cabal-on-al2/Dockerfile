FROM public.ecr.aws/lambda/provided:al2

# install ghcup
RUN yum install -y curl gcc-c++ gcc  gmp-devel make ncurses-compat-libs xz
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=${PATH}:/root/.ghcup/bin

# install ghc and cabal
RUN ghcup install ghc 8.10.7
RUN ghcup set ghc 8.10.7
RUN ghcup install cabal 3.4.0.0
RUN ghcup set cabal 3.4.0.0

# build
RUN yum install -y zlib-devel
VOLUME /code
WORKDIR /code
ENTRYPOINT ["cabal"]
