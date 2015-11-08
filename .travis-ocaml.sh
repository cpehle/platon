BUILD_CMD=${BUILD_CMD:-make}
set -uex
# the base opam repository to use for bootstrapping and catch-all namespace
BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository}
opam init -a ${BASE_REMOTE} --comp=4.02.3
eval $(opam config env)
opam install depext
opam --version
opam --git-version
# install dependencies
! [ -z "$DEPS" ] && opam install $DEPS

# run
${BUILD_CMD}
