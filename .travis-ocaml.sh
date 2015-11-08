BUILD_CMD=${BUILD_CMD:-make}
set -uex
# the base opam repository to use for bootstrapping and catch-all namespace
BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository}
echo $PATH

opam init -a ${BASE_REMOTE} --comp=4.02.3
eval $(opam config env)
opam install -y depext
opam --version
opam --git-version
# install dependencies
! [ -z "$DEPS" ] && opam install -y $DEPS

# run
${BUILD_CMD}
