#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${DIR}/.."
# End Boilerplate

# Get a list of all haskell files and pass them through ormolu.
git ls-files '*.hs' -z | xargs -0 -I {} ormolu --mode inplace {};

# Expect no changes in the working directory. Complain if there are any.
git diff --exit-code;
