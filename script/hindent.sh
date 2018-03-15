#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${DIR}"
# End Boilerplate


stack --resolver lts-9.6 setup
stack --resolver lts-9.6 build hindent
# Get a list of all haskell files and pass them through hindent.
git ls-files '*.hs' -z | xargs -0 -I {} stack --resolver lts-9.6 exec hindent -- {}

# Expect no changes in the working directory. Complain if there are any.
git diff --exit-code
