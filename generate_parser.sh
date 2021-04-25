#!/bin/bash

set -euo pipefail

mkdir -p generated-code

stack exec bnfc -- -m --haskell --outputdir=generated-code --name-space=Parser --generic Grammar.cf
stack exec alex -- --ghc generated-code/Parser/LexGrammar.x
stack exec happy -- --ghc --coerce --array --info generated-code/Parser/ParGrammar.y

rm generated-code/Parser/TestGrammar.hs
cp generated-code/Parser/*.hs src/Parser