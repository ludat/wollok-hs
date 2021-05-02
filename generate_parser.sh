#!/bin/bash

set -euo pipefail

mkdir -p generated-code

stack exec bnfc -- -m --haskell --outputdir=generated-code --name-space=Parser --generic Grammar.cf
stack exec alex -- --ghc generated-code/Parser/LexGrammar.x
stack exec happy -- --ghc --coerce --array --info generated-code/Parser/ParGrammar.y

rm generated-code/Parser/TestGrammar.hs

GHC_NO_WARNINGS="{-# OPTIONS_GHC -w #-}\n"
sed -i "1s;^;$GHC_NO_WARNINGS;" generated-code/Parser/AbsGrammar.hs
sed -i "1s;^;$GHC_NO_WARNINGS;" generated-code/Parser/ErrM.hs
sed -i "1s;^;$GHC_NO_WARNINGS;" generated-code/Parser/LexGrammar.hs
sed -i "1s;^;$GHC_NO_WARNINGS;" generated-code/Parser/PrintGrammar.hs
sed -i "1s;^;$GHC_NO_WARNINGS;" generated-code/Parser/SkelGrammar.hs

cp generated-code/Parser/*.hs src/Parser
