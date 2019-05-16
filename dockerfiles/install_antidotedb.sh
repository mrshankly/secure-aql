#!/bin/sh
set -ex
git clone https://github.com/PDCosta/antidote.git 
cd antidote
git checkout aql_oriented_features
make rel
