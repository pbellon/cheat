#!/usr/bin/env bash

SCRIPTPATH=`dirname $0`

# source $SCRIPTPATH/env.sh 


update_sheets() {
  git submodule update --init
}

as_command(){
  name=$(basename $1)
  command="${name%.*}"
  echo "$command"
}

transform_to_org(){
  md_sheet=$1
  command=${2:-$(as_command $1)}
  pandoc \
    -f markdown \
    -t org \
    --base-header-level=1 \
    --template=sheet-template.org \
    --log=transform.log \
    --lua-filter=$SCRIPTPATH/reduce-header-levels.lua \
    --verbose \
    $md_sheet
    # --variable=command=$command \
}

transform_to_org $SCRIPTPATH/test.md

