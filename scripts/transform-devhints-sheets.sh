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
  md_sheet=$SCRIPTPATH/cheatsheets/$1
  command=${2:-$(as_command $1)}
  source="https://github.com/rstacruz/cheatsheets/blob/master/$1"

  pandoc \
    -f gfm \
    -t org \
    --template=$SCRIPTPATH/sheet-template.org \
    --log=transform.log \
    --lua-filter=$SCRIPTPATH/reduce-header-levels.lua \
    --variable=command=$command \
    --variable=source=$source \
    --verbose \
    $md_sheet
}

batch(){
  out=$SCRIPTPATH/../sheets
  transform_to_org bash.md   > $out/bash.org
  transform_to_org es6.md    > $out/es6.org
  transform_to_org go.md     > $out/go.org
  transform_to_org html.md   > $out/html.org
  transform_to_org jsdoc.md  > $out/jsdoc.org
  transform_to_org python.md > $out/python.org
  transform_to_org xpath.md  > $out/xpath.org
}


update_sheets
batch
