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
  source=$3
  pandoc \
    -f markdown \
    -t org-fenced_code_blocks-backtick_code_blocks \
    --template=$SCRIPTPATH/sheet-template.org \
    --log=$SCRIPTPATH/transform.log \
    --highlight-style=pygments \
    --lua-filter=$SCRIPTPATH/reduce-header-levels.lua \
    --variable=command=$command \
    --variable=source=$source \
    --verbose \
    $md_sheet

}

transform_devhints_to_org() {
  md_sheet=$SCRIPTPATH/cheatsheets/$1
  source="https://github.com/rstacruz/cheatsheets/blob/master/$1"
  command=${2:-$(as_command $1)}

  transform_to_org $md_sheet $command $source
}

batch_devhints(){
  out=$SCRIPTPATH/../sheets
  transform_devhinsts_to_org bash.md   > $out/bash.org
  transform_devhinsts_to_org es6.md    > $out/es6.org
  transform_devhinsts_to_org go.md     > $out/go.org
  transform_devhinsts_to_org html.md   > $out/html.org
  transform_devhinsts_to_org jsdoc.md  > $out/jsdoc.org
  transform_devhinsts_to_org less.md   > $out/less.org
  transform_devhinsts_to_org npm.md    > $out/npm.org
  transform_devhinsts_to_org python.md > $out/python.org
  transform_devhinsts_to_org ruby.md   > $out/ruby.org
  transform_devhinsts_to_org xpath.md  > $out/xpath.org
}

if [ "$1" == "--batch" ]; then
  update_sheets
  batch_devhints
else
  out=$SCRIPTPATH/../sheets
  transform_to_org "$@"
fi
