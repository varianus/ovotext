#!/bin/bash
# set -x
#Set path where lazarus is installed
LAZARUS_DIR=~/development/lazarus
CONFIG_PATH=
# 
if [ "$BASE" = "" ]; then
  BASE=$(pwd)
fi   
   
if [ ! "$OS_TARGET" = "" ]; then 
  DC_ARCH="$DC_ARCH --os=$OS_TARGET"
fi
   
if [ ! "$CPU_TARGET" = "" ]; then
 DC_ARCH="$DC_ARCH --cpu=$CPU_TARGET"
fi 

if [ ! "$WIDGETSET_TARGET" = "" ]; then
 DC_ARCH="$DC_ARCH --ws=$WIDGETSET_TARGET"
fi 

NONE=-l
if [ ! "$CONFIG_PATH" = "" ]; then
 PCP=--pcp="$CONFIG_PATH"
fi

# clean build files
rm -Rf $BASE/lib/*

rm -Rf $BASE/linux
#
$LAZARUS_DIR/lazbuild -B -r $PCP --build-mode=Release $BASE/ovotext.lpi $DC_ARCH
strip --strip-all $BASE/bin/linux/ovotext

echo $NONE > $BASE/extrafpc.cfg
