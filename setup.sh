#! /bin/bash

if [[ "$1" != "day"* ]]; then
    echo "Missing input: number of day"
    exit 1
fi

PROG_NAME=$1
SOURCE_DIR="days/${PROG_NAME}"
BUILD_DIR=build
LIB_DIR=$(pwd)

GREEN='\033[0;32m'
YELLOW='\033[1;33m'


function template_init () {
    # create main .hs file
    cat <<EOT >> "$1.hs"
module Main where

import AocShared (expect, ftrace, readLines)
import Lib
import System.IO

main :: IO ()
main = do
  lines <- readLines "input.txt"
  expect (length lines) 1 \`ftrace\` show lines

EOT

    # create skeleton input file
    cat <<EOT >> input.txt
Almost empty file
EOT
}

MODIFIED_CMD="stat *.hs *.txt ${LIB_DIR}/*.hs -c %Y | tr -d '\n'"
LAST_MODIFIED=0

function source_modified () {
    MODIFIED=$(eval "$MODIFIED_CMD")
    if [ "$LAST_MODIFIED" != "$MODIFIED" ]; then
        return 0
    fi
    return 1
}

function update_modified_time () {
    LAST_MODIFIED=$(eval "$MODIFIED_CMD")
}

function format_code () {
    ormolu --mode inplace "$(find . -name '*.hs')" --no-cabal
}

function compile () {
    ghc --make "$1" -i"${LIB_DIR}" -o ${BUILD_DIR}/"$1" \
        -odir ${BUILD_DIR} -hidir ${BUILD_DIR} -O
}


# setup folder structure if needed
if [[ -d "$SOURCE_DIR" ]]; then
    cd "$SOURCE_DIR" || exit
else
    mkdir "$SOURCE_DIR"
    cd "$SOURCE_DIR" || exit
    mkdir ${BUILD_DIR}
    template_init "$PROG_NAME"
fi

while true
do
    if source_modified; then
        clear
        echo -e "${YELLOW}Modification detected, reloading...\n"
        tput sgr0

        format_code
        update_modified_time

        if compile "${PROG_NAME}"; then
            clear
            echo "Running $(pwd)/${BUILD_DIR}/${PROG_NAME}"
            echo -e "_______________________________________________\n${GREEN}"
            time ./${BUILD_DIR}/"${PROG_NAME}"
        fi
    fi
    sleep 0.03
done