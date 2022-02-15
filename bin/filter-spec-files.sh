#!/usr/bin/env bash

# https://stedolan.github.io/jq/manual/

patterns=(
    "c2ffi-spec/zlib.*.spec"
)
queries=(
    # it's tempting to do [sort_by(.name) | .[] |... for nice
    # diff'ability, but the lisp generator works in a streaming
    # fashion, i.e. it is sensitive to the order of the definitions
    # (e.g. it filters out struct fields whose type hasn't been seen
    # yet).
    '[.[] | select(.location | contains("/types.h") or contains("/stdint-uintn.h") or contains("/stdint-intn.h") or contains("/stddef.h") or contains("zlib.h") or contains("zconf.h"))]'
)

for i in "${!patterns[@]}"; do
    for file in ${patterns[$i]}; do
        if [ -e ${file} ]; then # avoid creating a 'zlib.*.spec' if it didn't match anything
            echo "Running jq --sort-keys '${queries[$i]}' ${file}"
            jq --sort-keys "${queries[$i]}" ${file} >${file}.filtered
            mv ${file}.filtered ${file}
        fi
    done
done
