#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

main() {
    local source_=$1; shift
    local target=$1; shift

    cp "$source_" "$target"
    echo -e '\nlet () = main ()' >> "$target"
}

source_=$1; shift
target=$1; shift
main "$source_" "$target"
