#!/bin/sh

# Copyright 2026 canardlapin
# Licensed under the Apache License, Version 2.0.

set -eu

EYELINKER_VERSION=0.2.2
EYELINKER_SOURCE_SHA256=baa08d892a8019da90d0ea5d11f9002a0c340aa3be4b365128ab7cb804c61973

sha256_file() {
  if command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
  elif command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  else
    echo "no SHA-256 executable found (need shasum or sha256sum)" >&2
    exit 2
  fi
}

require_file() {
  if [ ! -f "$2" ]; then
    echo "$1 file does not exist: $2" >&2
    exit 2
  fi
}

require_digest() {
  actual=$(sha256_file "$2")
  if [ "$actual" != "$3" ]; then
    echo "$1 digest mismatch: file=$2 expected=$3 actual=$actual" >&2
    exit 2
  fi
}

usage() {
  cat >&2 <<'EOF'
Usage:
  regenerate.sh eyelinker INPUT.asc OUTPUT.tsv FIXTURE_ID ORACLE_ID \
    EYELINKER_0.2.2_SOURCE.tar.gz R_LIBRARY

  EDF_SDK_INCLUDE=/path/to/include \
  EDF_SDK_LIB=/path/to/lib \
  EDFAPI_LIBRARY=/path/to/libedfapi.dylib \
    regenerate.sh edf INPUT.edf OUTPUT.tsv FIXTURE_ID ORACLE_ID [CONSISTENCY]

The EDF mode compiles only this repository's adapter and links it against a
licensed, user-installed SR Research EDF Access API. It never copies the
vendor headers, library, or EDF bytes into the repository.
EOF
}

if [ "$#" -lt 1 ]; then
  usage
  exit 2
fi

mode=$1
shift
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

case "$mode" in
  eyelinker)
    if [ "$#" -ne 6 ]; then
      usage
      exit 2
    fi
    input=$1
    output=$2
    fixture_id=$3
    oracle_id=$4
    package_source=$5
    r_library=$6
    adapter="$script_dir/eyelinker_reference.R"
    require_file "ASC input" "$input"
    require_file "eyelinker source" "$package_source"
    require_file "eyelinker adapter" "$adapter"
    require_digest "eyelinker $EYELINKER_VERSION source" "$package_source" \
      "$EYELINKER_SOURCE_SHA256"
    if [ ! -d "$r_library/eyelinker" ]; then
      echo "R library does not contain eyelinker $EYELINKER_VERSION: $r_library" >&2
      exit 2
    fi
    input_digest=$(sha256_file "$input")
    adapter_digest=$(sha256_file "$adapter")
    R_LIBS_USER="$r_library" Rscript "$adapter" \
      --input "$input" \
      --output "$output" \
      --fixture-id "$fixture_id" \
      --oracle-id "$oracle_id" \
      --tool-digest "$EYELINKER_SOURCE_SHA256" \
      --adapter-digest "$adapter_digest" \
      --input-digest "$input_digest"
    ;;

  edf)
    if [ "$#" -lt 4 ] || [ "$#" -gt 5 ]; then
      usage
      exit 2
    fi
    input=$1
    output=$2
    fixture_id=$3
    oracle_id=$4
    consistency=${5:-1}
    adapter="$script_dir/edfapi_reference.c"
    : "${EDF_SDK_INCLUDE:?EDF_SDK_INCLUDE must name the licensed SDK include directory}"
    : "${EDF_SDK_LIB:?EDF_SDK_LIB must name the licensed SDK library directory}"
    : "${EDFAPI_LIBRARY:?EDFAPI_LIBRARY must name the exact libedfapi file}"
    require_file "EDF input" "$input"
    require_file "EDF adapter" "$adapter"
    require_file "EDF Access API library" "$EDFAPI_LIBRARY"
    if [ ! -f "$EDF_SDK_INCLUDE/edf.h" ]; then
      echo "licensed SDK header is missing: $EDF_SDK_INCLUDE/edf.h" >&2
      exit 2
    fi
    task_temp=$(mktemp -d "${TMPDIR:-/tmp}/eyes4s-edf-oracle.XXXXXX")
    trap 'rm -rf "$task_temp"' EXIT HUP INT TERM
    compiler=${CC:-cc}
    "$compiler" -std=c11 -Wall -Wextra -Werror -pedantic \
      -I"$EDF_SDK_INCLUDE" "$adapter" -L"$EDF_SDK_LIB" -ledfapi -lm \
      -o "$task_temp/edfapi_reference"
    input_digest=$(sha256_file "$input")
    adapter_digest=$(sha256_file "$adapter")
    tool_digest=$(sha256_file "$EDFAPI_LIBRARY")
    if [ "$(uname -s)" = "Darwin" ]; then
      DYLD_LIBRARY_PATH="$EDF_SDK_LIB${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}" \
        "$task_temp/edfapi_reference" \
          --input "$input" --output "$output" \
          --fixture-id "$fixture_id" --oracle-id "$oracle_id" \
          --tool-digest "$tool_digest" --adapter-digest "$adapter_digest" \
          --input-digest "$input_digest" --consistency "$consistency"
    else
      LD_LIBRARY_PATH="$EDF_SDK_LIB${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
        "$task_temp/edfapi_reference" \
          --input "$input" --output "$output" \
          --fixture-id "$fixture_id" --oracle-id "$oracle_id" \
          --tool-digest "$tool_digest" --adapter-digest "$adapter_digest" \
          --input-digest "$input_digest" --consistency "$consistency"
    fi
    ;;

  *)
    usage
    exit 2
    ;;
esac

echo "oracle=$output sha256=$(sha256_file "$output")"
