#!/usr/bin/env bash
# Build each Eyes Studio source pin at its exact commit and publish it to the
# local Ivy repository as version 0.0.0-<full revision> (S0.3).
#
#   studio/publish-pins.sh [--force] [--source NAME=GIT_SOURCE]... [NAME...]
#
# NAME is a provider in studio/pins.properties (default: all of them). The
# script fetches the pinned revision from the recorded repository, or from an
# explicit --source (a local checkout or another Git URL), checks it out
# detached in a fresh directory, verifies the commit, and runs the provider's
# own sbt build with the fixed version. Only committed history is used: a
# working tree's uncommitted changes never reach the published artifact.
#
# A pin whose version is already in the local Ivy repository is skipped
# unless --force is given. STUDIO_PINS_WORKDIR overrides the scratch directory
# (default target/studio-pins).
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
pins="$root/studio/pins.properties"
work="${STUDIO_PINS_WORKDIR:-$root/target/studio-pins}"
ivy_local="$HOME/.ivy2/local" # sbt's default local repository, which studio resolves
export GIT_TERMINAL_PROMPT=0 # a missing repository fails instead of prompting

die() { echo "publish-pins: $*" >&2; exit 1; }
prop() {
  local value
  value="$(sed -n "s/^$1=//p" "$pins")"
  [[ -n "$value" ]] || die "$pins has no $1"
  printf '%s' "$value"
}

force=0
names=()
sources=() # NAME=GIT_SOURCE entries; bash 3.2 (macOS) has no associative arrays
while (($#)); do
  case "$1" in
    --force) force=1 ;;
    --source)
      [[ $# -ge 2 && "$2" == *=* ]] || die "--source needs NAME=GIT_SOURCE"
      sources+=("$2")
      shift ;;
    -h|--help) sed -n '2,17p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*) die "unknown option $1" ;;
    *) names+=("$1") ;;
  esac
  shift
done
((${#names[@]})) || names=(intaglio scaladock)

for name in "${names[@]}"; do
  repository="$(prop "$name.repository")"
  revision="$(prop "$name.revision")"
  organization="$(prop "$name.organization")"
  read -r -a modules <<<"$(prop "$name.modules")"
  [[ "$revision" =~ ^[0-9a-f]{40}$ ]] || die "$name.revision is not a full Git SHA: $revision"
  version="0.0.0-$revision"
  source="$repository"
  for entry in ${sources[@]+"${sources[@]}"}; do
    if [[ "${entry%%=*}" == "$name" ]]; then source="${entry#*=}"; fi
  done

  # Written only after every module of the pin has published, so an
  # interrupted run is repeated rather than skipped.
  marker="$ivy_local/$organization/.eyes4s-studio-pin-$version"
  if ((!force)) && [[ -f "$marker" ]]; then
    echo "publish-pins: $name $version already in $ivy_local; skipping (use --force to rebuild)"
    continue
  fi

  checkout="$work/$name-$revision"
  rm -rf "$checkout"
  mkdir -p "$checkout"
  echo "publish-pins: fetching $name $revision from $source"
  git -C "$checkout" init --quiet
  git -C "$checkout" fetch --quiet --depth 1 "$source" "$revision" ||
    die "cannot fetch $name $revision from $source (for an unpushed repository pass --source $name=<checkout>)"
  git -C "$checkout" checkout --quiet --detach FETCH_HEAD
  actual="$(git -C "$checkout" rev-parse HEAD)"
  [[ "$actual" == "$revision" ]] || die "$name checked out $actual, expected $revision"

  # Scaladoc jars are not needed to compile against a pin and dominate the
  # publishing time; binaries, sources and descriptors are published.
  commands=("set ThisBuild / version := \"$version\"")
  for module in "${modules[@]}"; do
    commands+=("set $module / Compile / packageDoc / publishArtifact := false")
  done
  for module in "${modules[@]}"; do commands+=("$module/publishLocal"); done
  echo "publish-pins: publishing $organization ${modules[*]} at $version"
  # CI is unset because a provider may make warnings fatal under it (scaladock
  # does, and its pinned commit has warnings). The provider's own CI gates its
  # warnings; this build only consumes the commit.
  (cd "$checkout" && env -u CI sbt -batch "${commands[@]}")
  printf '%s\n' "${modules[@]}" >"$marker"
done
