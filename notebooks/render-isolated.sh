#!/usr/bin/env bash
# Render in node-local temporary storage when another job has notebooks/.quarto
# open on the shared NFS filesystem. The final rsync is the only write back to
# the working tree.
set -euo pipefail

project_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
stage_dir=$(mktemp -d "${TMPDIR:-/tmp}/quarto-render.XXXXXXXX")
trap 'rm -rf -- "$stage_dir"' EXIT

# Quarto's Sass cache is a Deno KV database normally kept under ~/.cache.
# Put the XDG state alongside the staged project so simultaneous jobs cannot
# contend for that database either.
export XDG_CACHE_HOME="$stage_dir/xdg-cache"
export XDG_DATA_HOME="$stage_dir/xdg-data"
export XDG_RUNTIME_DIR="$stage_dir/xdg-runtime"
mkdir -p "$XDG_CACHE_HOME" "$XDG_DATA_HOME" "$XDG_RUNTIME_DIR"

# The staged copy gets its own .quarto directory. Do not inherit Quarto's
# shared scratch state or previously rendered output.
rsync -a \
  --exclude '.git' \
  --exclude '.jj' \
  --exclude 'docs' \
  --exclude 'notebooks/.quarto' \
  --exclude 'notebooks/_site' \
  --exclude 'notebooks/site_libs' \
  "$project_dir/" "$stage_dir/project/"

(
  cd "$stage_dir/project"
  quarto render notebooks
)

# `docs/` is configured as this Quarto project's generated output directory.
# --delete makes it an exact copy of the successful staged render.
rsync -a --delete "$stage_dir/project/docs/" "$project_dir/docs/"
