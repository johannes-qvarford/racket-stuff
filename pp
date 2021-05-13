#!/bin/bash
f="$(mktemp)"
raco read "$1" >"$f"
cat "$f" >"$1"