#!/usr/bin/env racket
#lang rash

find . -type f | grep -v "/\\." | xargs -n 1 -d "\n" racket-pretty-printer --inplace -f