#!/usr/bin/env bash

stack test --flag scheduler:library-only --flag scheduler:dev --coverage --fast
