#!/bin/bash

cat | prettier  --plugin-search-dir=. --stdin-filepath $1
