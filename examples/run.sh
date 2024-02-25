#!/bin/bash

for file in *; do
  if [ -f "$file" ]; then
    # Skip the script file itself
    if [ "$file" != "$(basename "$0")" ]; then
      echo "Running cargo run for $file"
      cargo run "$file" -- --debug
      echo "----------------------------------------------"
    fi
  fi
done
