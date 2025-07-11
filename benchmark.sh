#!/usr/bin/env bash

for script in $(find 2024 -name "*.py" | sort); do
  echo $script
  time python $script
  echo "#############"
done
