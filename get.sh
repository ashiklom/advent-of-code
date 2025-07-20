#!/usr/bin/env bash

if [[ ! -f token ]]; then
  echo "Missing token file 'token'"
  exit 1
fi

SESSION="$(cat token)"

for year in {2020..2024}; do
  for day in {1..25}; do
    dd=$(printf "%02d" $day)
    ddir="$year/$dd"
    echo "Retrieving $ddir"
    mkdir -p $ddir
    if [[ ! -f "$ddir/desc.txt" ]]; then
      if html2text -v &> /dev/null; then
        curl -s https://adventofcode.com/$year/day/$day | html2text > "$ddir/desc.txt"
      else
        echo "html2text not available. Falling back to HTML"
        if [[ ! -f "$ddir/desc.html" ]]; then
          curl -s https://adventofcode.com/$year/day/$day -o "$ddir/desc.html"
        fi
      fi
    fi
    if [[ ! -f "$ddir/input" ]]; then
      curl -s https://adventofcode.com/$year/day/$day/input --cookie "session=$SESSION" -o "$ddir/input"
    fi
  done
done
