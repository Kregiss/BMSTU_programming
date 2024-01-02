#!/bin/bash
project_path=$1
tmpfile=$(mktemp)
count_lines() 
{
    file="$1"
    tmpfile="$2"
    lines=$(grep -v '^\s*$' "$file" | wc -l)
    echo "$lines" >> "$tmpfile"
}
export -f count_lines
find "$project_path" -type f \( -name "*.c" -o -name "*.h" \) -exec bash -c 'count_lines \
 "$0" "$1"' {} "$tmpfile" \;
total_lines=$(awk '{sum += $1} END {print sum}' "$tmpfile")
echo "Общее число непустых строк: $total_lines"
rm "$tmpfile"