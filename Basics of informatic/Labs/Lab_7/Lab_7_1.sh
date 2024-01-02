#!/bin/bash
period=$1
program_path=$2
if [ -z "$period" ] || [ -z "$program_path" ]; then
    echo "Usage: $0 <period in minutes> <path to program>"
    exit 1
fi
run_program()
{
    output_file="output_$(date +'%Y%m%d%H%M%S').log"
    if [ ! -x "$program_path" ]; then
        echo "The file '$program_path' is not executable or does not exist."
        exit 2
    fi
    "$program_path" > "$output_file" 2>&1
}
rand=$RANDOM
PID=-1
log_file_name="logs$rand.txt"
while true; do
    if ! pgrep -fx "$program_path" > /dev/null; then
        run_program &
        PID=$!
        echo "Successful launch; logs in $log_file_name"
        echo "Start time is $(date +%s)s" >> "$log_file_name"
    else
        echo "Program is already running..; logs in $log_file_name"
    fi
    sleep "${period}m"
done