#!/usr/bin/env bash

start_date=$(git log --format="%ad" --date=short | tail -1)
start_date=$(date -d "$start_date" +%Y-%m-01) 

# Iterate until the current date
current_date=$(date +%Y-%m-01) 

while [[ "$start_date" < "$current_date" ]] 
do
  # Calculate end of the month
  end_of_month=$(date -d "$start_date + 1 month - 1 day" +%Y-%m-%d) 

  # Interactive rebase for the month's commits
  commits=$(git log --after="$start_date" --before="$end_of_month" --format="%H")
  if [[ $(echo "$commits" | wc -l) -gt 1 ]]; then
    first_commit=$(echo "$commits" | tail -1)
    last_commit=$(echo "$commits" | head -1)

    echo $start_date
    echo $commits
  fi


  # Move to the next month
  start_date=$(date -d "$start_date + 1 month" +%Y-%m-01) 
done

