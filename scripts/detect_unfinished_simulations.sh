#!/bin/bash
FILES=$*
END_TIME=8354
COLUMN=8
DELIMITER=","

for file in $FILES
do
    last_time=`tail -n 1 $file | cut -d $DELIMITER -f $COLUMN`
    # Print name if string is empty or is not a number or the last time is lower than the end time
    if [ -z $last_time ] || [[ $last_time =~ '^[0-9]+$' ]] || [ "$last_time" -lt "$END_TIME" ]
    then
        echo "$file"
    fi
done
