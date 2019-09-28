dir=`dirname $0`
input=$*
cmd="pypy"
script="$dir/extract_gtrace_task_info.py"
output="task-events.txt"
tmp_output="${output}_tmp"

$cmd $script $input 
sort -n -k1 $output > $tmp_output
mv $tmp_output $output
