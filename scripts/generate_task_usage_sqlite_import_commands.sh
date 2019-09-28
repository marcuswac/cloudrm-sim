dir=$1
out="import_task_usage_sqlite_commands.txt"

files=`ls $dir`

echo "DROP TABLE task_usage;" > $out
echo "CREATE TABLE task_usage (startTime, endTime, jobId, taskId, machineId,
      cpuRate, canMemory, assMemory, unCache, totCache, maxMemory, diskTime,
      diskSpace, maxCpuRate, maxDiskTime, cpi, mapi, samplePortion, aggType);" >> $out
echo ".separator ','" >> $out

for file in $files; do
    echo ".import ${dir}/${file} task_usage" >> $out
done

echo "CREATE INDEX startTime_idx ON task_usage (startTime ASC);" >> $out
echo "CREATE INDEX task_time_idx ON task_usage (startTime ASC, jobId, taskId);" >> $out
