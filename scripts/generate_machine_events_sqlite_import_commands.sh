dir=$1
out="import_machine_events_sqlite_commands.txt"

files=`ls $dir`

echo "DROP TABLE machine_events;" > $out
echo "CREATE TABLE machine_events (time, machineId, eventType, platformdId,
                                   cpu, memory);" >> $out
echo ".separator ','" >> $out

for file in $files; do
    echo ".import ${dir}/${file} machine_events" >> $out
done

echo "CREATE INDEX time_idx ON machine_events (time ASC);" >> $out
echo "CREATE INDEX machine_idx ON machine_events (machineId ASC);" >> $out
