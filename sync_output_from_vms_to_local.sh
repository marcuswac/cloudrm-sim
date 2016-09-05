HOSTS="10.4.1.74 10.4.2.98 10.4.5.111 10.4.1.191"
USERNAME="ubuntu"
DIR="./cloudrm-sim/output"
#DIR="./volume-resultados-sim/cloudrm-sim/output"

for host in $HOSTS
do
    rsync -avz ${USERNAME}@${host}:${DIR}/res_*_ac.csv output/
    rsync -avz ${USERNAME}@${host}:${DIR}/res_*_vm-avail-summary.txt output/
done
