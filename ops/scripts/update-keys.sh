for machine in $(docker-machine ls -f {{.Name}})
do
	cat ~/.docker/machine/machines/$machine/config.json
done 
