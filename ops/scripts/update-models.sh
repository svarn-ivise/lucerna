#!/bin/bash

docker-machine create --driver amazonec2 \
                --amazonec2-userdata ~/yaml/worker.yml \
                --amazonec2-instance-type 't2.medium' \
                --amazonec2-vpc-id vpc-32c6264a \
                --amazonec2-region us-west-2 worker 

# for MACHINE in $(docker-machine ls --format '{{.Name}}')
# do 
# 	docker-machine ssh $MACHINE 'sudo -i s3cmd get --force s3://modelstorage/dynamic.rds /models/dynamic.rds' 
# done

echo 'complete'