#!/bin/bash

AWSKEY=$(aws configure get default.aws_access_key_id)
AWSSECRET=$(aws configure get default.aws_secret_access_key)
LOCATION="us-west-2"
OUTPUT='"output.txt"'
CONFIG='"/root/.s3cfg"'
CFGSUB="'/usr/bin/envsubst < $OUTPUT > $CONFIG;'"
COMMAND="sudo su -c 'cd /root && \
    curl -L https://raw.githubusercontent.com/svarn-ivise/api/master/aws.s3cfg -o output.txt && \
    ACCESS_KEY=$AWSKEY \
    SECRET_KEY=$AWSSECRET \
    LOCATION=$LOCATION \
    bash -c $CFGSUB'"

for MACHINE in $(docker-machine ls -f {{.Name}})
do
    echo $MACHINE
    echo $COMMAND
    
    docker-machine ssh $MACHINE "$COMMAND"
done

# ACCESS_KEY=AWSKEY \
# SECRET_KEY=AWSSECRET \
# LOCATION=us-west-2 \
# bash -c '/usr/bin/envsubst < "output.txt" > "/root/.s3cfg";'