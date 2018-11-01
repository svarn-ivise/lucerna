cp -R ~/.docker/machine/machines/my_machine . && \
  cp ~/.docker/machine/certs/* ./my_machine/certs
  
sed -i.bak 's/machine\/certs/machine\/machines\/my_machine\/certs/' ./my_machine/config.json
sed -i.bak 's/your_username/her_username/' ./my_machine/config.json