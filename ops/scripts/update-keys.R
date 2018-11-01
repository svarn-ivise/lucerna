#!/usr/lib/R/bin/Rscript

suppressMessages(library(jsonlite))
suppressMessages(library(dplyr))

config.file <- readLines('~/.aws/credentials')
aws.key <- gsub(".*= ","",config.file[config.file %>% grepl("aws_access_key",.)])
aws.secret <- gsub(".*= ","",config.file[config.file %>% grepl("aws_secret_access_key",.)])

quiet <- lapply(system("docker-machine ls -f {{.Name}}",intern = T),function(machine){
  
  machine_path <- paste0("~/.docker/machine/machines/",machine,"/config.json")
  
  x <- read_json(machine_path)
  x$Driver$AccessKey <- aws.key
  x$Driver$SecretKey <- aws.secret
  
  write_json(x, machine_path, auto_unbox=T, null = "null", pretty=T)
  
})