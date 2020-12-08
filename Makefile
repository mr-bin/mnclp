
all: compose_down compose_up sleep init_ddb

sleep:
	sleep 20

build_env:
	pip install -r ./_localdev/requirements.txt

init_ddb:
	python ./_localdev/ddb_init.py

compose_up:
	docker-compose -f ./_localdev/docker-compose.yml up -d

compose_down:
	docker-compose -f ./_localdev/docker-compose.yml down

generate_client:
	protoc --python_out=./_client --proto_path=./apps/mnclp/proto kv.proto

deploy:
	ansible-playbook ./_deployment/playbook.yml

get_ip_address:
	aws ec2 describe-instances --filter Name="instance-state-name",Values="running" | jq .Reservations[0].Instances[0].PublicIpAddress
