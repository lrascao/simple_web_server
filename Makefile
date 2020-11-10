all: clean compile

clean_release:
	rm -rf _build/default/rel

console: clean_release release
	_build/default/rel/simple_web_server/bin/simple_web_server console

docker-login:
	aws --profile miniclippool ecr get-login-password --region us-west-2 \
	| docker login \
		--username AWS \
		--password-stdin 879202131194.dkr.ecr.us-west-2.amazonaws.com


docker-build:
	# * --build-arg (https://docs.docker.com/engine/reference/commandline/build/#set-build-time-variables---build-arg)
	# 		This flag allows you to pass the build-time variables that are accessed like regular environment variables in the RUN instruction of the Dockerfile.
	#
	# * --ssh (https://docs.docker.com/develop/develop-images/build_enhancements/#using-ssh-to-access-private-data-in-builds)
	# 		Allow the Docker Engine to forward SSH agent connections
	#
	# * --secret
	# 		Secret file to expose to the build (only if BuildKit enabled): id=mysecret,src=/local/secret
	#
	# * --tag (https://docs.docker.com/engine/reference/commandline/build/#tag-an-image--t)
	#		Tags an image
	#
	DOCKER_BUILDKIT=1 docker build --build-arg BUILD_ID=1 --ssh default --secret id=private-key-repo-pem.key,src=private-key-repo-pem.key --tag simple-web-server:latest .

docker-push:
	# before tagging and pushing to ECR you'll need to obtain AWS credentials:
	# 	awscli v1:
	# 		$(aws --profile miniclippool ecr get-login --no-include-email --region us-west-2)
	# 	awscli v2:
    # 		aws --profile miniclippool ecr get-login-password --region us-west-2 \
    #       	| docker login \
    #            	--username AWS \
    #           	--password-stdin 879202131194.dkr.ecr.us-west-2.amazonaws.com
	docker tag simple-web-server:latest 879202131194.dkr.ecr.us-west-2.amazonaws.com/simple-web-server:latest
	docker push 879202131194.dkr.ecr.us-west-2.amazonaws.com/simple-web-server:latest

# local-up:
# 	docker-compose --file docker-compose.local.yaml --file docker-compose.yaml up

# local-down:
# 	docker-compose --file docker-compose.local.yaml --file docker-compose.yaml down

# local-console:
# 	docker exec --tty --interactive simple_web_server_simple-web-service_1 /srv/service/bin/simple_web_server remote_console

# fargate-up:
# 	# https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cmd-ecs-cli-compose-service-up.html
# 	ecs-cli compose --project-name simple-web-server --file docker-compose.yaml --file docker-compose.fargate.yaml --ecs-params ecs-params.yaml --cluster-config simple-web-server --ecs-profile simple-web-server-profile service up --create-log-groups 

# fargate-down:
# 	ecs-cli compose --project-name simple-web-server --file docker-compose.yaml --file docker-compose.fargate.yaml --ecs-params ecs-params.yaml --cluster-config simple-web-server --ecs-profile simple-web-server-profile service down

# fargate-ps:
# 	ecs-cli compose --project-name simple-web-server --file docker-compose.yaml --file docker-compose.fargate.yaml --cluster-config simple-web-server --ecs-profile simple-web-server-profile service ps

k8s-secrets:
	kubectl create secret docker-registry regcred --docker-server=879202131194.dkr.ecr.us-west-2.amazonaws.com --docker-username=AWS --docker-password=`aws --profile miniclippool ecr get-login-password --region us-west-2` --docker-email=luis.rascao@miniclip.com

include rebar3.mk

