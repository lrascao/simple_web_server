all: clean compile

clean_release:
	rm -rf _build/default/rel

console: clean_release release
	_build/default/rel/simple_web_server/bin/simple_web_server console

docker-login:
	# aws --profile <aws-profile> ecr get-login-password --region <aws-region> \
	# | docker login \
	# 	--username AWS \
	# 	--password-stdin <aws-ecr-endpoint>
	docker login


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
	# 		$(aws --profile <aws-profile> ecr get-login --no-include-email --region <aws-region>)
	# 	awscli v2:
    # 		aws --profile <aws-profile> ecr get-login-password --region us-west-2 \
    #       	| docker login \
    #            	--username AWS \
    #           	--password-stdin <aws-ecr-endpoint>
	#
	#  below we're pushing to docker hub so no need to worry about any of that
	docker tag simple-web-server:latest lrascao/simple-web-server:latest
	docker push lrascao/simple-web-server:latest

k8s-secrets:
	kubectl create secret docker-registry regcred --docker-server=<aws-ecr-endpoint> --docker-username=AWS --docker-password=`aws --profile <aws-profile> ecr get-login-password --region <aws-region>` --docker-email=<email>

include rebar3.mk

