all: clean compile

clean_release:
	rm -rf _build/default/rel

console: clean_release release
	_build/default/rel/simple_web_server/bin/simple_web_server console

docker-build:
	# * build-arg (https://docs.docker.com/engine/reference/commandline/build/#set-build-time-variables---build-arg)
	# 	This flag allows you to pass the build-time variables that are accessed like regular environment variables in the RUN instruction of the Dockerfile.
	#
	# * secret
	# 	Secret file to expose to the build (only if BuildKit enabled): id=mysecret,src=/local/secret
	#
	# * tag (https://docs.docker.com/engine/reference/commandline/build/#tag-an-image--t)
	#	Tags an image
	DOCKER_BUILDKIT=1 docker build --build-arg BUILD_ID=1 --ssh default --secret id=stash-miniclip-com-pem.key,src=stash-miniclip-com-pem.key --tag simple_web_server:latest .

include rebar3.mk

