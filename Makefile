all: clean compile

clean_release:
	rm -rf _build/default/rel

console: clean_release release
	_build/default/rel/simple_web_server/bin/simple_web_server console

docker-build:
	# DOCKER_BUILDKIT=1 docker build --build-arg BUILD_ID=1 --ssh default --secret id=secret-id,src=secret-file-location -t simple_web_server:latest .
	DOCKER_BUILDKIT=1 docker build --build-arg BUILD_ID=1 --ssh default  -t simple_web_server:latest .

include rebar3.mk
