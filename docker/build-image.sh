#!/usr/bin/env bash

set -eu

show_help() {
  echo
  echo "Usage: $0 [--push] [--no-cache] [--cleanup] [--help]"
  echo
  echo "Options:"
  echo
  echo "  --test            Run basic tests."
  echo "  --no-build        Do not build docker imeage (Useful with --test)."
  echo "  --push            Push generated image to docker hub."
  echo "  --no-cache        Remove any previously produced artifacts or built images."
  echo "  --cleanup         Remove all artifacts (except the built image) once finished."
  echo "  --pid1-version    Version of fpco/pid1 to use as an entrypoint."
  echo "  --help            Show this help."
  echo
}

TEST=false
NO_BUILD=false
PUSH=false
NO_CACHE=""
CLEANUP=false
PID1_VERSION="0.1.0.1"


while [[ $# -gt 0 ]]; do
  case "$1" in
    --help)
      show_help
      exit 0
      ;;
    --test)
      TEST=true
      shift
      ;;
    --no-build)
      NO_BUILD=true
      shift
      ;;
    --push)
      PUSH=true
      shift
      ;;
    --no-cache)
      NO_CACHE="--no-cache"
      shift
      ;;
    --cleanup)
      CLEANUP=true
      shift
      ;;
    --pid1-version=*)
      PID1_VERSION="${1#--pid1-version=}"
      shift
      ;;
    --pid1-version)
      PID1_VERSION="$2"
      shift 2
      ;;
    *)
      echo "Unknown option $0: $1" >&2
      show_help
      ;;
  esac
done


[[ $NO_CACHE = "--no-cache" ]] && rm -rf artifacts

if [ $NO_BUILD = false ]; then

  mkdir -p artifacts

  docker build $NO_CACHE --build-arg PID1_VERSION=$PID1_VERSION --tag fpco/wai-ldap:build -f Dockerfile-build .

  cd ..
  docker run --rm --user="$(id -u):$(id -g)" --volume="$PWD:/src" fpco/wai-ldap:build

  cd docker

  docker build $NO_CACHE --build-arg PID1_VERSION=$PID1_VERSION --tag fpco/wai-ldap:current .

  # Sanity check and get wai-ldap version for the docker image tag
  WAI_LDAP_VERSION=$(docker run --rm fpco/wai-ldap wai-ldap --version | cut -d, -f1 | cut -d' ' -f2)

  if [ -z "${WAI_LDAP_VERSION}" ]; then
    echo "Can not get the version of wai-ldap" >&2
    exit 1
  fi
fi

if [ $TEST = true ]; then
  docker run --name test-wai-ldap --detach \
         --volume="$PWD/test:/config" \
         --volume="$PWD/test/website:/website" \
         -e LDAP_URI="ldap://$(ifconfig docker0 | grep 'inet addr:' | cut -d: -f2 | awk '{print $1}'):3389" \
         --publish=3000:3000 \
         fpco/wai-ldap:current
  docker run --name test-openldap-container --publish 3389:389 --detach osixia/openldap:1.1.8
  # Give wai-ldap and LDAP server some time to start up
  sleep 2
  cat test/alexey.ldif | docker exec -i test-openldap-container ldapadd -x -H ldap://localhost -D "cn=admin,dc=example,dc=org" -w admin
  docker exec test-openldap-container \
         ldappasswd -x "cn=alexey,dc=example,dc=org" \
         -s "foobar123" -H ldap://localhost -D "cn=admin,dc=example,dc=org" -w admin
  # Validate redirect code
  ERROR_MSG=""
  RESPONSE_CODE=$(curl --write-out %{http_code} --silent --output /dev/null localhost:3000 || true)
  if [ $RESPONSE_CODE != 303 ]; then
    ERROR_MSG="* Server did not produce a redirect to login page!"
  fi
  # Validate failed login
  RESPONSE_CODE=$(curl -d "username=alexey&password=foobar" --write-out %{http_code} --silent --output /dev/null -X POST localhost:3000/_auth_middleware/ldap || true)
  if [ $RESPONSE_CODE != 403 ]; then
    ERROR_MSG="$ERROR_MSG\n* Server did not properly reject incorrect credentials!"
  fi
  # Validate ability to login
  RESPONSE_CODE=$(curl -d "username=alexey&password=foobar123" --write-out %{http_code} --silent --output /dev/null -X POST localhost:3000/_auth_middleware/ldap || true)
  if [ $RESPONSE_CODE != 303 ]; then
    ERROR_MSG="$ERROR_MSG\n* Server did not authenticate"
  fi
  # Cleanup test containers
  docker kill -s 15 test-openldap-container test-wai-ldap
  sleep 1
  docker rm test-openldap-container test-wai-ldap
  if [ -n "$ERROR_MSG" ]; then
    echo "Error: Some of the tests have failed:" >&2
    printf "${ERROR_MSG}\n" >&2
  else
    echo "Success: All tests have passed!"
  fi
fi


if [ $PUSH = true ]; then
  docker tag fpco/wai-ldap:current fpco/wai-ldap:latest
  docker tag fpco/wai-ldap:current fpco/wai-ldap:$WAI_LDAP_VERSION
  docker push fpco/wai-ldap:$WAI_LDAP_VERSION
  docker push fpco/wai-ldap:latest
fi

if [ $CLEANUP = true ]; then
  docker rm fpco/wai-ldap:build
  rm -rf artifacts
fi
