#!/bin/sh

# disable exit-on-error mode, if some command returns a nonzero status
# then don't crash immediately
set +e

for i in {1..5}
do
    curl -X POST http://simple-web-service:8585/v1/setup/create_tables
    if [ $? -eq 0 ]
    then
        break
    fi
    echo "curl failed with $?, retrying..."
    sleep 5
done

# re-enable exit-on-error mode since apparently localstack has this
# enabled
set -e
