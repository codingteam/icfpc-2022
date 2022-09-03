#!/bin/bash

if [ ! -e api.token ]
then
    echo "Please put the API token into a file called api.token"
    exit 1
fi

TOKEN=$(cat api.token)

curl --header "Authorization: Bearer $TOKEN" https://robovinci.xyz/api/results/user
