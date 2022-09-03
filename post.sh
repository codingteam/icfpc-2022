#!/bin/bash

if [ ! -e api.token ]
then
    echo "Please put the API token into a file called api.token"
    exit 1
fi

if [ $# -ne 2 ]
then
    echo "Usage: $0 problem_no FILE.isl"
    exit 2
fi

TOKEN=$(cat api.token)
PROBLEM=$1
SRC=$2

curl --header "Authorization: Bearer $TOKEN" -F file=@$SRC https://robovinci.xyz/api/submissions/$PROBLEM/create

