#!/bin/bash

TOKEN=$(cat api.token)
PROBLEM=$1
SRC=$2

curl --header "Authorization: Bearer $TOKEN" -F file=@$SRC https://robovinci.xyz/api/submissions/$PROBLEM/create

