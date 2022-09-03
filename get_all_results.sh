#!/bin/bash

TOKEN=$(cat api.token)

curl --header "Authorization: Bearer $TOKEN" https://robovinci.xyz/api/results/user
