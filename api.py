#!/usr/bin/python3

import requests
from os.path import join
import json
import sys

BASEURL = "https://robovinci.xyz/api"

try:
    with open('api.token') as f:
        TOKEN = f.read().strip()
except FileNotFoundError:
    print("Please put the API token into a file called api.token")
    sys.exit(1)

def do_get(url):
    rs = requests.get(url, headers={"Authorization": f"Bearer {TOKEN}"})
    return rs.json()

rs = do_get(join(BASEURL, "results", "user"))

if 'results' in rs:
    for item in rs['results']:
        print(f"ID: {item['problem_id']}\tCost: {item['min_cost']}\tLast submission: {item['last_submitted_at']}")
else:
    print(rs)
