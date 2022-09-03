#!/usr/bin/python3

import requests
from os.path import join
import json

BASEURL = "https://robovinci.xyz/api"

with open('api.token') as f:
    TOKEN = f.read().strip()

def do_get(url):
    rs = requests.get(url, headers={"Authorization": f"Bearer {TOKEN}"})
    return rs.json()

rs = do_get(join(BASEURL, "results", "user"))

for item in rs['results']:
    print(f"ID: {item['problem_id']}\tCost: {item['min_cost']}\tLast submission: {item['last_submitted_at']}")
