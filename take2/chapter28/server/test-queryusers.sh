#!/usr/bin/env bash

curl -v http://localhost:3000 -d '{"tag": "QueryUsersRequest", "values": [{"authToken": "ff48af9a-c3fe-4258-a1ec-c66ac50e7ee7"}]}'
