#!/usr/bin/env bash

#curl -v http://localhost:3000 -d '{"LogonRequest": {"contents": {"userName": "admin", "password": "admin"}}}'
curl -v http://localhost:3000 -d "{\"tag\": \"LogoffRequest\", \"values\": [{\"authToken\": \"$1\"}]}"
