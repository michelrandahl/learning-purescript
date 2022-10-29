#!/usr/bin/env bash

#curl -v http://localhost:3000 -d '{"LogonRequest": {"contents": {"userName": "admin", "password": "admin"}}}'
curl -v http://localhost:3000 -d '{"tag": "LogonRequest", "values": [{"userName": "admin", "password": "admin"}]}'
#curl -v http://localhost:3000 -d '{"tag": "LogonRequest", "values": [{"userName": "foo", "password": "foobar"}]}'
