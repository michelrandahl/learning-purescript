#!/usr/bin/env bash

curl -v http://localhost:3000 -d '{"tag": "CreateUserRequest", "values": [{"authToken": "a7b68570-cec8-48d6-8d2b-03eb26a31f40", "user": {"userName": "foo", "password": "foobar", "temporaryPassword": true, "admin": true, "firstName": "Foo", "lastName": "Bar"}}]}'
