#!/bin/bash

# Replace <database> below with name of database
db="<database>"

# PostgreSQL user
psql_user="postgres"

# Modify according to host environment
psql_cmd="sudo -u $psql_user psql -d $db -c"

case $1 in
    list)
        eval "$psql_cmd \"SELECT client, key FROM trombone_keys;\"" | tail -n+3 | head -n-2 | awk '{printf "%-20s %-40s\n", $1, $3}'
        ;;
    register)
        eval "$psql_cmd \"INSERT INTO trombone_keys (client, key) VALUES ('$2', encode(digest(random()::text, 'sha1'), 'hex'));\""
        ;;
    renew)
        eval "$psql_cmd \"UPDATE trombone_keys SET key = encode(digest(random()::text, 'sha1'), 'hex') WHERE client = '$2';\""
        ;;
    revoke)
        eval "$psql_cmd \"DELETE FROM trombone_keys WHERE client = '$2';\""
        ;;
    *)
        echo "Usage: $0 {list|register|renew|revoke} [client]"
        exit 1
esac
