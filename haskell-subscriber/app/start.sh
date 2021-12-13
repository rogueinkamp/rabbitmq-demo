#!/bin/bash

echo "Waiting rabbitmq to launch on port 5672..."

while ! nc -z rabbitmq 5672; do
    sleep 3
done

echo "rabbitmq service is available!"
