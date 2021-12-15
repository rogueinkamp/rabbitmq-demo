# RABBITMQ Testing
Testing repo for rabbitmq messaging between python pub/sub

Built this in an afternoon. Really basic.

Note I had to have a local user rabbitmq with uid 1001 and group rabbitmq with uid of 1000 for volumes to mount successfully

## TODO

- Spawn more than one consumer [x] DONE
- Asyncio spam the queue and see how well it performs
- Logging to be less verbose so we catch errors and not spam (looks like pika.channel, pika.adapters, etc. are super chatty) [x] DONE
- Review config options
- Implement rabbitmq fanout so that each subscriber gets a copy of the data
- Do something useful with messages (Start a mutlithreaded fibonacci computation / fizzbuzz or something else fun)
- Figure out what ACK is good for and what to do if we do not get an ACK
