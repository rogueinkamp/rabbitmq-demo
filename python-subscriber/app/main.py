import logging
import logging.handlers
import os

import time
import pika

stream_handler = logging.StreamHandler()
stream_handler.setLevel(logging.INFO)

logging.basicConfig(
    level=logging.INFO,
    format='%(name)s - %(levelname)-8s - [ %(filename)s:%(lineno)s ] - %(funcName)s - %(message)s',
    handlers=[stream_handler]
)

logger = logging.getLogger("python-subscriber")

def callback(ch, method, properties, body):
    logger.debug(
        "CALLBACK | ch=%s, method=%s, properties=%s, body=%s",
        ch,
        method,
        properties,
        body
    )
    process_function(body)


def process_function(msg):
    msg = msg.decode() if isinstance(msg, bytes) else str(msg)
    logger.info("PYTHON_SUBSCRIBER_MESSAGE_RECEIVED: %s", msg)
    time.sleep(5)
    logger.info("Processing finished");
    return


def main():
    # Access the CLODUAMQP_URL environment variable and parse it (fallback to localhost)
    url = os.environ.get('CLOUDAMQP_URL', 'amqp://guest:guest@rabbitmq:5672/%2f')
    params = pika.URLParameters(url)
    connection = pika.BlockingConnection(params)
    channel = connection.channel()
    channel.exchange_declare(exchange='test-exchange', exchange_type='direct', durable=True)
    channel.queue_declare(queue='python-testing')
    # set up subscription on the queue
    channel.basic_consume(
        'python-testing',
        callback,
        auto_ack=True
    )

    # start consuming (blocks)
    channel.start_consuming()
    connection.close()


if __name__ == "__main__":
    main()
