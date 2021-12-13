from datetime import timedelta
import logging
import logging
import logging.handlers
import os
import random

import pika
from timeloop import Timeloop

tl = Timeloop()

stream_handler = logging.StreamHandler()
stream_handler.setLevel(logging.INFO)

logging.basicConfig(
    level=logging.DEBUG,
    format='%(name)s - %(levelname)-8s - [ %(filename)s:%(lineno)s ] - %(funcName)s - %(message)s',
    handlers=[stream_handler]
)

for _logger in ("pika.connection", "pika.adapters", "pika.channel"):
    logging.getLogger(_logger).setLevel(logging.WARNING)

logger = logging.getLogger(__name__)


def random_message():
    word = ['Deer', 'Dog', 'Cat', 'Queen', 'Bird']
    return ' '.join(random.sample(word, 3))


@tl.job(interval=timedelta(minutes=1))
def main():
    connection = None
    channel = None
    try:
        url = os.environ.get('CLOUDAMQP_URL', 'amqp://guest:guest@rabbitmq/%2f')
        params = pika.URLParameters(url)
        params.socket_timeout = 5

        connection = pika.BlockingConnection(params)
        channel = connection.channel()
        channel.queue_declare(queue='python-testing')

    except Exception as err:
        logger.error("Could not establish connection to backend | err=%s", err, exc_info=True)


    if channel:
        try:
            message = random_message()
            channel.basic_publish(exchange='', routing_key='python-testing', body=message)
            logger.info("Message sent to consumer")
            if connection:
                connection.close()
        except Exception as err:
            logger.error("Could not send message | err=%s", err, exc_info=True)


if __name__ == "__main__":
    tl.start(block=True)
