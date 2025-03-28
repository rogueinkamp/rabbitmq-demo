from datetime import timedelta
import json
import logging
import os
import random
import sys
import time
from typing import Literal

import pika
from pika.adapters.blocking_connection import BlockingChannel

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

logger = logging.getLogger("python-publisher")


class NoExchangeException(Exception):
    """Custom No Exchange Exception."""
    pass


class Jobs():
    def __init__(self):
        pass

    def generate_prime_calculation_job(self) -> str:
      """Generates a JSON message for prime number calculation."""
      message_id = f"prime-{int(time.time())}"
      # Generate a large number, but not excessively so.
      data = random.randint(100000000, 1000000000)
      timestamp = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
      return json.dumps({
          "message_id": message_id,
          "work_type": "prime_number_calculation",
          "data": data,
          "timestamp": timestamp,
      })

    def generate_fibonacci_job(self) -> str:
      """Generates a JSON message for Fibonacci sequence calculation."""
      message_id = f"fib-{int(time.time())}"
      # Generate a moderate Fibonacci number index.
      data = random.randint(30, 40) #This range will make the calculation take some time, but not forever.
      timestamp = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
      return json.dumps({
          "message_id": message_id,
          "work_type": "fibonacci",
          "data": data,
          "timestamp": timestamp,
      })

    def generate_matrix_multiplication_job(self) -> str:
      """Generates a JSON message for matrix multiplication."""
      message_id = f"matrix-{int(time.time())}"
      # Generate two 3x3 matrices with random integers.
      matrix1 = [[random.randint(1, 10) for _ in range(3)] for _ in range(3)]
      matrix2 = [[random.randint(1, 10) for _ in range(3)] for _ in range(3)]
      timestamp = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
      return json.dumps({
          "message_id": message_id,
          "work_type": "matrix_multiplication",
          "data": {
              "matrix1": matrix1,
              "matrix2": matrix2,
          },
          "timestamp": timestamp,
      })

    def generate_metrics(self):
        """generate some fake metrics to send to the subscribers."""
        available_operations = [
            "generate_prime_calculation_job",
            "generate_fibonacci_job",
            "generate_matrix_multiplication_job",
        ]
        operation = random.choice(available_operations)
        try:
            selected_function = getattr(self, operation, None) 
            if selected_function is None:
                raise AttributeError(f"Could not find operation {operation}")
            job_data = selected_function()
            logger.info("JOB_DATA %s", job_data)
            return job_data
        except Exception as err:
            logger.warning("CANNOT_SELECT_RANDOM_OPERATION %s", err, exc_info=True)


def declare_exchange(channel: BlockingChannel, exchange_name: str, exchange_type: Literal["topic"]) -> bool:
    """Declare a new Exchange."""
    try:
        channel.exchange_declare(  # type: ignore
            exchange=exchange_name,
            exchange_type=exchange_type,
            durable=True,
        )
        logger.info("EXCHANGE_NAME_DECLARED_OK=True")
        return True
    except Exception as err:
        logger.critical("Error declaring exchange %s err=%s", exchange_type, err)
        return False


@tl.job(interval=timedelta(seconds=10))  # type: ignore
def create_job():
    global global_channel, global_exchange_name, global_exchange_type
    try:
        jobs = Jobs()
        message = jobs.generate_metrics()
        global_channel.basic_publish(exchange=global_exchange_name, routing_key='', body=str(message))
        logger.info("Message sent to consumer -> %s", message)
        # if global_connection:
        #     global_connection.close()
    except Exception as err:
        logger.error("Could not send message | err=%s", err, exc_info=True)


def setup() -> bool:
    """Setup the RabbitMQ connection and instantiate the global variables."""
    global global_connection, global_channel, global_exchange_name, global_exchange_type
    global_exchange_name = "test-exchange"
    global_exchange_type = "topic"
    retries = 0
    while True:
        try:
            url = os.environ.get('CLOUDAMQP_URL', 'amqp://guest:guest@rabbitmq/%2f')
            params = pika.URLParameters(url)
            params.socket_timeout = 5
            params.connection_attempts = 3
            params.retry_delay = 2

            global_connection = pika.BlockingConnection(params)
            global_channel = global_connection.channel()
            if declare_exchange(global_channel, global_exchange_name, global_exchange_type) is False:
                raise NoExchangeException("Exchange Declaration Failed")
            global_channel.queue_declare(queue='python-testing')  # type: ignore
            logger.info("Publisher setup completed, now starting job timer")
            return True
        except Exception as err:
            logger.warning(err, exc_info=True)
            retries += 1
            if retries >= 3:
                logger.critical("RABBITMQ_CONNECTION_FAIL | MAX_RETRIES=True | err=%s", err, exc_info=True)
                return False
            logger.info("Waiting 30s to attempt reconnect...")
            time.sleep(30)


def main():
    if setup():
        tl.start(block=True)  # type: ignore
    sys.exit(0)

if __name__ == "__main__":
    main()
