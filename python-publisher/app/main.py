from datetime import timedelta
import logging
import logging.handlers
import os
import random
import json
import time

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

logger = logging.getLogger("python-publisher")


def generate_prime_calculation_job() -> str:
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

def generate_fibonacci_job() -> str:
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

def generate_matrix_multiplication_job() -> str:
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

def generate_metrics():
    """generate some fake metrics to send to the subscribers."""
    available_operations = [
        "prime_number_calculation",
        "fibonacci",
        "matrix_multiplication",
    ]
    operation = random.choice(available_operations)
    try:
        selected_function = locals()[operation]
        job_data = selected_function()
        logger.info("JOB_DATA %s", job_data)
    except Exception as err:
        logger.warning("CANNOT_SELECT_RANDOM_OPERATION", err)


@tl.job(interval=timedelta(seconds=10))  # type: ignore
def main():
    connection = None
    channel = None
    try:
        url = os.environ.get('CLOUDAMQP_URL', 'amqp://guest:guest@rabbitmq/%2f')
        params = pika.URLParameters(url)
        params.socket_timeout = 5

        connection = pika.BlockingConnection(params)
        channel = connection.channel()
        channel.queue_declare(queue='python-testing')  # type: ignore

    except Exception as err:
        logger.error("Could not establish connection to backend | err=%s", err, exc_info=True)


    if channel:
        try:
            message = generate_metrics()
            channel.basic_publish(exchange='test-exchange', routing_key='', body=message)
            logger.info("Message sent to consumer -> %s", message)
            if connection:
                connection.close()
        except Exception as err:
            logger.error("Could not send message | err=%s", err, exc_info=True)


if __name__ == "__main__":
    tl.start(block=True)
