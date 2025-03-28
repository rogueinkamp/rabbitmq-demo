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


@tl.job(interval=timedelta(seconds=10))  # type: ignore
def main():
    connection = None
    channel = None
    try:
        url = os.environ.get('CLOUDAMQP_URL', 'amqp://guest:guest@rabbitmq/%2f')
        params = pika.URLParameters(url)
        params.socket_timeout = 5
        params.connection_attempts = 3
        params.retry_delay = 2

        connection = pika.BlockingConnection(params)
        channel = connection.channel()
        channel.queue_declare(queue='python-testing')  # type: ignore

    except Exception as err:
        logger.error("Could not establish connection to backend | err=%s", err, exc_info=True)


    if channel:
        try:
            jobs = Jobs()
            message = jobs.generate_metrics()
            channel.basic_publish(exchange='test-exchange', routing_key='', body=message)
            logger.info("Message sent to consumer -> %s", message)
            if connection:
                connection.close()
        except Exception as err:
            logger.error("Could not send message | err=%s", err, exc_info=True)


if __name__ == "__main__":
    tl.start(block=True)
