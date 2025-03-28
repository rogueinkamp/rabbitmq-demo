use amiquip::{
    Connection, ConsumerMessage, ConsumerOptions, ExchangeDeclareOptions, FieldTable, QueueDeclareOptions, Result
};
use std::thread;
use std::time::Duration;
use std::str;

fn split_message(input_string: &str) -> Vec<String> {
    let vec = input_string.split_whitespace().map(str::to_string).collect();
    vec
}

fn main() -> Result<()> {
    // Open connection with retries
    let mut retries = 0;
    let exchange_name = "test-exchange";
    let queue_name = "python-testing";
    loop {
        match Connection::insecure_open("amqp://guest:guest@rabbitmq:5672") {
            Err(err) => {
                if retries >= 3 {
                    panic!("Cannot Connect To RabbitMQ | retries exceeded!");
                } else {
                    println!("Cannot Connect To RabbitMQ | err={:?} retry=True | waiting 30s to try again...", err);
                    thread::sleep(Duration::from_secs(30));
                    retries += 1;
                }
            },
            Ok(mut connection) => {
                println!("Connection OK!");
                let channel = connection.open_channel(None)?;
                // Declare the queue
                let queue = channel.queue_declare(
                    queue_name,
                    QueueDeclareOptions {
                        durable: true,
                        exclusive: false,
                        auto_delete: false,
                        arguments: FieldTable::default(),
                    },
                )?;
                // Declare the exchange
                let exchange = channel.exchange_declare(
                    amiquip::ExchangeType::Topic,
                    exchange_name,
                    ExchangeDeclareOptions {
                        durable: true,
                        auto_delete: false,
                        internal: false,
                        arguments: FieldTable::default(),
                    },
                )?;
                // Declare the exchange
                channel.exchange_declare(
                    amiquip::ExchangeType::Topic,
                    exchange_name,
                    ExchangeDeclareOptions {
                        durable: true,
                        auto_delete: false,
                        internal: false,
                        arguments: FieldTable::default(),
                    },
                )?;
                queue.bind(&exchange, "", FieldTable::new())?;
                // Start a consumer.
                let consumer = queue.consume(ConsumerOptions::default())?;
                println!("Waiting for messages. Press Ctrl-C to exit.");

                for message in consumer.receiver().iter() {
                    match message {
                        ConsumerMessage::Delivery(delivery) => {
                            let body = String::from_utf8_lossy(&delivery.body);
                            let vector_of_strings = split_message(body.as_ref());
                            println!("RUST_MESSAGE_RECEIVED -> {:?}", vector_of_strings);
                            consumer.ack(delivery)?;
                        }
                        other => {
                            println!("Consumer ended: {:?}", other);
                            break;
                        }
                    }
                }
                connection.close()?
            }
        }
    }
}
