FROM rabbitmq:3.9.3-management-alpine

# Define environment variables.
ENV RABBITMQ_USER rabbitmq
ENV RABBITMQ_PASSWORD rabbitmq
ENV RABBITMQ_PID_FILE /var/lib/rabbitmq/mnesia/rabbitmq
ENV USER=rabbitmq

# Seriously fuck you alpine
RUN apk --no-cache add shadow && \
    usermod -u 1001 rabbitmq && \
    groupmod -g 1000 rabbitmq

RUN rabbitmq-plugins enable --offline rabbitmq_management

EXPOSE 15671 15672

# Define default command
CMD ["rabbitmq-server"]
