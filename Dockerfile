FROM rabbitmq:4.0.7-management-alpine

# Define environment variables.
# NOTE: These would be removed in a production environment and injected using CI-CD
ENV RABBITMQ_USER rabbitmq
ENV RABBITMQ_PASSWORD rabbitmq
ENV RABBITMQ_PID_FILE /var/lib/rabbitmq/mnesia/rabbitmq
ENV USER=rabbitmq

# Set the correct permissions
RUN chown rabbitmq:rabbitmq /etc/rabbitmq/enabled_plugins

# Enable the webui (very handy)
RUN rabbitmq-plugins enable --offline rabbitmq_management

EXPOSE 5672 15671 15672

# Add Healthcheck
HEALTHCHECK --interval=30s --timeout=10s --retries=3 CMD rabbitmqctl status

RUN chgrp rabbitmq /etc/rabbitmq
RUN chmod g+r /etc/rabbitmq

RUN chmod a+r /etc/rabbitmq/enabled_plugins

# Define default command
CMD ["rabbitmq-server"]
