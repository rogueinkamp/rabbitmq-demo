FROM rabbitmq:4.0.7-management-alpine

# Define environment variables.
# NOTE: These would be removed in a production environment and injected using CI-CD
ENV RABBITMQ_USER rabbitmq
ENV RABBITMQ_PASSWORD rabbitmq
ENV RABBITMQ_PID_FILE /var/lib/rabbitmq/mnesia/rabbitmq
ENV USER=rabbitmq

# Ensure correct user permissions for alpine
RUN apk --no-cache add shadow && \
    usermod -u 1001 rabbitmq && \
    groupmod -g 1000 rabbitmq

# Enable the webui (very handy)
RUN rabbitmq-plugins enable --offline rabbitmq_management

EXPOSE 15671 15672

# Add Healthcheck
HEALTHCHECK --interval=30s --timeout=10s --retries=3 CMD rabbitmqctl status

# Define default command
CMD ["rabbitmq-server"]
