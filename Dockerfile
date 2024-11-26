# Start from the Haskell base image
FROM haskell:latest

# Create a new user with a home directory
RUN useradd -m -d /home/myuser myuser
RUN whoami

# Set permissions for the work directory
WORKDIR /home/myuser

# Switch to the non-root user
USER myuser

# Default command (this can be overridden when running the container)
CMD ["ghci"]
