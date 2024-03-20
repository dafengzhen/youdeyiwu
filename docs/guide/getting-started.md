# Getting Started

Before deploying, it is recommended to try Youdeyiwu first.

The easiest way to deploy is by using Docker Compose.

Before installing Youdeyiwu, make sure you have installed [Docker](https://docs.docker.com/install) and [Docker Compose](https://docs.docker.com/compose/install).

For the server you choose to experience, you can either deploy it locally or use pre-emptible or on-demand instances on a cloud service.

## Installation Steps

### Clone the Script

Clone the script and it will automatically execute. Wait for Docker Compose to complete the execution, then proceed to the next step.

You can use ```docker compose ps``` to check the execution status.

Afterward, you can access the UI through ```http://localhost:3000```. The default frontend port is 3000, and the backend port is 8080.

Note: You can modify "localhost" to your current server IP.

```sh
mkdir -p youdeyiwu-hosting && cd youdeyiwu-hosting && curl -o deploy.sh -fsSL https://raw.githubusercontent.com/dafengzhen/youdeyiwu/main/hosting/deploy.sh && chmod +x deploy.sh && ./deploy.sh
```

### Configure the Administrator

Before configuration, you need to have an account. Please register before proceeding (```/register```).

Visit the ```/init/root``` page to configure the forum administrator.

The required secret can be found in the output logs of the backend service.

You can use ```docker compose logs -f youdeyiwu``` to view the output.

### Completion

Congratulations! You have completed the installation.
