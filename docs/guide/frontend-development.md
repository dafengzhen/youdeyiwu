# Frontend Development

::: info

Below is an explanation of how to set up the development environment for front-end development. It is assumed that you have the necessary programming knowledge.

:::

Youdeyiwu front-end development primarily uses [Next.js 14](https://nextjs.org) and the UI is built using [Bootstrap 5](https://getbootstrap.com).

You can choose your preferred development editor, but we recommend using [WebStorm](https://www.jetbrains.com/webstorm).

## Clone the Repository

Clone the repository and open the project directory ```web``` in your editor.

```sh
git clone https://github.com/dafengzhen/youdeyiwu
```

## Development Setup

**1. Install Dependencies**

Use Npm to install the dependencies.

```sh
npm install
```

**2. Configure API Service**

Open the ```.env``` file (or create a new ```.env.development.local``` file) and modify the corresponding backend API request address.

```txt
API_SERVER=http://localhost:8080
```

**3. Start the Application**

Run the following command to start the application.

```sh
npm run dev
```

## Packaging and Building

Youdeyiwu front-end development uses [Docker](https://www.docker.com) for packaging and building.

**1. Build the Image**

Specify the image name and tag using ```-t youdeyiwu-web```, and you can modify other parameters as needed.

```sh
docker build -t youdeyiwu-web .
```

**2. Run the Image**

Similarly, you can modify the image name and other parameters as needed.

```sh
docker run --name youdeyiwu-web --restart=always -d -p 3000:3000 youdeyiwu-web
```

## Completion

By customizing the development, you can add various functionalities to the application as needed.

With this, the front-end development and build process are complete.
