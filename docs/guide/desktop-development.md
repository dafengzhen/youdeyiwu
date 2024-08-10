# Desktop Development

::: info

The following describes how to set up the development environment for desktop applications. It is assumed that you have a basic programming foundation.

:::

Youdeyiwu desktop applications are primarily developed using [Electron 30](https://electronjs.org) and [Angular 17](https://angular.dev).

You can choose your preferred development editor, and it is recommended to use [WebStorm](https://www.jetbrains.com/webstorm).

## Clone the Repository

Clone the project directory ```desktop``` and open it in your editor.

```sh
git clone https://github.com/dafengzhen/youdeyiwu
```

## Development Configuration

**1. Install Dependencies**

Use npm to install the dependencies.

```sh
npm install
```

**2. Configure API Service**

Open the ```src/proxy.conf.json``` file.

Modify the corresponding backend request address.

```text
"target": "http://localhost:8080"
```

**3. Start the Application**

Run the command.

```sh
npm run dev
```

## Packaging and Building

Youdeyiwu desktop applications can be built using [Docker](https://www.docker.com) or built and packaged locally.

### Using Docker

**1. Build the Image**

```-t youdeyiwu-docker```, the image name and tag, and other parameters. You can modify them as needed.

```sh
docker build -t youdeyiwu-pc .
```

**2. Run the Image**

Same as above.

```sh
docker run --rm -v youdeyiwu-pc-electron:/root/.cache/electron -v youdeyiwu-pc-electron-builder:/root/.cache/electron-builder -v youdeyiwu-pc-out:/youdeyiwu-pc/out youdeyiwu-pc
```

**3. View the Output**

```sh
docker volume inspect --format '{{ .Mountpoint }}' youdeyiwu-pc-out | xargs cd && ll
```

### Building Locally

**1. Install Dependencies**

```sh
npm install
```

**2. Package and Build**

```sh
npm run build
```

**3. View the Output**

The files will be output in the ```out``` directory of the project.

## Other

**What if I want to build for Apple desktop?**

Due to the lack of relevant devices, I am unable to test the build on Apple desktop.

You may need to adapt certain parts of the project for the desktop version.

## Completion

Through custom development, you can add various functionalities to the application as needed.

With this, the desktop development and build process are complete.
