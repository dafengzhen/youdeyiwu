# 桌面端开发

::: info

下面介绍如何搭建二次开发环境，默认你有相应编程基础

:::

Youdeyiwu 桌面端程序主要使用 [Electron 30](https://electronjs.org) 和 [Angular 17](https://angular.dev) 开发

开发编辑器可以选择你喜欢的，推荐使用 [WebStorm](https://www.jetbrains.com/webstorm)

## 克隆仓库

拉取并用编辑器打开项目目录 ```desktop```

```sh
git clone https://github.com/dafengzhen/youdeyiwu
```

## 开发配置

**1. 安装依赖**

使用 Npm 安装依赖

```sh
npm install
```

**2. 配置 Api 服务**

打开 ```src/proxy.conf.json``` 文件

修改对应后端请求地址

```text
"target": "http://localhost:8080"
```

**3. 启动应用**

运行命令

```sh
npm run dev
```

## 打包构建

Youdeyiwu 桌面端程序可以使用 [Docker](https://www.docker.com) 来构建或者在本机构建打包

### 使用 Docker

**1. 构建镜像**

```-t youdeyiwu-docker```，镜像名称和标签，以及其他参数，你可以根据需要来修改

```sh
docker build -t youdeyiwu-pc .
```

**2. 运行镜像**

同上

```sh
docker run --rm -v youdeyiwu-pc-electron:/root/.cache/electron -v youdeyiwu-pc-electron-builder:/root/.cache/electron-builder -v youdeyiwu-pc-out:/youdeyiwu-pc/out youdeyiwu-pc
```

**3. 查看输出**

```sh
docker volume inspect --format '{{ .Mountpoint }}' youdeyiwu-pc-out | xargs cd && ll
```

### 在本机构建

**1. 安装依赖**

```sh
npm install
```

**2. 打包构建**

```sh
npm run build
```

**3. 查看输出**

文件输出在项目的 ```out``` 目录下

## 其他

**如果我想构建苹果桌面端，怎么办？**

由于我没有相应设备，无法测试在苹果桌面端的构建

你可能需要对项目的桌面端进行部分内容适配

## 完成

通过自定义开发，你可以根据需要为应用程序添加各种功能

至此，桌面端开发到构建流程就完成了
