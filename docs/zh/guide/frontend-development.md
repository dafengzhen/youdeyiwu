# 前端开发

::: info
下面介绍如何搭建二次开发环境，默认你有相应编程基础
:::

Youdeyiwu 前端程序主要使用 [Next.js 14](https://nextjs.org) 开发，UI 使用 [Bootstrap 5](https://getbootstrap.com)

开发编辑器可以选择你喜欢的，推荐使用 [WebStorm](https://www.jetbrains.com/webstorm)

## 克隆仓库

拉取并用编辑器打开项目目录 ```web```

```git
git clone https://github.com/dafengzhen/youdeyiwu
```

## 开发配置

**1. 安装依赖**

使用 Npm 安装依赖

```sh
npm install
```

**2. 配置 Api 服务**

打开 ```.env``` 文件 (或者新建 ```.env.development.local``` 配置文件)

修改对应后端请求地址

```env
API_SERVER=http://localhost:8080
```

**3. 启动应用**

运行命令

```sh
npm run dev
```

## 打包构建

Youdeyiwu 前端程序使用 [Docker](https://www.docker.com) 来打包构建

**1. 构建镜像**

```-t youdeyiwu-web```，镜像名称和标签，以及其他参数，你可以根据需要来修改

```sh
docker build -t youdeyiwu-web .
```

**2. 运行镜像**

同上

```sh
docker run --name youdeyiwu-web --restart=always -d -p 3000:3000 youdeyiwu-web
```

## 完成

通过自定义开发，你可以根据需要为应用程序添加各种功能

至此，前端开发到构建流程就完成了
