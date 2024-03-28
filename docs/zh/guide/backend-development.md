# 后端开发

::: info
下面介绍如何搭建二次开发环境，默认你有相应编程基础
:::

Youdeyiwu 后端程序主要使用 [Spring Boot 3 (JDK 17)](https://spring.io/projects/spring-boot) 开发，数据库使用 [MySQL 8](https://www.mysql.com)

开发编辑器可以选择你喜欢的，推荐使用 [IntelliJ IDEA](https://www.jetbrains.com/idea)

## 克隆仓库

拉取并用编辑器打开项目

```sh
git clone https://github.com/dafengzhen/youdeyiwu
```

## 开发配置

**1. 配置数据库链接**

打开 ```src/main/resources/application.properties``` 文件

修改对应数据库信息

```properties
spring.datasource.url = jdbc:mysql://youdeyiwu:3306/youdeyiwu?createDatabaseIfNotExist=true&serverTimezone=UTC&allowMultiQueries=true
spring.datasource.username = youdeyiwu
spring.datasource.password = 123456
```

**2. 加载属性文件**

以 IntelliJ IDEA 为示例：

点击菜单栏上的 ```Run``` -> ```Edit Configurations``` -> ```Active profiles``` -> 输入 ```dev```

**3. 启动应用**

打开 ```src/main/java/com/youdeyiwu/YoudeyiwuApplication.java``` 文件

并启动 ```YoudeyiwuApplication``` 应用

## 打包构建

Youdeyiwu 后端程序使用 [Docker](https://www.docker.com) 来打包构建

**1. 构建镜像**

```-t youdeyiwu```，镜像名称和标签，以及其他参数，你可以根据需要来修改

```sh
docker build -t youdeyiwu .
```

**2. 运行镜像**

同上

```sh
docker run --name youdeyiwu --restart=always -d -p 8080:8080 youdeyiwu
```

## 完成

通过自定义开发，你可以根据需要为应用程序添加各种功能

至此，后端开发到构建流程就完成了
