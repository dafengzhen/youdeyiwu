# 安卓端开发

::: info

下面介绍如何搭建二次开发环境，默认你有相应编程基础

:::

Youdeyiwu 安卓端程序主要使用 [Flutter 3](https://flutter.dev) 开发

开发编辑器可以选择你喜欢的，推荐使用 [Android Studio](https://developer.android.com/studio)

## 克隆仓库

拉取并用编辑器打开项目目录 ```app```

```sh
git clone https://github.com/dafengzhen/youdeyiwu
```

## 开发配置

**1. 安装依赖**

安装依赖

```sh
flutter pub get
```

**2. 修改图标**

替换资源路径 ```assets/images/icon.png```

然后运行如下命令更新：

```sh
flutter pub run flutter_launcher_icons
```

**3. 配置 Api 服务**

打开 ```lib/configs/configs.dart``` 文件

修改对应后端请求地址

```text
const appApiServer = 'http://10.0.2.2:8080';
```

**4. 启动应用**

以 Android Studio 编辑器为例，启动一个安卓模拟器（Device Manager），并运行（Run 'main.dart'）

## 构建 Apk

### 生成密钥

**1. Windows 系统**

```USERPROFILE```：表示密钥存放的目录

运行如下命令生成密钥，并根据提示完成：

```sh
keytool -genkey -v -keystore $env:USERPROFILE\upload-keystore.jks -storetype JKS -keyalg RSA -keysize 2048 -validity 10000 -alias upload
```

**2. 其他系统或者其他方式**

请查看[官方文档](https://docs.flutter.dev/deployment/android#create-an-upload-keystore)介绍

### 构建应用

**1. 运行命令**

```sh
flutter build apk --release
```

**2. 查看输出**

文件输出在项目的 ```build/app/outputs/flutter-apk``` 目录下

## 其他

**如果我想构建苹果端，怎么办？**

由于我没有相应设备，无法测试在苹果端的构建

你可能需要对项目的 App 端进行部分内容适配

## 完成

通过自定义开发，你可以根据需要为应用程序添加各种功能

至此，安卓端开发到构建流程就完成了
