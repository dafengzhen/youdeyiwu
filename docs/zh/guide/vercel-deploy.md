# Vercel 部署

一般情况下可能会将前后端部署在一起，但其实可以分开

下面介绍如何将前端应用部署在 [Vercel](https://vercel.com) 平台上

## 拷贝仓库

Fork [Youdeyiwu](https://github.com/dafengzhen/youdeyiwu) 仓库

拷贝的目的主要是可以根据需要来修改仓库文件

有可能需要修改是图标资源，所在目录 [```web/public/assets```](https://github.com/dafengzhen/youdeyiwu/tree/main/web/public/assets)

其他大部分情况下不需要修改

## 登录平台

登录 [Vercel](https://vercel.com)，使用 Github 登录方式

## 导入仓库

导入 ```Youdeyiwu``` 仓库

## 配置项目

可能需要填写的信息如下，其他默认即可：

- ```Root Directory```: 选择仓库下的 [```web```](https://github.com/dafengzhen/youdeyiwu/blob/main/web) 目录
- ```Environment Variables```: 下面仅列出所需变量，要查看所有变量，请查看目录下 [```.env```](https://github.com/dafengzhen/youdeyiwu/blob/main/web/.env) 文件

可能需要填写或修改的变量如下：

```sh
# Application url
URL=http://localhost:3000

# Application name
NAME=Youdeyiwu

# Application description
DESCRIPTION=Youdeyiwu is an open-source lightweight forum

# Interface address
API_SERVER=http://localhost:8080

# Is it an HTTPS site
IS_HTTPS_SITE=false

# Allowed source domain(s) for sending requests, multiple domains separated by commas
SERVER_ACTIONS_ALLOWED_ORIGINS=

# Whether to display the footer
SHOW_FOOTER=true

# Define footer content with HTML support (Example："<div class='text-center'> Copyright&nbsp;&copy;&nbsp;2024-present Youdeyiwu </div>")
CUSTOM_FOOTER=
```

## 部署项目

配置项目完成后，点击按钮 ```Deploy```，开始部署

等待部署完成后，根据提示就可以进行访问

至此完成
