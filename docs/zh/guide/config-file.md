# 配置文件

配置文件 ```.env``` 可以在项目 [hosting](https://github.com/dafengzhen/youdeyiwu/tree/main/hosting) 目录中找到

你可以根据需要进行相应的修改配置

下面列出部分配置示例：

```text
# Database
DATABASE_USERNAME=youdeyiwu
DATABASE_PASSWORD=123456

# Application url (Here, the term 'URL' refers to the frontend URL, not the backend API URL)
URL=http://localhost:3000

# Application name (Likewise)
NAME=Youdeyiwu

# Application description (Likewise)
DESCRIPTION=Youdeyiwu is an open-source lightweight forum

# Is it an HTTPS site
IS_HTTPS_SITE=false

# Set the remotePatterns to allow images, in the format of an array
# IMAGES_REMOTE_PATTERNS_0_PROTOCOL=https
# IMAGES_REMOTE_PATTERNS_0_HOSTNAME=example.com
# IMAGES_REMOTE_PATTERNS_0_PORT=
# IMAGES_REMOTE_PATTERNS_0_PATHNAME=**

# Should the page footer be displayed?
SHOW_FOOTER=true
```

::: tip Note

部分配置项是不能为空的，即该配置项需要一个默认值

可以为空的配置项，一般该项的值不进行填写 (```key=```)，或注释 (```#key=value```) 该项

:::
