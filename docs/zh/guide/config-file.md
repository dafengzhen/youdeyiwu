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

# Whether to display the footer
SHOW_FOOTER=true

# Define footer content with HTML support (Example："<div class='text-center'> Copyright&nbsp;&copy;&nbsp;2024-present Youdeyiwu </div>")
CUSTOM_FOOTER=
```

::: tip Note

部分配置项是不能为空的，即该配置项需要一个默认值

可以为空的配置项，一般该项的值不进行填写 (```key=```)，或注释 (```#key=value```) 该项

:::
