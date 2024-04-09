# Configuration File

The configuration file ```.env``` can be found in the project [hosting](https://github.com/dafengzhen/youdeyiwu/tree/main/hosting) directory.

You can modify the configuration according to your needs.

Here are some configuration examples:

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

Some configuration items cannot be empty and require a default value.

For configuration items that can be empty, you can either leave the value blank (```key=```) or comment out the item (```#key=value```).

:::
