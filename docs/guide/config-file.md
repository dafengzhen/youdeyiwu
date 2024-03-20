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

# Set the remotePatterns to allow images, in the format of an array
# IMAGES_REMOTE_PATTERNS_0_PROTOCOL=https
# IMAGES_REMOTE_PATTERNS_0_HOSTNAME=example.com
# IMAGES_REMOTE_PATTERNS_0_PORT=
# IMAGES_REMOTE_PATTERNS_0_PATHNAME=**

# Should the page footer be displayed?
SHOW_FOOTER=true
```

::: tip Note

Some configuration items cannot be empty and require a default value.

For configuration items that can be empty, you can either leave the value blank (```key=```) or comment out the item (```#key=value```).

:::
