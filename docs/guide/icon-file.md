# Icon Files

The application's icon files are mounted in a Docker Volume.

You can find the relevant icon files in the [Assets](https://github.com/dafengzhen/youdeyiwu/tree/main/web/public/assets) directory.

```text
├── avatar.png
├── favicon
└── og.jpg
```

**How to Replace?**

To determine the mountpoint of the Docker Volume, use the following Docker command:

```sh
docker volume inspect youdeyiwu_web_assets
```

Navigate to the mountpoint and replace the source files. Please note that the path provided below is just an example. Refer to the actual mountpoint mentioned in the output.

```sh
cd /var/lib/docker/volumes/youdeyiwu_web_assets/_data
```

**About Favicon**

If you don't have your own favicon file, you can consider generating one online using [Favicon](https://favicon.io).

For other icon files, you can directly overwrite the source files as needed.
