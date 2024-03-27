# 图标文件

应用程序图标文件挂载在 Docker Volume 中

相关图标文件可以查看 [Assets](https://github.com/dafengzhen/youdeyiwu/tree/main/web/public/assets) 目录

```text
├── avatar.png
├── favicon
└── og.jpg
```

**如何替换?**

使用 Docker 查看挂载点 (Mountpoint)

```sh
docker volume inspect youdeyiwu_web_assets
```

进入挂载点替换源文件 (以下路径只是示例，具体以实际挂载点为准)

```sh
cd /var/lib/docker/volumes/youdeyiwu_web_assets/_data
```

**关于 Favicon**

如果你没有自己的 Favicon 文件，可以考虑使用在线生成 [Favicon](https://favicon.io)

其他图标文件，可以根据需要直接覆盖源文件即可
