# Update Image

The Youdeyiwu program is regularly updated.

If you need to apply these updates, it is recommended to use [Docker Compose](https://docs.docker.com/compose) for easy upgrading.

```sh
docker compose pull
docker compose down
docker compose up -d
```

Check the status:

```sh
docker compose ps
```

::: tip

Generally, there should be no issues with the update process, but it is still advisable to backup your data.

:::
