# 更新镜像

Youdeyiwu 程序会定期的更新

如果需要应用这些更新，使用 [Docker Compose](https://docs.docker.com/compose) 来更新升级会比较容易

```sh
docker compose pull
docker compose down
docker compose up -d
```

查看状态

```sh
docker compose ps
```

::: tip

一般更新升级不会有什么问题，但还是建议备份一下数据

:::
