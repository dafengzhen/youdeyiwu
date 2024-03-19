# 开始使用

在安装 Youdeyiwu 之前，请确保你已安装 [Docker](https://docs.docker.com/install) 和 [Docker Compose](https://docs.docker.com/compose/install)

最简单方式是使用 Docker Compose 来部署

在开始正式部署使用前，建议你先行体验 Youdeyiwu

关于体验时选择的服务器，可以是在本地部署，或者是云服务上使用抢占式或按量计费实例


## 安装步骤

### 拉取脚本

拉取脚本后会执行，等待 Docker Compose 执行成功，继续下一步

可以使用 ```docker compose ps``` 查看执行状态

随后你可以通过 ```http://localhost:3000``` 访问 UI，前端端口默认为 3000，后端为 8080

PS：你可以将 localhost 修改为你当前服务器 Ip

```shell
mkdir -p youdeyiwu-hosting && cd youdeyiwu-hosting && curl -o deploy.sh -fsSL https://raw.githubusercontent.com/dafengzhen/youdeyiwu/main/hosting/deploy.sh && chmod +x deploy.sh && ./deploy.sh
```

### 配置管理

在配置之前，你需要有一个账号，请注册并登录后再进行下面操作：

访问 ```http://localhost:3000/init/root``` 页面配置论坛管理员

其中需要填写的密钥，可以在后端服务输出的日志找到，使用 ```docker compose logs -f youdeyiwu```

### 完成

至此，安装部署已完成

感谢你的支持

