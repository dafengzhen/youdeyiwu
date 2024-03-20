# 开始使用

在正式部署之前，建议你先行体验 Youdeyiwu

最简单方式是使用 Docker Compose 来部署

在安装 Youdeyiwu 之前，请确保你已安装 [Docker](https://docs.docker.com/install) 和 [Docker Compose](https://docs.docker.com/compose/install)

关于体验时选择的服务器，可以是在本地部署，或者是云服务上使用抢占式或按量计费实例

## 安装步骤

### 拉取脚本

拉取脚本后会自动执行，等待 Docker Compose 执行成功，继续下一步

可以使用 ```docker compose ps``` 查看执行状态

随后你可以通过 ```http://localhost:3000``` 访问 UI，前端端口默认为 3000，后端为 8080

Note：你可以将 "localhost" 修改为你当前服务器 Ip

```sh
mkdir -p youdeyiwu-hosting && cd youdeyiwu-hosting && curl -o deploy.sh -fsSL https://raw.githubusercontent.com/dafengzhen/youdeyiwu/main/hosting/deploy.sh && chmod +x deploy.sh && ./deploy.sh
```

### 配置管理员

在配置之前，你需要有一个账号，请注册后再操作 ```(/register)```

访问 ```/init/root``` 页面配置论坛管理员

其中需要填写的密钥，可以从后端服务输出日志中找到

可以使用 ```docker compose logs -f youdeyiwu``` 查看输出

### 完成

至此完成

