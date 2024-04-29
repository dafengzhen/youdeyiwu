# Vercel Deployment

In general, the frontend and backend may be deployed together, but they can also be separated.

Below are the instructions on how to deploy the frontend application on the [Vercel](https://vercel.com) platform.

## Fork Repository

Fork the [Youdeyiwu](https://github.com/dafengzhen/youdeyiwu) repository.

The purpose of forking is mainly to modify the repository files as needed.

You may need to modify icon resources located in the directory [```web/public/assets```](https://github.com/dafengzhen/youdeyiwu/tree/main/web/public/assets).

In most cases, no other modifications are required.

## Login to the Platform

Login to [Vercel](https://vercel.com) using the GitHub login method.

## Import Repository

Import the ```Youdeyiwu``` repository.

## Configure Project

You may need to fill in the following information, while leaving the rest as default:

- ```Root Directory```: Select the [```web```](https://github.com/dafengzhen/youdeyiwu/blob/main/web) directory in the repository.
- ```Environment Variables```: Below are only the required variables. To view all variables, please refer to the [```.env```](https://github.com/dafengzhen/youdeyiwu/blob/main/web/.env) file in the directory.

The variables that may need to be filled in or modified are as follows:

```sh
# Application url
URL=http://localhost:3000

# Application name
NAME=Youdeyiwu

# Application description
DESCRIPTION=Youdeyiwu is an open-source lightweight forum

# Interface address
API_SERVER=http://localhost:8080

# Is it an HTTPS site
IS_HTTPS_SITE=false

# Allowed source domain(s) for sending requests, multiple domains separated by commas
SERVER_ACTIONS_ALLOWED_ORIGINS=

# Whether to display the footer
SHOW_FOOTER=true

# Define footer content with HTML support (Example："<div class='text-center'> Copyright&nbsp;&copy;&nbsp;2024-present Youdeyiwu </div>")
CUSTOM_FOOTER=
```

## Deploy the Project

After configuring the project, click the ```Deploy``` button to start the deployment.

Wait for the deployment to complete, and then you can access the site according to the instructions provided.

That's it. The deployment is complete.
