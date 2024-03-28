# Backend Development

::: info
The following describes how to set up a development environment for backend development. It is assumed that you have the necessary programming foundation.
:::

The Youdeyiwu backend application is primarily developed using [Spring Boot 3 (JDK 17)](https://spring.io/projects/spring-boot), and the database used is [MySQL 8](https://www.mysql.com).

You can choose your preferred development editor, but we recommend using [IntelliJ IDEA](https://www.jetbrains.com/idea).

## Clone the Repository

Clone the project and open it in your editor.

```git
git clone https://github.com/dafengzhen/youdeyiwu
```

## Development Configuration

**1. Configure the Database Connection**

Open the ```src/main/resources/application.properties``` file.

Modify the corresponding database information.

```properties
spring.datasource.url = jdbc:mysql://youdeyiwu:3306/youdeyiwu?createDatabaseIfNotExist=true&serverTimezone=UTC&allowMultiQueries=true
spring.datasource.username = youdeyiwu
spring.datasource.password = 123456
```

**2. Load the Property File**

Using IntelliJ IDEA as an example:

Click on the menu bar ```Run``` -> ```Edit Configurations``` -> ```Active profiles``` -> Enter ```dev```

**3. Start the Application**

Open the ```src/main/java/com/youdeyiwu/YoudeyiwuApplication.java``` file.

Start the ```YoudeyiwuApplication``` application.

## Packaging and Building

The Youdeyiwu backend application uses [Docker](https://www.docker.com) for packaging and building.

**1. Build the Image**

```-t youdeyiwu```, the image name and tag, and other parameters can be modified as needed.

```sh
docker build -t youdeyiwu .
```

**2. Run the Image**

Similarly,

```sh
docker run --name youdeyiwu --restart=always -d -p 8080:8080 youdeyiwu
```

## Completion

Through custom development, you can add various functionalities to the application as needed.

With this, the backend development to build process is complete.

