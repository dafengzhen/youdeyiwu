FROM maven:3-eclipse-temurin-17-alpine AS deps
WORKDIR /youdeyiwu
COPY pom.xml .
COPY src src
RUN --mount=type=cache,target=/root/.m2 mvn clean package -DskipTests

FROM eclipse-temurin:17-jre-alpine AS builder
WORKDIR /youdeyiwu
COPY --from=deps /youdeyiwu/target/*.jar ./youdeyiwu.jar
RUN java -Djarmode=layertools -jar youdeyiwu.jar extract

FROM eclipse-temurin:17-jre-alpine AS runner
WORKDIR /youdeyiwu
RUN addgroup --system --gid 1001 java
RUN adduser --system --uid 1001 java
COPY --chown=java:java --from=builder /youdeyiwu/dependencies/ ./
COPY --chown=java:java --from=builder /youdeyiwu/spring-boot-loader/ ./
COPY --chown=java:java --from=builder /youdeyiwu/snapshot-dependencies/ ./
COPY --chown=java:java --from=builder /youdeyiwu/application/ ./
USER java
ENV PORT 8080
EXPOSE $PORT
ENTRYPOINT ["java", "org.springframework.boot.loader.launch.JarLauncher"]
