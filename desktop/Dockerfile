FROM node:20-alpine As deps
RUN apk add --no-cache libc6-compat
WORKDIR /youdeyiwu-pc
COPY package.json package-lock.json* .npmrc* ./
RUN npm ci

FROM node:20-alpine As compiler
WORKDIR /youdeyiwu-pc
COPY --from=deps /youdeyiwu-pc/node_modules ./node_modules
COPY . .
ENV NODE_ENV production
RUN npm run ng:build

FROM electronuserland/builder:wine As packager
WORKDIR /youdeyiwu-pc
COPY --from=compiler /youdeyiwu-pc/node_modules ./node_modules
COPY --from=compiler /youdeyiwu-pc/dist ./dist
COPY . .
ENV NODE_ENV production
ENV ELECTRON_CACHE /root/.cache/electron
ENV ELECTRON_BUILDER_CACHE /root/.cache/electron-builder
VOLUME /root/.cache/electron
VOLUME /root/.cache/electron-builder
VOLUME /youdeyiwu-pc/out
RUN npm run dist
