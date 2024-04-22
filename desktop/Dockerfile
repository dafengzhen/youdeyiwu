FROM node:20 As deps
WORKDIR /youdeyiwu-pc
COPY package.json package-lock.json* .npmrc* ./
RUN sudo apt install mono-devel
RUN npm ci

FROM node:20 As builder
WORKDIR /youdeyiwu-pc
COPY --from=deps /youdeyiwu-pc/node_modules ./node_modules
COPY . .
ENV NODE_ENV production
RUN npm run build && npm run make

FROM node:20 As runner
WORKDIR /youdeyiwu-pc
RUN addgroup --system --gid 1001 angular
RUN adduser --system --uid 1001 angular
COPY --chown=nextjs:angular --from=builder /youdeyiwu-pc/out ./out
VOLUME /youdeyiwu-pc/out
USER angular
