FROM node:20-alpine As deps
RUN apk add --no-cache libc6-compat
WORKDIR /youdeyiwu-web
COPY package.json package-lock.json* .npmrc* ./
RUN npm ci

FROM node:20-alpine As builder
WORKDIR /youdeyiwu-web
COPY --from=deps /youdeyiwu-web/node_modules ./node_modules
COPY . .
ENV NODE_ENV production
ENV NEXT_TELEMETRY_DISABLED 1
RUN npm run build

FROM node:20-alpine As runner
WORKDIR /youdeyiwu-web
RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs
RUN mkdir .next
RUN chown nextjs:nodejs .next
COPY --chown=nextjs:nodejs --from=builder /youdeyiwu-web/public ./public
COPY --chown=nextjs:nodejs --from=builder /youdeyiwu-web/.next/standalone ./
COPY --chown=nextjs:nodejs --from=builder /youdeyiwu-web/.next/static ./.next/static
VOLUME /youdeyiwu-web/public/assets
USER nextjs
ENV NODE_ENV production
ENV NEXT_TELEMETRY_DISABLED 1
ENV HOSTNAME "0.0.0.0"
ENV PORT 3000
EXPOSE $PORT
CMD ["node", "server.js"]
