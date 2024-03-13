const createNextIntlPlugin = require('next-intl/plugin');

const withNextIntl = createNextIntlPlugin();

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'standalone',
  poweredByHeader: false,
  images: {
    remotePatterns: parseImageRemotePatterns(),
  },
  experimental: {
    serverActions: {
      bodySizeLimit: '16mb',
      allowedOrigins: process.env.SERVER_ACTIONS_ALLOWED_ORIGINS
        ? process.env.SERVER_ACTIONS_ALLOWED_ORIGINS.split(',')
        : [],
    },
  },
};

function parseImageRemotePatterns() {
  const imagesRemotePatterns = [];
  let index = 0;

  while (process.env[`IMAGES_REMOTE_PATTERNS_${index}_PROTOCOL`]) {
    imagesRemotePatterns.push({
      protocol: process.env[`IMAGES_REMOTE_PATTERNS_${index}_PROTOCOL`] ?? '',
      hostname: process.env[`IMAGES_REMOTE_PATTERNS_${index}_HOSTNAME`] ?? '',
      port: process.env[`IMAGES_REMOTE_PATTERNS_${index}_PORT`] ?? '',
      pathname: process.env[`IMAGES_REMOTE_PATTERNS_${index}_PATHNAME`] ?? '',
    });

    index++;
  }

  return imagesRemotePatterns;
}

module.exports = withNextIntl(nextConfig);
