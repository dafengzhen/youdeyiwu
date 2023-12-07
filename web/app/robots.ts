import { type MetadataRoute } from 'next';

export default function robots(): MetadataRoute.Robots {
  return {
    rules: {
      userAgent: '*',
      allow: ['/', '/health', '/login', '/register', '/bookmarks'],
      disallow: [
        '/collections',
        '/collections/*',
        '/exception',
        '/exception/*',
        '/users',
        '/users/*',
        '/profile',
        '/settings',
      ],
    },
  };
}
