import { type MetadataRoute } from 'next';

export default function robots(): MetadataRoute.Robots {
  return {
    rules: {
      userAgent: '*',
      allow: [
        '/',
        '/sections/*',
        '/posts/*',
        '/users/*',
        '/login',
        '/register',
      ],
      disallow: ['/admin/*', '/posts/*/edit'],
    },
  };
}
