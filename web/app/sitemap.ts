import { type MetadataRoute } from 'next';

const now = new Date().toISOString();

export default function sitemap(): MetadataRoute.Sitemap {
  const url = process.env.URL ?? 'http://localhost:3000';

  return [
    {
      url: `${url}`,
      lastModified: now,
    },
    {
      url: `${url}/sections`,
      lastModified: now,
    },
    {
      url: `${url}/posts`,
      lastModified: now,
    },
    {
      url: `${url}/posts/new`,
      lastModified: now,
    },
    {
      url: `${url}/users`,
      lastModified: now,
    },
    {
      url: `${url}/login`,
      lastModified: now,
    },
    {
      url: `${url}/register`,
      lastModified: now,
    },
  ];
}
