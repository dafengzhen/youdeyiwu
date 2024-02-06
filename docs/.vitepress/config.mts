import { DefaultTheme, defineConfig } from 'vitepress';
// @ts-ignore
import pkg from '../package.json';

// https://vitepress.dev/reference/site-config
export default defineConfig({
  lang: 'en-US',
  base: '/docs/',
  cleanUrls: true,
  title: 'Youdeyiwu',
  description: 'Youdeyiwu is an open-source lightweight forum',
  head: [
    ['link', { rel: 'icon', href: '/docs/favicon/favicon.ico' }],
    [
      'link',
      {
        rel: 'apple-touch-icon',
        sizes: '180x180',
        href: '/docs/favicon/apple-touch-icon.png',
      },
    ],
    [
      'link',
      {
        rel: 'icon',
        type: 'image/png',
        sizes: '32x32',
        href: '/docs/favicon/favicon-32x32.png',
      },
    ],
    [
      'link',
      {
        rel: 'icon',
        type: 'image/png',
        sizes: '16x16',
        href: '/docs/assets/favicon/favicon-16x16.png',
      },
    ],
    ['link', { rel: 'manifest', href: '/docs/favicon/site.webmanifest' }],
    ['meta', { name: 'theme-color', content: '#fc5bb6' }],
    ['meta', { property: 'og:type', content: 'website' }],
    ['meta', { property: 'og:locale', content: 'en' }],
    [
      'meta',
      {
        property: 'og:title',
        content: 'Youdeyiwu | Youdeyiwu is an open-source lightweight forum',
      },
    ],
    ['meta', { property: 'og:site_name', content: 'Youdeyiwu' }],
    [
      'meta',
      {
        property: 'og:image',
        content: 'https://www.youdeyiwu.com/og.jpg',
      },
    ],
    ['meta', { property: 'og:url', content: 'https://www.youdeyiwu.com' }],
  ],
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    logo: '/logo.png',
    socialLinks: [
      { icon: 'github', link: 'https://github.com/dafengzhen/youdeyiwu' },
    ],
    nav: nav(),
    sidebar: {
      '/guide/': { base: '/guide/', items: sidebarGuide() },
      '/reference/': { base: '/reference/', items: sidebarReference() },
    },
    editLink: {
      pattern: 'https://github.com/vuejs/vitepress/edit/main/docs/:path',
      text: 'Edit this page on GitHub',
    },
    footer: {
      message: 'Released under the MIT License.',
      copyright: 'Copyright © 2024-present Evan You',
    },
  },
});

function nav(): DefaultTheme.NavItem[] {
  return [
    {
      text: 'Guide',
      link: '/guide/what-is-vitepress',
      activeMatch: '/guide/',
    },
    {
      text: 'Reference',
      link: '/reference/site-config',
      activeMatch: '/reference/',
    },
    {
      text: pkg.version,
      items: [
        {
          text: 'Changelog',
          link: 'https://github.com/dafengzhen/youdeyiwu/blob/main/CHANGELOG.md',
        },
        {
          text: 'Contributing',
          link: 'https://github.com/dafengzhen/youdeyiwu/blob/main/.github/contributing.md',
        },
      ],
    },
  ];
}

function sidebarGuide(): DefaultTheme.SidebarItem[] {
  return [
    {
      text: 'Introduction',
      collapsed: false,
      items: [
        { text: 'What is Youdeyiwu?', link: 'what-is-youdeyiwu' },
        { text: 'Getting Started', link: 'getting-started' },
      ],
    },
    {
      text: 'API Reference',
      base: '/reference/',
      link: 'config-site',
    },
  ];
}

function sidebarReference(): DefaultTheme.SidebarItem[] {
  return [
    {
      text: 'Reference',
      items: [
        {
          text: 'Config',
          base: '/reference/config-',
          items: [{ text: 'Site Config', link: 'site' }],
        },
      ],
    },
  ];
}
