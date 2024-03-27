import { defineConfig } from 'vitepress';
import { en } from './en.mjs';
import { zh } from './zh.mjs';

const isDev = process.env.NODE_ENV === 'development';
const isProd = !isDev;
const base = '/youdeyiwu/';

// https://vitepress.dev/reference/site-config
export default defineConfig({
  lang: 'en-US',
  locales: {
    root: { label: 'English', ...en },
    zh: { label: '简体中文', ...zh },
  },
  base,
  cleanUrls: true,
  title: 'Youdeyiwu',
  description: 'Youdeyiwu is an open-source lightweight forum',
  head: [
    ['link', { rel: 'icon', href: `${base}favicon/favicon.ico` }],
    [
      'link',
      {
        rel: 'apple-touch-icon',
        sizes: '180x180',
        href: `${base}favicon/apple-touch-icon.png`,
      },
    ],
    [
      'link',
      {
        rel: 'icon',
        type: 'image/png',
        sizes: '32x32',
        href: `${base}favicon/favicon-32x32.png`,
      },
    ],
    [
      'link',
      {
        rel: 'icon',
        type: 'image/png',
        sizes: '16x16',
        href: `${base}assets/favicon/favicon-16x16.png`,
      },
    ],
    ['link', { rel: 'manifest', href: `${base}favicon/site.webmanifest` }],
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

    isProd
      ? [
          'script',
          {
            async: '',
            src: 'https://eu.umami.is/script.js',
            'data-website-id': '530fe927-a914-40f9-9055-ec7b1730bbdb',
          },
        ]
      : ([] as any),
  ],
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    logo: '/logo.png',
    socialLinks: [
      { icon: 'github', link: 'https://github.com/dafengzhen/youdeyiwu' },
    ],
    search: {
      provider: 'local',
      options: {
        locales: {
          zh: {
            translations: {
              button: {
                buttonText: '搜索文档',
                buttonAriaLabel: '搜索文档',
              },
              modal: {
                noResultsText: '无法找到相关结果',
                resetButtonTitle: '清除查询条件',
                displayDetails: '显示详细信息',
                backButtonTitle: '返回按钮标题',
                footer: {
                  selectText: '选择',
                  navigateText: '切换',
                  closeText: '关闭',
                },
              },
            },
          },
        },
      },
    },
    editLink: {
      pattern: 'https://github.com/dafengzhen/youdeyiwu/edit/main/docs/:path',
      text: 'Edit this page on GitHub',
    },
    footer: {
      message: 'Released under the MIT License.',
      copyright: 'Copyright © 2024-present Youdeyiwu',
    },
  },
});
