import { DefaultTheme, defineConfig } from 'vitepress'; // @ts-ignore
import pkg from '../package.json'; // https://vitepress.dev/reference/site-config

// https://vitepress.dev/reference/site-config
export const en = defineConfig({
  lang: 'en-US',
  title: 'Youdeyiwu',
  description: 'Youdeyiwu is an open-source lightweight forum',
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: nav(),
    sidebar: {
      '/guide/': { base: '/guide/', items: sidebarGuide() },
      '/reference/': { base: '/reference/', items: sidebarReference() },
    },
    editLink: {
      pattern: 'https://github.com/dafengzhen/youdeyiwu/edit/main/docs/:path',
      text: 'Edit this page on GitHub',
    },
  },
});

function nav(): DefaultTheme.NavItem[] {
  return [
    {
      text: 'Guide',
      link: '/guide/what-is-youdeyiwu',
      activeMatch: '/guide/',
    },
    {
      text: 'Reference',
      link: '/reference/overview',
      activeMatch: '/reference/',
    },
    // {
    //   text: pkg.version,
    //   items: [
    //     {
    //       text: 'Changelog',
    //       link: 'https://github.com/dafengzhen/youdeyiwu/blob/main/CHANGELOG.md',
    //     },
    //     {
    //       text: 'Contributing',
    //       link: 'https://github.com/dafengzhen/youdeyiwu/blob/main/.github/contributing.md',
    //     },
    //   ],
    // },
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
        { text: 'Update Image', link: 'update-image' },
        { text: 'Config File', link: 'config-file' },
        { text: 'Icon File', link: 'icon-file' },
        { text: 'FQA', link: 'fqa' },
      ],
    },
    {
      text: 'Use',
      collapsed: false,
      items: [
        { text: 'Are you ready?', link: 'are-you-ready' },
        { text: 'Create Article', link: 'create-article' },
      ],
    },
    {
      text: 'Development',
      collapsed: false,
      items: [
        { text: 'Backend', link: 'backend-development' },
        { text: 'Frontend', link: 'frontend-development' },
      ],
    },
    {
      text: 'API Reference',
      base: '/reference/',
      link: 'overview',
    },
  ];
}

function sidebarReference(): DefaultTheme.SidebarItem[] {
  return [
    {
      text: 'Reference',
      items: [
        {
          text: 'Overview',
          link: 'overview',
        },
        {
          text: 'Apis',
          base: '/reference/api-',
          items: [{ text: 'Login', link: 'login' }],
        },
      ],
    },
  ];
}
