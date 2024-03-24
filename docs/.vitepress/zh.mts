import { DefaultTheme, defineConfig } from 'vitepress';
// @ts-ignore
import pkg from '../package.json';

// https://vitepress.dev/reference/site-config
export const zh = defineConfig({
  lang: 'zh-Hans',
  title: '尤得一物',
  description: '尤得一物是一个开源的轻量级论坛',
  themeConfig: {
    // https://vitepress.dev/reference/default-theme-config
    nav: nav(),
    sidebar: {
      '/zh/guide/': { base: '/zh/guide/', items: sidebarGuide() },
      '/zh/reference/': { base: '/zh/reference/', items: sidebarReference() }
    },
    editLink: {
      pattern: 'https://github.com/dafengzhen/youdeyiwu/edit/main/docs/:path',
      text: '在 GitHub 上编辑此页面'
    },
    docFooter: {
      prev: '上一页',
      next: '下一页'
    },
    outline: {
      label: '页面导航'
    },
    langMenuLabel: '多语言',
    returnToTopLabel: '回到顶部',
    sidebarMenuLabel: '菜单',
    darkModeSwitchLabel: '主题',
    lightModeSwitchTitle: '切换到浅色模式',
    darkModeSwitchTitle: '切换到深色模式'
  }
});

function nav(): DefaultTheme.NavItem[] {
  return [
    {
      text: '指南',
      link: '/guide/what-is-youdeyiwu',
      activeMatch: '/guide/'
    },
    {
      text: '参考',
      link: '/reference/site-config',
      activeMatch: '/reference/'
    },
    {
      text: pkg.version,
      items: [
        {
          text: '更新日志',
          link: 'https://github.com/dafengzhen/youdeyiwu/blob/main/CHANGELOG.md'
        },
        {
          text: '参与贡献',
          link: 'https://github.com/dafengzhen/youdeyiwu/blob/main/.github/contributing.md'
        }
      ]
    }
  ];
}

function sidebarGuide(): DefaultTheme.SidebarItem[] {
  return [
    {
      text: '简介',
      collapsed: false,
      items: [
        { text: '什么是尤得一物？', link: 'what-is-youdeyiwu' },
        { text: '快速开始', link: 'getting-started' },
        { text: '配置文件', link: 'config-file' },
        { text: '常见问题', link: 'fqa' }
      ]
    },
    {
      text: 'API 参考',
      base: '/zh/reference/',
      link: 'config-site'
    }
  ];
}

function sidebarReference(): DefaultTheme.SidebarItem[] {
  return [
    {
      text: '参考',
      items: [
        {
          text: '配置',
          base: '/zh/reference/config-',
          items: [{ text: '站点配置', link: 'site' }]
        }
      ]
    }
  ];
}
