import type { RouteComponent } from 'vue-router'

export type NavigationGroup = 'workspace' | 'operations'
export interface AdminPage {
  name: string
  path: string
  title: string
  icon: string
  navigationGroup: NavigationGroup
  requiredCapabilities: readonly string[]
  component: RouteComponent
}

export const pages: readonly AdminPage[] = [
  { name: 'overview', path: '/overview', title: 'Overview', icon: 'pi pi-home', navigationGroup: 'workspace', requiredCapabilities: [], component: () => import('@/pages/OverviewPage.vue') },
  { name: 'chat', path: '/chat/:sessionId?', title: 'Chat', icon: 'pi pi-comments', navigationGroup: 'workspace', requiredCapabilities: ['chat.demo'], component: () => import('@/pages/ChatPage.vue') },
  { name: 'audit', path: '/audit/:auditId?', title: 'Audit', icon: 'pi pi-wave-pulse', navigationGroup: 'workspace', requiredCapabilities: ['audit.demo'], component: () => import('@/pages/AuditPage.vue') },
  { name: 'tasks', path: '/tasks/:taskId?', title: 'Tasks', icon: 'pi pi-bolt', navigationGroup: 'operations', requiredCapabilities: ['tasks.demo'], component: () => import('@/pages/TasksPage.vue') },
  { name: 'resources', path: '/resources/:resourceId?', title: 'Resources', icon: 'pi pi-box', navigationGroup: 'operations', requiredCapabilities: ['resources.demo'], component: () => import('@/pages/ResourcesPage.vue') },
  { name: 'plugins', path: '/plugins', title: 'Plugins', icon: 'pi pi-objects-column', navigationGroup: 'operations', requiredCapabilities: ['plugins.demo'], component: () => import('@/pages/PluginsPage.vue') },
  { name: 'logs', path: '/logs', title: 'Logs', icon: 'pi pi-align-left', navigationGroup: 'operations', requiredCapabilities: ['logs.demo'], component: () => import('@/pages/LogsPage.vue') },
  { name: 'configuration', path: '/configuration/:section?', title: 'Configuration', icon: 'pi pi-cog', navigationGroup: 'operations', requiredCapabilities: ['config.demo'], component: () => import('@/pages/ConfigurationPage.vue') },
] as const
