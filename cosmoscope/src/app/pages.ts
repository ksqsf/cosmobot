import type { RouteComponent } from 'vue-router'
import { chatMethods, type LiveAdminMethod } from '@/rpc/protocol'

type DemoCapability = 'plugins.demo' | 'logs.demo' | 'config.demo'
export type PageCapability = LiveAdminMethod | DemoCapability
export interface AdminPage {
  name: string
  path: string
  title: string
  icon: string
  requiredCapabilities: readonly PageCapability[]
  component: RouteComponent
}

export const pages = [
  { name: 'overview', path: '/overview', title: 'Overview', icon: 'pi pi-home', requiredCapabilities: ['concurrency.list', 'audit.recent', 'chat.list_sessions', 'resource.list', 'media.stats'], component: () => import('@/pages/OverviewPage.vue') },
  { name: 'chat', path: '/chat/:sessionId?', title: 'Chat', icon: 'pi pi-comments', requiredCapabilities: chatMethods, component: () => import('@/pages/ChatPage.vue') },
  { name: 'audit', path: '/audit/:auditId?', title: 'Audit', icon: 'pi pi-wave-pulse', requiredCapabilities: ['audit.recent', 'audit.get', 'audit.thread', 'audit.subscribe'], component: () => import('@/pages/AuditPage.vue') },
  { name: 'media', path: '/media/:mediaId?', title: 'Media', icon: 'pi pi-images', requiredCapabilities: ['media.stats', 'media.search', 'media.get', 'media.delete', 'media.gc'], component: () => import('@/pages/MediaPage.vue') },
  { name: 'tasks', path: '/tasks/:taskId?', title: 'Tasks', icon: 'pi pi-bolt', requiredCapabilities: ['concurrency.list', 'concurrency.lookup', 'concurrency.cancel', 'concurrency.await', 'resource.list_associated', 'resource.destroy_associated'], component: () => import('@/pages/TasksPage.vue') },
  { name: 'resources', path: '/resources/:resourceId?', title: 'Resources', icon: 'pi pi-box', requiredCapabilities: ['resource.list', 'resource.detail', 'resource.destroy', 'resource.rename', 'resource.keep_alive', 'resource.make_permanent'], component: () => import('@/pages/ResourcesPage.vue') },
  { name: 'plugins', path: '/plugins', title: 'Plugins', icon: 'pi pi-objects-column', requiredCapabilities: ['plugin.list', 'plugin.load', 'plugin.reload', 'plugin.unload'], component: () => import('@/pages/PluginsPage.vue') },
  { name: 'logs', path: '/logs', title: 'Logs', icon: 'pi pi-align-left', requiredCapabilities: ['logs.demo'], component: () => import('@/pages/LogsPage.vue') },
  { name: 'configuration', path: '/configuration/:section?', title: 'Configuration', icon: 'pi pi-cog', requiredCapabilities: ['config.demo'], component: () => import('@/pages/ConfigurationPage.vue') },
] satisfies readonly AdminPage[]
