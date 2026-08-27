import { createRouter, createWebHistory } from 'vue-router'
import { pages } from './pages'

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
  routes: [
    { path: '/login', name: 'login', component: () => import('@/pages/LoginPage.vue'), meta: { public: true, title: 'Login' } },
    {
      path: '/',
      component: () => import('@/components/app/AppShell.vue'),
      children: [
        { path: '', redirect: '/overview' },
        ...pages.map((page) => ({ path: page.path.slice(1), name: page.name, component: page.component, meta: { title: page.title } })),
      ],
    },
    { path: '/:pathMatch(.*)*', name: 'not-found', component: () => import('@/pages/NotFoundPage.vue'), meta: { title: 'Not found' } },
  ],
})

router.afterEach((to) => {
  const title = to.meta['title']
  document.title = `${typeof title === 'string' ? title : 'Cosmoscope'} — Cosmoscope`
})

export default router
