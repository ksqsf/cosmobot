import { onMounted, onScopeDispose } from 'vue'

export interface VisibilityPollingOptions {
  interval: number
  immediate?: boolean
}

export interface VisibilityPolling {
  start: () => void
  stop: () => void
}

export function useVisibilityPolling(task: () => Promise<void>, options: VisibilityPollingOptions): VisibilityPolling {
  let timer: ReturnType<typeof setTimeout> | undefined
  let active = false
  let running = false

  function clear(): void {
    if (timer !== undefined) clearTimeout(timer)
    timer = undefined
  }
  function schedule(): void {
    clear()
    if (!active || document.hidden) return
    timer = setTimeout(() => { void run() }, options.interval)
  }
  async function run(): Promise<void> {
    if (!active || document.hidden || running) return
    running = true
    try { await task() } finally { running = false; schedule() }
  }
  function start(): void {
    active = true
    if (options.immediate === true) void run()
    else schedule()
  }
  function stop(): void { active = false; clear() }
  function visibilityChanged(): void {
    clear()
    if (active && !document.hidden) void run()
  }

  onMounted(() => { document.addEventListener('visibilitychange', visibilityChanged) })
  onScopeDispose(() => { stop(); document.removeEventListener('visibilitychange', visibilityChanged) })
  return { start, stop }
}
