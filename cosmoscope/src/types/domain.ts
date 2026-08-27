export type FixtureScenario = 'ready' | 'loading' | 'empty' | 'error' | 'offline' | 'forbidden'
export type Status = 'running' | 'waiting' | 'completed' | 'failed' | 'stopped' | 'degraded'

export interface Task {
  id: string
  label: string
  detail: string
  owner: string
  platform: string
  status: Status
  started: string
  elapsed: string
}

export interface Activity {
  id: string
  kind: string
  summary: string
  source: string
  time: string
  tone: 'success' | 'info' | 'warning' | 'danger'
}

export interface PlatformSummary {
  id: string
  name: string
  state: 'Connected' | 'Degraded'
  messages: number
}

export interface Plugin {
  id: string
  name: string
  description: string
  version: string
  tools: number
  routes: number
  status: 'Loaded' | 'Stopped'
  error?: string
}

export interface LogEntry {
  id: string
  time: string
  level: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR'
  source: string
  message: string
  fields: string
}

export interface OverviewSnapshot {
  tasks: Task[]
  activity: Activity[]
  platforms: PlatformSummary[]
}
