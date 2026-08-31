import { getCurrentScope, onScopeDispose } from 'vue'

export type LatestToken = symbol

export interface Latest {
  begin: () => LatestToken
  current: (token: LatestToken) => boolean
  invalidate: () => void
}

export function useLatest(): Latest {
  let latest: LatestToken | undefined
  let disposed = false
  const invalidate = (): void => { latest = undefined }
  if (getCurrentScope() !== undefined) onScopeDispose(() => { disposed = true; invalidate() })
  return {
    begin: () => {
      const token = Symbol()
      if (!disposed) latest = token
      return token
    },
    current: (token) => latest === token,
    invalidate,
  }
}

export interface LatestSubscription extends Latest {
  owned: () => boolean
  own: (token: LatestToken, cleanup: () => void) => boolean
}

export function useLatestSubscription(): LatestSubscription {
  const latest = useLatest()
  let owned: (() => void) | undefined
  const release = (): void => {
    const cleanup = owned
    owned = undefined
    cleanup?.()
  }
  const begin = (): LatestToken => {
    const token = latest.begin()
    release()
    return token
  }
  const invalidate = (): void => {
    latest.invalidate()
    release()
  }
  if (getCurrentScope() !== undefined) onScopeDispose(release)
  return {
    begin,
    current: latest.current,
    owned: () => owned !== undefined,
    invalidate,
    own: (token, cleanup) => {
      if (!latest.current(token)) { cleanup(); return false }
      release()
      owned = cleanup
      return true
    },
  }
}
