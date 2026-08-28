export function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${String(bytes)} B`
  const units = ['KB', 'MB', 'GB', 'TB'] as const
  const unit = Math.min(Math.floor(Math.log(bytes) / Math.log(1024)), units.length)
  return `${(bytes / 1024 ** unit).toFixed(unit === 1 ? 0 : 1)} ${String(units[unit - 1])}`
}
