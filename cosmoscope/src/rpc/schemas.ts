import { z } from 'zod'

export const concurrencyListSchema = z.object({
  entries: z.array(z.object({
    id: z.number(),
    label: z.string(),
    status: z.enum(['running', 'completed', 'failed', 'cancelled']),
    error: z.string().nullable(),
    startedAt: z.string(),
    finishedAt: z.string().nullable(),
  })),
})
