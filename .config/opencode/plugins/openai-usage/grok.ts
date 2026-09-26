import { z } from "zod"
import type { ProviderUsage } from "./usage"

const cents = z.object({ val: z.union([z.number(), z.string().regex(/^-?\d+$/)]).optional() })
const billing = z.object({
  config: z.object({
    creditUsagePercent: z.number().finite().nullish(),
    currentPeriod: z.object({ type: z.string().optional(), end: z.string().optional() }).nullish(),
    monthlyLimit: cents.nullish(),
    used: cents.nullish(),
    billingPeriodEnd: z.string().optional(),
    isUnifiedBillingUser: z.boolean().optional(),
  }).nullable(),
})

export function parseGrokUsage(value: unknown): ProviderUsage {
  const unavailable: ProviderUsage = { provider: "Grok", status: "usage-unavailable", windows: [] }
  const decoded = billing.safeParse(value)
  if (!decoded.success || !decoded.data.config) return unavailable
  const config = decoded.data.config
  const limit = config.monthlyLimit ? Number(config.monthlyLimit.val ?? 0) : undefined
  const used = config.used ? Number(config.used.val ?? 0) : undefined
  const percent = config.creditUsagePercent ?? (
    limit !== undefined && limit > 0 && used !== undefined ? used / limit * 100 : undefined
  )
  if (percent === undefined || !Number.isFinite(percent) || percent < 0) return unavailable
  const period = config.currentPeriod?.type
  const label = period === "USAGE_PERIOD_TYPE_WEEKLY" ? "Weekly"
    : period === "USAGE_PERIOD_TYPE_MONTHLY" || (!period && limit !== undefined) ? "Monthly" : "Allowance"
  const end = config.currentPeriod?.end ?? config.billingPeriodEnd
  return {
    provider: "Grok",
    status: "success",
    windows: [{ label, remaining: Math.round(Math.max(0, 100 - percent)) }],
    resetsAt: end && Number.isFinite(Date.parse(end)) ? end : undefined,
    note: config.isUnifiedBillingUser ? "Shared Grok allowance" : undefined,
  }
}

// First-party Grok Build protocol; see docs/grok-usage-research.md.
export async function fetchGrokUsage(accessToken: string, signal?: AbortSignal): Promise<ProviderUsage> {
  const requestSignal = AbortSignal.any([AbortSignal.timeout(15_000), ...(signal ? [signal] : [])])
  const headers: Record<string, string> = {
    Authorization: `Bearer ${accessToken}`,
    Accept: "application/json",
    "X-XAI-Token-Auth": "xai-grok-cli",
    "x-grok-client-mode": "headless",
    "User-Agent": "opencode-usage/1.0",
  }
  const get = async (path: string) => {
    const response = await fetch(`https://cli-chat-proxy.grok.com/v1/${path}`, {
      headers, signal: requestSignal, redirect: "error",
    })
    if (!response.ok) return null
    return response.json() as Promise<unknown>
  }
  try {
    const user = z.object({ userId: z.string().min(1) }).safeParse(await get("user"))
    if (!user.success) return { provider: "Grok", status: "usage-unavailable", windows: [] }
    headers["x-userid"] = user.data.userId
    return parseGrokUsage(await get("billing?format=credits"))
  } catch {
    return { provider: "Grok", status: "usage-unavailable", windows: [] }
  }
}
