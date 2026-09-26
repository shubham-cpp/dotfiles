
const FIVE_HOURS_SECONDS = 5 * 60 * 60
const WEEK_SECONDS = 7 * 24 * 60 * 60
const REQUEST_TIMEOUT_MS = 10_000
const USAGE_URL = "https://chatgpt.com/backend-api/wham/usage"

export type RemainingUsage = {
  fiveHour: number | null
  weekly: number | null
}

export type UsageResult =
  | { status: "success"; usage: RemainingUsage }
  | { status: "auth-unavailable" }
  | { status: "usage-unavailable" }

export type Credentials = {
  accessToken: string
  accountId: string
}

type UsageWindow = {
  seconds: number
  remaining: number
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value)
}

export function remainingPercent(usedPercent: number): number {
  return Math.round(Math.max(0, Math.min(100, 100 - usedPercent)))
}

function parseWindow(value: unknown): UsageWindow | null {
  if (!isRecord(value)) return null

  const usedPercent = value.used_percent
  const seconds = value.limit_window_seconds
  if (typeof usedPercent !== "number" || !Number.isFinite(usedPercent)) return null
  if (typeof seconds !== "number" || !Number.isFinite(seconds) || seconds <= 0) return null

  return { seconds, remaining: remainingPercent(usedPercent) }
}

export function parseRemainingUsage(value: unknown): RemainingUsage | null {
  if (!isRecord(value) || !isRecord(value.rate_limit)) return null

  const windows = [
    parseWindow(value.rate_limit.primary_window),
    parseWindow(value.rate_limit.secondary_window),
  ].filter((window): window is UsageWindow => window !== null)

  const fiveHour = windows.find((window) => window.seconds === FIVE_HOURS_SECONDS)?.remaining ?? null
  const weekly = windows.find((window) => window.seconds === WEEK_SECONDS)?.remaining ?? null
  if (fiveHour === null && weekly === null) return null

  return { fiveHour, weekly }
}

function decodeJwtPayload(token: string): Record<string, unknown> | null {
  const payload = token.split(".")[1]
  if (!payload) return null

  try {
    const decoded = Buffer.from(payload, "base64url").toString("utf8")
    const value = JSON.parse(decoded)
    return isRecord(value) ? value : null
  } catch {
    return null
  }
}

export function extractAccountId(token: string): string | null {
  const payload = decodeJwtPayload(token)
  const claim = payload?.["https://api.openai.com/auth"]
  if (!isRecord(claim)) return null
  return typeof claim.chatgpt_account_id === "string" ? claim.chatgpt_account_id : null
}

async function requestUsage(credentials: Credentials, signal?: AbortSignal): Promise<unknown | null> {
  const controller = new AbortController()
  const abort = () => controller.abort()
  const timer = setTimeout(abort, REQUEST_TIMEOUT_MS)

  if (signal?.aborted) controller.abort()
  else signal?.addEventListener("abort", abort, { once: true })

  try {
    const response = await fetch(USAGE_URL, {
      headers: {
        Accept: "application/json",
        Authorization: `Bearer ${credentials.accessToken}`,
        "ChatGPT-Account-Id": credentials.accountId,
        "User-Agent": "opencode-openai-usage",
      },
      signal: controller.signal,
    })
    if (!response.ok) return null
    return await response.json()
  } catch {
    return null
  } finally {
    clearTimeout(timer)
    signal?.removeEventListener("abort", abort)
  }
}

export async function fetchOpenAIUsage(credentials: Credentials | null, signal?: AbortSignal): Promise<UsageResult> {
  if (!credentials) return { status: "auth-unavailable" }

  const response = await requestUsage(credentials, signal)
  const usage = parseRemainingUsage(response)
  return usage ? { status: "success", usage } : { status: "usage-unavailable" }
}
