import { describe, expect, test } from "bun:test"
import { extractAccountId, parseRemainingUsage, remainingPercent } from "./core"

describe("remainingPercent", () => {
  test("inverts and rounds used percentage", () => {
    expect(remainingPercent(12.4)).toBe(88)
    expect(remainingPercent(12.6)).toBe(87)
  })

  test("clamps unexpected endpoint values", () => {
    expect(remainingPercent(-20)).toBe(100)
    expect(remainingPercent(140)).toBe(0)
  })
})

describe("parseRemainingUsage", () => {
  test("maps windows by duration rather than response position", () => {
    expect(parseRemainingUsage({
      rate_limit: {
        primary_window: { used_percent: 35, limit_window_seconds: 604_800 },
        secondary_window: { used_percent: 12, limit_window_seconds: 18_000 },
      },
    })).toEqual({ fiveHour: 88, weekly: 65 })
  })

  test("retains a valid window when the other is missing", () => {
    expect(parseRemainingUsage({
      rate_limit: {
        primary_window: { used_percent: 25, limit_window_seconds: 18_000 },
        secondary_window: null,
      },
    })).toEqual({ fiveHour: 75, weekly: null })
  })

  test("rejects malformed or unrecognized windows", () => {
    expect(parseRemainingUsage(null)).toBeNull()
    expect(parseRemainingUsage({ rate_limit: {} })).toBeNull()
    expect(parseRemainingUsage({
      rate_limit: {
        primary_window: { used_percent: "25", limit_window_seconds: 18_000 },
        secondary_window: { used_percent: 25, limit_window_seconds: 3_600 },
      },
    })).toBeNull()
  })
})

describe("extractAccountId", () => {
  test("reads the account id from the OpenAI JWT claim", () => {
    const payload = Buffer.from(JSON.stringify({
      "https://api.openai.com/auth": { chatgpt_account_id: "account-123" },
    })).toString("base64url")
    expect(extractAccountId(`header.${payload}.signature`)).toBe("account-123")
  })

  test("returns null for invalid tokens", () => {
    expect(extractAccountId("not-a-jwt")).toBeNull()
  })
})
