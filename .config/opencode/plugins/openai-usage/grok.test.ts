import { afterEach, expect, mock, spyOn, test } from "bun:test"
import { fetchGrokUsage, parseGrokUsage } from "./grok"

afterEach(() => mock.restore())

test("uses shared weekly percentage instead of manufacturing OpenAI windows", () => {
  const result = parseGrokUsage({ config: {
    creditUsagePercent: 42.5,
    currentPeriod: { type: "USAGE_PERIOD_TYPE_WEEKLY", end: "2026-09-20T00:00:00Z" },
    isUnifiedBillingUser: true,
    productUsage: [{ product: "PRODUCT_GROK_BUILD", usagePercent: 61.2 }],
  } })
  expect(result.windows).toEqual([{ label: "Weekly", remaining: 58 }])
  expect(result.resetsAt).toBe("2026-09-20T00:00:00Z")
  expect(result.note).toBe("Shared Grok allowance")
})

test("supports legacy monthly cents, including omitted protobuf zero", () => {
  expect(parseGrokUsage({ config: { monthlyLimit: { val: "1000" }, used: { val: "250" } } }).windows)
    .toEqual([{ label: "Monthly", remaining: 75 }])
  expect(parseGrokUsage({ config: { monthlyLimit: { val: 1000 }, used: {} } }).windows[0].remaining).toBe(100)
})

test("missing or malformed allowance is unknown, not zero usage", () => {
  for (const value of [null, { config: null }, { config: {} },
    { config: { monthlyLimit: {}, used: {} } },
    { config: { creditUsagePercent: -1 } },
    { config: { creditUsagePercent: "42" } },
  ]) expect(parseGrokUsage(value).status).toBe("usage-unavailable")
  expect(parseGrokUsage({ config: { creditUsagePercent: 110 } }).windows[0].remaining).toBe(0)
})

test("resolves user identity before billing and returns only normalized quota", async () => {
  const requests: { url: string; headers: Headers }[] = []
  spyOn(globalThis, "fetch").mockImplementation(async (input, init) => {
    requests.push({ url: String(input), headers: new Headers(init?.headers) })
    return Response.json(requests.length === 1 ? { userId: "fixture-user" }
      : { config: { creditUsagePercent: 10, currentPeriod: { type: "USAGE_PERIOD_TYPE_MONTHLY" } } })
  })
  const result = await fetchGrokUsage("fixture-token")
  expect(requests.map(item => item.url)).toEqual([
    "https://cli-chat-proxy.grok.com/v1/user",
    "https://cli-chat-proxy.grok.com/v1/billing?format=credits",
  ])
  expect(requests[1].headers.get("x-userid")).toBe("fixture-user")
  expect(requests[1].headers.get("authorization")).toBe("Bearer fixture-token")
  expect(result.windows).toEqual([{ label: "Monthly", remaining: 90 }])
  expect(JSON.stringify(result)).not.toContain("fixture-")
})

test("does not request billing after authentication failure", async () => {
  const fetch = spyOn(globalThis, "fetch").mockResolvedValue(new Response(null, { status: 401 }))
  expect((await fetchGrokUsage("fixture-token")).status).toBe("usage-unavailable")
  expect(fetch).toHaveBeenCalledTimes(1)
})
