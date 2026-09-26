import { Plugin } from "@opencode/plugin"
import { extractAccountId, fetchOpenAIUsage } from "./core"
import { Usage } from "./rpc"
import type { ProviderUsage } from "./usage"
import { fetchGrokUsage } from "./grok"

export default Plugin.define({
  id: "openai-usage",
  async setup(ctx) {
    const openai = async (signal: AbortSignal): Promise<ProviderUsage> => {
      const connection = await ctx.integration.connection.active("openai")
      const credential = connection ? await ctx.integration.connection.resolve(connection) : undefined
      if (credential?.type !== "oauth") return { provider: "OpenAI", status: "auth-unavailable", windows: [] }
      const explicit = credential.metadata?.accountId
      const accountId = typeof explicit === "string" ? explicit : extractAccountId(credential.access)
      const result = await fetchOpenAIUsage(accountId ? { accessToken: credential.access, accountId } : null, signal)
      return {
        provider: "OpenAI",
        status: result.status,
        windows: result.status === "success" ? [
          { label: "5H", remaining: result.usage.fiveHour },
          { label: "Weekly", remaining: result.usage.weekly },
        ] : [],
      }
    }

    const grok = async (signal: AbortSignal): Promise<ProviderUsage> => {
      const connection = await ctx.integration.connection.active("xai")
      const credential = connection ? await ctx.integration.connection.resolve(connection) : undefined
      if (credential?.type !== "oauth") return { provider: "Grok", status: "auth-unavailable", windows: [] }
      return fetchGrokUsage(credential.access, signal)
    }

    await ctx.rpc.register(Usage, {
      get: async (_input, request) => {
        return {
          providers: await Promise.all([
            openai(request.signal).catch((): ProviderUsage => ({ provider: "OpenAI", status: "usage-unavailable", windows: [] })),
            grok(request.signal).catch((): ProviderUsage => ({ provider: "Grok", status: "usage-unavailable", windows: [] })),
          ]),
        }
      },
    })
  },
})
