import { z } from "zod"

export const providerUsageSchema = z.object({
  provider: z.string(),
  status: z.enum(["success", "auth-unavailable", "usage-unavailable"]),
  windows: z.array(z.object({ label: z.string(), remaining: z.number().min(0).max(100).nullable() })),
  resetsAt: z.string().optional(),
  note: z.string().optional(),
})

export const usageSnapshotSchema = z.object({ providers: z.array(providerUsageSchema) })
export type ProviderUsage = z.infer<typeof providerUsageSchema>
export type UsageSnapshot = z.infer<typeof usageSnapshotSchema>

export function usageProvider(model: { providerID: string; id: string } | undefined): string | undefined {
  if (!model) return undefined
  const provider = model.providerID.toLowerCase()
  const id = model.id.toLowerCase().split("/").at(-1) ?? ""
  if (id.startsWith("grok") || ["grok", "grok-build", "xai"].includes(provider)) return "Grok"
  if (/^(gpt-|chatgpt-|o[1-9](?:-|$)|codex)/.test(id) || provider === "openai") return "OpenAI"
  return undefined
}
