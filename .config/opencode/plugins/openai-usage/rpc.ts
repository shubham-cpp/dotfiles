import { Rpc } from "@opencode/plugin/rpc"
import { z } from "zod"
import { usageSnapshotSchema } from "./usage"

export const Usage = Rpc.define({
  id: "openai-usage",
  events: {},
  methods: { get: { input: z.object({}), output: usageSnapshotSchema } },
})
