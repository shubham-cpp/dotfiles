---@type LazySpec
return {
  { "echasnovski/mini.pairs", enabled = false },
  {
    "altermo/ultimate-autopair.nvim",
    event = { "InsertEnter", "CmdlineEnter" },
    enabled = true,
    branch = "v0.6", --recommended as each new version will have breaking changes
    opts = {
      --Config goes here
    },
  },
}
