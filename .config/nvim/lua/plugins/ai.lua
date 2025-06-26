---@type LazySpec
return {
  "olimorris/codecompanion.nvim",
  cmd = {
    "CodeCompanion",
    "CodeCompanionChat",
    "CodeCompanionCmd",
    "CodeCompanionActions",
  },
  keys = {
    { "<leader>c", "", desc = "+codecompanion", mode = { "n", "x" } },
    { "<leader>cc", "<cmd>CodeCompanionChat toggle<CR>", desc = "Chat Toggle", mode = { "n", "x" } },
    { "<leader>cA", "<cmd>CodeCompanionChat add<CR>", desc = "Chat Add Selection", mode = "x" },
    { "<leader>ca", "<cmd>CodeCompanionActions<CR>", desc = "Chat Actions", mode = { "n", "x" } },
    { "<leader>cd", "<cmd>CodeCompanionCmd<space>/", desc = "Chat Cmd" },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    {
      "MeanderingProgrammer/render-markdown.nvim",
      opts = { file_types = { "markdown", "Avante", "codecompanion" } },
      ft = { "markdown", "Avante", "codecompanion" },
    },
  },
  opts = {
    display = { chat = { render_headers = false } },
    strategies = {
      --NOTE: Change the adapter as required
      chat = {
        adapter = "gemini",
        slash_commands = {
          ["file"] = {
            -- Location to the slash command in CodeCompanion
            callback = "strategies.chat.slash_commands.file",
            description = "Select a file using Telescope",
            opts = {
              ---@type 'default'| 'mini_pick'| 'fzf_lua'| 'snacks'|'telescope'
              provider = "snacks", -- Other options include
              contains_code = true,
            },
          },
        },
      },
      inline = { adapter = "gemini" },
    },
    adapters = {
      gemini = function()
        return require("codecompanion.adapters").extend("gemini", {
          env = {
            api_key = "cmd:age -i ~/.config/age/identity.txt -d ~/.config/age/gemini_api.age",
          },
          schema = { model = { default = "gemini-2.5-flash" } },
        })
      end,
    },
  },
}
