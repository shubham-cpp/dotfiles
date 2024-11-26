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
    { "<leader>cc", "<cmd>CodeCompanionChat toggle<CR>", desc = "Chat Toggle", mode = { "n", "x" } },
    { "<leader>cA", "<cmd>CodeCompanionChat add<CR>", desc = "Chat Add Selection", mode = "x" },
    { "<leader>ca", "<cmd>CodeCompanionActions<CR>", desc = "Chat Actions", mode = { "n", "x" } },
    { "<leader>cd", "<cmd>CodeCompanionCmd<space>/", desc = "Chat Cmd" },
  },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",
    "KingMichaelPark/age.nvim",
    {
      "MeanderingProgrammer/render-markdown.nvim",
      opts = {
        file_types = { "markdown", "Avante", "codecompanion" },
      },
      ft = { "markdown", "Avante", "codecompanion" },
    },
  },
  config = function()
    require("codecompanion").setup({
      display = { chat = { render_headers = false } },
      strategies = {
        --NOTE: Change the adapter as required
        chat = { adapter = "openai_compatible" },
        inline = { adapter = "openai_compatible" },
      },
      adapters = {
        openai_compatible = function()
          return require("codecompanion.adapters").extend("openai_compatible", {
            env = {
              url = "https://glhf.chat",
              api_key = function()
                require("m-utils").get_age_credentials("glhf.age")
              end,
              chat_url = "/api/openai/v1/chat/completions",
            },
            schema = {
              model = { default = "hf:Qwen/Qwen2.5-Coder-32B-Instruct" },
              num_ctx = { default = 32768 },
            },
          })
        end,
      },
    })
  end,
}
