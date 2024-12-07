local get_age_credentials = require("config.m-utils").get_age_credentials

---@type LazySpec
return {
  {
    "yetone/avante.nvim",
    version = false,
    build = "make",
    keys = {
      { "<leader>a", "", mode = { "n", "x" }, desc = "+avante" },
      { "<leader>aa", mode = { "n", "x" }, desc = "Ask" },
      { "<leader>ae", mode = { "n", "x" }, desc = "Edit" },
    },
    opts = function()
      vim.env.GEMINI_API_KEY = get_age_credentials("gemini_api.age")
      local ollama_setup = {
        -- add any opts here
        ---@type Provider
        provider = "gemini",
        vendors = {
          ---@type AvanteProvider
          ollama = {
            ["local"] = true,
            endpoint = "127.0.0.1:11434/v1",
            model = "llama3.2",
            parse_curl_args = function(opts, code_opts)
              return {
                url = opts.endpoint .. "/chat/completions",
                headers = {
                  ["Accept"] = "application/json",
                  ["Content-Type"] = "application/json",
                },
                body = {
                  model = opts.model,
                  messages = require("avante.providers").copilot.parse_message(code_opts), -- you can make your own message, but this is very advanced
                  max_tokens = 2048,
                  stream = true,
                },
              }
            end,
            parse_response_data = function(data_stream, event_state, opts)
              require("avante.providers").openai.parse_response(data_stream, event_state, opts)
            end,
          },
        },
      }
      if not vim.env.GEMINI_API_KEY then
        return ollama_setup
      end
      return {
        ---@type Provider
        provider = "gemini",
      }
    end,
    dependencies = {
      -- 'KingMichaelPark/age.nvim', -- Add age dependency
      { dir = "~/Downloads/GitClones/age.nvim" }, -- Add age dependency
      "nvim-treesitter/nvim-treesitter",
      {
        "stevearc/dressing.nvim",
        opts = {
          select = {
            -- Priority list of preferred vim.select implementations
            backend = { "telescope", "fzf_lua", "fzf", "builtin", "nui" },
            -- Options for fzf-lua
            fzf_lua = {
              winopts = {
                height = 0.5,
                width = 0.8,
              },
            },
          },
        },
      },
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
            -- required for Windows users
            use_absolute_path = true,
          },
        },
      },
      {
        -- Make sure to set this up properly if you have lazy=true
        "MeanderingProgrammer/render-markdown.nvim",
        opts = {
          file_types = { "markdown", "Avante" },
        },
        ft = { "markdown", "Avante" },
      },
    },
  },
  {
    "olimorris/codecompanion.nvim",
    cmd = {
      "CodeCompanion",
      "CodeCompanionChat",
      "CodeCompanionCmd",
      "CodeCompanionActions",
    },
    keys = {
      { "<leader>C", "", desc = "+codecompanion", mode = { "n", "x" } },
      { "<leader>Cc", "<cmd>CodeCompanionChat toggle<CR>", desc = "Chat Toggle", mode = { "n", "x" } },
      { "<leader>CA", "<cmd>CodeCompanionChat add<CR>", desc = "Chat Add Selection", mode = "x" },
      { "<leader>Ca", "<cmd>CodeCompanionActions<CR>", desc = "Chat Actions", mode = { "n", "x" } },
      { "<leader>Cd", "<cmd>CodeCompanionCmd<space>/", desc = "Chat Cmd" },
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
                  return get_age_credentials("glhf.age")
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
  },
}
