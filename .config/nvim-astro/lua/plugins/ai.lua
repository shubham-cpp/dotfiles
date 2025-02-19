--- A wrapper around `age.nvim` to get the credentials from the identity file.
---@param secret_file string The secret file to read the credentials from(`secret_file` should be in location `$HOME/.config/age/`).
---@return string|nil secret The credentials from the identity file.
local function get_age_credentials(secret_file)
  if 0 == vim.fn.filereadable(vim.fn.expand "$HOME/.config/age/identity.txt") then return nil end
  local identity = vim.fn.expand "$HOME/.config/age/identity.txt"
  local secret = vim.fn.expand("$HOME/.config/age/" .. secret_file)
  return require("age").get(secret, identity)
end

local function get_ollama_setup()
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
  return ollama_setup
end

---@type LazySpec
return {
  {
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
      require("codecompanion").setup {
        display = { chat = { render_headers = false } },
        strategies = {
          --NOTE: Change the adapter as required
          chat = { adapter = "gemini" },
          inline = { adapter = "gemini" },
        },
        adapters = {
          gemini = function()
            return require("codecompanion.adapters").extend("gemini", {
              env = {
                api_key = function() return get_age_credentials "gemini_api.age" end,
              },
              schema = {
                model = {
                  default = "gemini-2.0-flash",
                  -- default = "gemini-2.0-flash-thinking-exp-01-21",
                },
              },
            })
          end,
        },
      }
    end,
  },
  {
    "yetone/avante.nvim",
    enabled = false,
    version = false,
    build = "make",
    event = "InsertEnter",
    opts = function()
      vim.env.GEMINI_API_KEY = get_age_credentials "gemini_api.age"
      if not vim.env.GEMINI_API_KEY then return get_ollama_setup() end
      return {
        ---@type Provider
        provider = "gemini",
        gemini = {
          model = "gemini-2.0-flash",
          -- model = "gemini-exp-1206",
        },
      }
    end,
    dependencies = {
      "KingMichaelPark/age.nvim", -- Add age dependency
      "nvim-treesitter/nvim-treesitter",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "MeanderingProgrammer/render-markdown.nvim",
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
    },
  },
}
