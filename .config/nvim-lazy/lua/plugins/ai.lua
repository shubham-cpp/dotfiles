--- A wrapper around `age.nvim` to get the credentials from the identity file.
---@param secret_file string The secret file to read the credentials from(`secret_file` should be in location `$HOME/.config/age/`).
---@return string|nil secret The credentials from the identity file.
local function get_age_credentials(secret_file)
  if 0 == vim.fn.filereadable(vim.fn.expand("$HOME/.config/age/identity.txt")) then
    return nil
  end
  local identity = vim.fn.expand("$HOME/.config/age/identity.txt")
  local secret = vim.fn.expand("$HOME/.config/age/" .. secret_file)
  return require("age").get(secret, identity)
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
      { "<leader>C", "", desc = "+codecompanion", mode = { "n", "x" } },
      { "<leader>Cc", "<cmd>CodeCompanionChat toggle<CR>", desc = "Chat Toggle", mode = { "n", "x" } },
      { "<leader>CA", "<cmd>CodeCompanionChat add<CR>", desc = "Chat Add Selection", mode = "x" },
      { "<leader>Ca", "<cmd>CodeCompanionActions<CR>", desc = "Chat Actions", mode = { "n", "x" } },
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
              api_key = function()
                return get_age_credentials("gemini_api.age")
              end,
            },
            schema = {
              model = {
                -- default = "gemini-2.0-flash",
                default = "gemini-2.5-flash-preview-04-17",
              },
            },
          })
        end,
      },
    },
  },
  {
    "yetone/avante.nvim",
    enabled = false,
    version = false,
    build = "make",
    event = "InsertEnter",
    opts = function()
      vim.env.GEMINI_API_KEY = get_age_credentials("gemini_api.age")

      return {
        provider = not vim.env.GEMINI_API_KEY and "ollama" or "gemini",
        providers = {
          ollama = {
            ["local"] = true,
            endpoint = "127.0.0.1:11434",
            model = "llama3.2",
            extra_request_body = {
              options = {
                temperature = 0.75,
                num_ctx = 20480,
                keep_alive = "5m",
              },
            },
          },
          gemini = {
            -- model = "gemini-2.0-flash",
            model = "gemini-2.5-flash-preview-04-17",
          },
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
