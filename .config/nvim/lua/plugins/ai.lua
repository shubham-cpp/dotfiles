---@type LazySpec
return {
  {
    "olimorris/codecompanion.nvim",
    enabled = false,
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
      {
        "ravitemer/mcphub.nvim",
        dependencies = {
          "nvim-lua/plenary.nvim",
        },
        build = "bun install -g mcp-hub@latest",
        opts = {},
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
        open_router = function()
          return require("codecompanion.adapters").extend("openai_compatible", {
            env = {
              url = "https://openrouter.ai/api",
              api_key = "cmd:age -i ~/.config/age/identity.txt -d ~/.config/age/roo_open_router.age",
            },
            schema = {
              model = {
                default = "z-ai/glm-4.5-air:free",
                choices = {
                  ["qwen/qwen3-253b-a22b:free"] = {
                    formatted_name = "Qwen3 253b",
                    opts = { has_vision = true, can_reason = true, stream = true },
                  },
                  ["qwen/qwen3-coder:free"] = {
                    formatted_name = "Qwen 3 480b",
                    opts = { has_vision = true, can_reason = true, stream = true },
                  },
                  ["minimax/minimax-m2:free"] = {
                    formatted_name = "MiniMax m2",
                    opts = { stream = true },
                  },
                  ["z-ai/glm-4.5-air:free"] = {
                    formatted_name = "GLM 4.5-air",
                    opts = { stream = true, can_reason = true },
                  },
                  ["moonshotai/kimi-k2:free"] = {
                    formatted_name = "Kimi-k2",
                    opts = { stream = true, can_reason = true },
                  },
                },
              },
            },
          })
        end,
      },
      extensions = {
        mcphub = {
          callback = "mcphub.extensions.codecompanion",
          opts = {
            -- MCP Tools
            make_tools = true, -- Make individual tools (@server__tool) and server groups (@server) from MCP servers
            show_server_tools_in_chat = true, -- Show individual tools in chat completion (when make_tools=true)
            add_mcp_prefix_to_tool_names = false, -- Add mcp__ prefix (e.g `@mcp__github`, `@mcp__neovim__list_issues`)
            show_result_in_chat = true, -- Show tool results directly in chat buffer
            format_tool = nil, -- function(tool_name:string, tool: CodeCompanion.Agent.Tool) : string Function to format tool names to show in the chat buffer
            -- MCP Resources
            make_vars = true, -- Convert MCP resources to #variables for prompts
            -- MCP Prompts
            make_slash_commands = true, -- Add MCP prompts as /slash commands
          },
        },
      },
    },
  },
  {
    "yetone/avante.nvim",
    build = "make BUILD_FROM_SOURCE=true",
    event = "VeryLazy",
    version = false,
    ---@module 'avante'
    ---@type avante.Config
    opts = {
      instructions_file = "avante.md",
      provider = "qwen3_253b",
      providers = {
        qwen3_253b = {
          __inherited_from = "openai",
          endpoint = "https://openrouter.ai/api/v1",
          api_key_name = "cmd:age -i ~/.config/age/identity.txt -d ~/.config/age/roo_open_router.age",
          model = "qwen/qwen3-253b-a22b:free",
        },
        qwen3_coder = {
          __inherited_from = "openai",
          endpoint = "https://openrouter.ai/api/v1",
          api_key_name = "cmd:age -i ~/.config/age/identity.txt -d ~/.config/age/roo_open_router.age",
          model = "qwen/qwen3-coder:free",
        },
        minimax_m2 = {
          __inherited_from = "openai",
          endpoint = "https://openrouter.ai/api/v1",
          api_key_name = "cmd:age -i ~/.config/age/identity.txt -d ~/.config/age/roo_open_router.age",
          model = "minimax/minimax-m2:free",
        },
        glm_4_5_air = {
          __inherited_from = "openai",
          endpoint = "https://openrouter.ai/api/v1",
          api_key_name = "cmd:age -i ~/.config/age/identity.txt -d ~/.config/age/roo_open_router.age",
          model = "z-ai/glm-4.5-air:free",
        },
        kimi_k2 = {
          __inherited_from = "openai",
          endpoint = "https://openrouter.ai/api/v1",
          api_key_name = "cmd:age -i ~/.config/age/identity.txt -d ~/.config/age/roo_open_router.age",
          model = "moonshotai/kimi-k2:free",
        },
      },
      system_prompt = function()
        local hub = require("mcphub").get_hub_instance()
        return hub and hub:get_active_servers_prompt() or ""
      end,
      -- Using function prevents requiring mcphub before it's loaded
      custom_tools = function()
        return {
          require("mcphub.extensions.avante").mcp_tool(),
        }
      end,
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      "folke/snacks.nvim", -- for input provider snacks
      {
        -- Make sure to set this up properly if you have lazy=true
        "MeanderingProgrammer/render-markdown.nvim",
        opts = {
          file_types = { "markdown", "Avante" },
        },
        ft = { "markdown", "Avante" },
      },
      {
        "ravitemer/mcphub.nvim",
        dependencies = {
          "nvim-lua/plenary.nvim",
        },
        build = "bun install -g mcp-hub@latest",
        opts = {},
      },
    },
  },
}
