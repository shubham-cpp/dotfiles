---@type LazySpec
return {
  'yetone/avante.nvim',
  event = 'VeryLazy',
  version = false, -- set this if you want to always pull the latest change
  -- keys = function(_, keys)
  --   ---@type avante.Config
  --   local opts =
  --     require('lazy.core.plugin').values(require('lazy.core.config').spec.plugins['avante.nvim'], 'opts', false)
  --
  --   local mappings = {
  --     {
  --       opts.mappings.ask,
  --       function()
  --         require('avante.api').ask()
  --       end,
  --       desc = 'avante: ask',
  --       mode = { 'n', 'v' },
  --     },
  --     {
  --       opts.mappings.refresh,
  --       function()
  --         require('avante.api').refresh()
  --       end,
  --       desc = 'avante: refresh',
  --       mode = 'v',
  --     },
  --     {
  --       opts.mappings.edit,
  --       function()
  --         require('avante.api').edit()
  --       end,
  --       desc = 'avante: edit',
  --       mode = { 'n', 'v' },
  --     },
  --   }
  --   mappings = vim.tbl_filter(function(m)
  --     return m[1] and #m[1] > 0
  --   end, mappings)
  --   return vim.list_extend(mappings, keys)
  -- end,
  opts = {
    -- add any opts here
    ---@alias Provider "claude" | "openai" | "azure" | "gemini" | "cohere" | "copilot" | string
    provider = 'ollama',
    vendors = {
      ---@type AvanteProvider
      ollama = {
        ['local'] = true,
        endpoint = '127.0.0.1:11434/v1',
        model = 'llama3.2',
        parse_curl_args = function(opts, code_opts)
          return {
            url = opts.endpoint .. '/chat/completions',
            headers = {
              ['Accept'] = 'application/json',
              ['Content-Type'] = 'application/json',
            },
            body = {
              model = opts.model,
              messages = require('avante.providers').copilot.parse_message(code_opts), -- you can make your own message, but this is very advanced
              max_tokens = 2048,
              stream = true,
            },
          }
        end,
        parse_response_data = function(data_stream, event_state, opts)
          require('avante.providers').openai.parse_response(data_stream, event_state, opts)
        end,
      },
    },
  },
  -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
  build = 'make',
  -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
  dependencies = {
    'nvim-treesitter/nvim-treesitter',
    {
      'stevearc/dressing.nvim',
      opts = {
        select = {
          -- Priority list of preferred vim.select implementations
          backend = { 'fzf_lua', 'telescope', 'fzf', 'builtin', 'nui' },
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
    'nvim-lua/plenary.nvim',
    'MunifTanjim/nui.nvim',
    --- The below dependencies are optional,
    'nvim-tree/nvim-web-devicons', -- or echasnovski/mini.icons
    'zbirenbaum/copilot.lua', -- for providers='copilot'
    {
      -- support for image pasting
      'HakonHarnes/img-clip.nvim',
      event = 'VeryLazy',
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
      'MeanderingProgrammer/render-markdown.nvim',
      opts = {
        file_types = { 'markdown', 'Avante' },
      },
      ft = { 'markdown', 'Avante' },
    },
  },
}
