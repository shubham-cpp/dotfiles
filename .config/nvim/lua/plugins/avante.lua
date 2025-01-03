local function get_ollama_setup()
  local ollama_setup = {
    -- add any opts here
    ---@type Provider
    provider = 'gemini',
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
  }
  return ollama_setup
end

---@type LazySpec
return {
  {
    'yetone/avante.nvim',
    event = 'VeryLazy',
    version = false,
    build = 'make',
    opts = function()
      vim.env.GEMINI_API_KEY = require('plugins.config.util').get_age_credentials 'gemini_api.age'
      if not vim.env.GEMINI_API_KEY then
        return get_ollama_setup()
      end
      return {
        ---@type Provider
        provider = 'gemini',
        gemini = {
          model = 'gemini-2.0-flash-exp',
        },
      }
    end,
    dependencies = {
      'KingMichaelPark/age.nvim', -- Add age dependency
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
  },
  {
    'olimorris/codecompanion.nvim',
    cmd = {
      'CodeCompanion',
      'CodeCompanionChat',
      'CodeCompanionCmd',
      'CodeCompanionActions',
    },
    keys = {
      { '<leader>cc', '<cmd>CodeCompanionChat toggle<CR>', desc = 'Chat Toggle', mode = { 'n', 'x' } },
      { '<leader>cA', '<cmd>CodeCompanionChat add<CR>', desc = 'Chat Add Selection', mode = 'x' },
      { '<leader>ca', '<cmd>CodeCompanionActions<CR>', desc = 'Chat Actions', mode = { 'n', 'x' } },
      { '<leader>cd', '<cmd>CodeCompanionCmd<space>/', desc = 'Chat Cmd' },
    },
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      'KingMichaelPark/age.nvim',
      {
        'MeanderingProgrammer/render-markdown.nvim',
        opts = {
          file_types = { 'markdown', 'Avante', 'codecompanion' },
        },
        ft = { 'markdown', 'Avante', 'codecompanion' },
      },
    },
    config = function()
      require('codecompanion').setup({
        display = { chat = { render_headers = false } },
        strategies = {
          --NOTE: Change the adapter as required
          chat = { adapter = 'gemini' },
          inline = { adapter = 'gemini' },
        },
        adapters = {
          openai_compatible = function()
            return require('codecompanion.adapters').extend('openai_compatible', {
              env = {
                url = 'https://glhf.chat',
                api_key = function()
                  return require('plugins.config.util').get_age_credentials 'glhf.age'
                end,
                chat_url = '/api/openai/v1/chat/completions',
              },
              schema = {
                model = { default = 'hf:Qwen/Qwen2.5-Coder-32B-Instruct' },
                num_ctx = { default = 32768 },
              },
            })
          end,
          gemini = function()
            return require('codecompanion.adapters').extend('gemini', {
              schema = {
                model = {
                  default = 'gemini-2.0-flash-exp',
                },
              },
            })
          end,
        },
      })
    end,
  },
}
