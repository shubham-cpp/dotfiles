local init = {
  ['goolord/alpha-nvim'] = { disable = true },
  ['akinsho/bufferline.nvim'] = { disable = true },
  ['svermeulen/vim-subversive'] = {
    event = 'BufWinEnter',
    config = function()
      require 'user.plugins.subversive'
    end,
  },
  ['nvim-treesitter/nvim-treesitter-textobjects'] = {
    after = 'nvim-treesitter',
    commit = '98476e7364821989ab9b500e4d20d9ae2c5f6564',
  },
  ['kylechui/nvim-surround'] = {
    config = function()
      require('nvim-surround').setup({})
    end,
    after = 'indent-blankline.nvim',
    commit = '6b45fbffdabb2d8cd80d310006c92e59cec8fd74',
  },
  ['unblevable/quick-scope'] = {
    setup = function()
      vim.g.qs_highlight_on_keys = { 'f', 'F', 't', 'T' }
      vim.g.qs_buftype_blacklist = {
        'terminal',
        'nofile',
        'fzf',
        'floaterm',
        'NvimTree',
      }
      vim.g.qs_lazy_highlight = 1
    end,
  },

  ['Shatur/neovim-session-manager'] = {
    event = 'VimEnter',
    config = function()
      local sm = require 'session_manager'
      local conf = require 'session_manager.config'
      sm.setup({
        -- Define what to do when Neovim is started without arguments.
        -- Possible values: Disabled, CurrentDir, LastSession
        autoload_mode = conf.AutoloadMode.CurrentDir,
      })
    end,
    commit = '7cde4df6790c708ea304f17ac92f00425bd63e9a',
  },
  ['monaqa/dial.nvim'] = {
    config = function()
      require 'user.plugins.dial'
    end,
    event = 'BufEnter',
    commit = '9ba17c2ee636a8e7fdef5b69d6aac54dd26f4384',
  },
  ['mg979/vim-visual-multi'] = {
    setup = function()
      vim.g.VM_maps = {}
      vim.g.VM_mouse_mappings = 1
      vim.g.VM_maps = {
        ['Find Under'] = '<M-d>',
        ['Find Subword Under'] = '<M-d>',
        ['Skip Region'] = '<C-x>',
        ['Select All'] = '<M-a>',
        ['Start Regex Search'] = '\\/',
      }
    end,
    after = 'dial.nvim',
  },
  ['phaazon/hop.nvim'] = {
    setup = function()
      vim.keymap.set('n', 'S', ':HopWord<cr>')
      vim.keymap.set('n', 's', ':HopChar2<cr>', { silent = false })
    end,
    config = function()
      require('hop').setup()
    end,
    cmd = { 'HopChar2', 'HopWord' },
    commit = '90db1b2c61b820e230599a04fedcd2679e64bd07',
  },
  ['rafcamlet/tabline-framework.nvim'] = {
    config = function()
      require 'user.plugins.tabline'
    end,
  },
  ['tweekmonster/startuptime.vim'] = { cmd = 'StartupTime' },
  ['hrsh7th/cmp-nvim-lsp-signature-help'] = {
    after = 'nvim-cmp',
    config = function()
      astronvim.add_cmp_source({ name = 'nvim_lsp_signature_help', priority = 700 })
    end,
  },
  ['hrsh7th/cmp-cmdline'] = { after = 'nvim-cmp' },
  ['tzachar/cmp-tabnine'] = {
    after = 'nvim-cmp',
    run = './install.sh',
    config = function()
      astronvim.add_cmp_source({ name = 'cmp_tabnine', priority = 100 })
    end,
  },
}
return init
