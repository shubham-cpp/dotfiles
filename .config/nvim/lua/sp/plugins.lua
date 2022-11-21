return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use 'lewis6991/impatient.nvim'
  use 'nathom/filetype.nvim'

  -- Nvim cmp {{{
  use({
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lua',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'hrsh7th/cmp-buffer',
      'hrsh7th/vim-vsnip',
      'hrsh7th/cmp-vsnip',
      'rafamadriz/friendly-snippets',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      { 'tzachar/cmp-tabnine', run = './install.sh' },
      'onsails/lspkind.nvim',
    },
    config = function()
      require 'sp.cmp'
    end,
  })
  -- }}}

  -- LSP {{{

  use({ 'nvim-lua/plenary.nvim' })
  use({
    'jose-elias-alvarez/null-ls.nvim',
    config = function()
      require 'sp.nulls'
    end,
  })
  -- use({
  --   'neoclide/coc.nvim',
  --   branch = 'release',
  --   requires = {
  --     { 'honza/vim-snippets' },
  --     { 'SirVer/ultisnips' },
  --     {
  --       'L3MON4D3/LuaSnip',
  --       tag = 'v1.*',
  --       config = function()
  --         require('luasnip.loaders.from_vscode').lazy_load()
  --         require('luasnip.loaders.from_snipmate').lazy_load()
  --       end,
  --     },
  --     { 'rafamadriz/friendly-snippets' },
  --   },
  -- })
  use({ 'williamboman/mason.nvim' })
  use({
    'williamboman/mason-lspconfig.nvim',
    after = { 'mason.nvim', 'nvim-lspconfig' },
    config = function()
      require 'sp.mason'
    end,
  })
  use({ 'b0o/schemastore.nvim' })
  use({
    'neovim/nvim-lspconfig',
    config = function()
      require 'sp.lsp'
    end,
  })
  use({
    'simrat39/rust-tools.nvim',
    after = 'mason-lspconfig.nvim',
    config = function()
      require 'sp.rust'
    end,
  })

  use({
    'j-hui/fidget.nvim',
    config = function()
      require 'sp.fidget'
    end,
  })
  -- }}}

  -- Fuzzy Finders {{{

  use 'kyazdani42/nvim-web-devicons'
  use({
    'nvim-telescope/telescope.nvim',
    config = function()
      require 'sp.telescope'
    end,
    requires = { { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' } },
  })
  use({
    'ibhagwan/fzf-lua',
    config = function()
      require 'sp.fzf-lua'
    end,
  })
  -- }}}

  -- TreeSitter {{{
  use({
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      require 'sp.tree'
    end,
  })
  use({ 'jose-elias-alvarez/nvim-lsp-ts-utils' })
  use({ 'JoosepAlviste/nvim-ts-context-commentstring' })
  use({ 'windwp/nvim-ts-autotag' })
  use({ 'nvim-treesitter/nvim-treesitter-textobjects' })
  use({ 'nvim-treesitter/playground' })
  use({ 'andymass/vim-matchup' })
  -- }}}

  -- Theming {{{
  use({
    'Tsuzat/NeoSolarized.nvim',
    disable = false,
    config = function()
      require 'sp.colors.solarize'
    end,
  })
  use({
    'ellisonleao/gruvbox.nvim',
    disable = true,
    config = function()
      require 'sp.colors.gruvbox'
    end,
  })
  use({
    'navarasu/onedark.nvim',
    disable = true,
    config = function()
      require 'sp.colors.onedark'
    end,
  })
  use({
    'catppuccin/nvim',
    as = 'catppuccin',
    disable = true,
    config = function()
      require 'sp.colors.cat'
    end,
  })
  use({
    'folke/tokyonight.nvim',
    disable = true,
    config = function()
      require 'sp.colors.tokyo'
    end,
  })
  use({
    'nvim-lualine/lualine.nvim',
    config = function()
      require 'sp.lualine'
    end,
    -- requires = { 'kyazdani42/nvim-web-devicons', opt = true },
  })
  use({
    'rafcamlet/tabline-framework.nvim',
    config = function()
      require 'sp.tabline'
    end,
  })
  -- }}}

  -- Motion {{{
  use({
    'kylechui/nvim-surround',
    config = function()
      require('nvim-surround').setup({})
    end,
  })

  use({
    'unblevable/quick-scope',
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
  })

  use({
    'phaazon/hop.nvim',
    config = function()
      require('hop').setup()
    end,
    cmd = { 'HopChar2', 'HopWord' },
  })
  -- }}}

  -- Misc {{{
  use({
    'NvChad/nvim-colorizer.lua',
    config = function()
      require 'sp.colorizer'
    end,
  })
  use({ 'tweekmonster/startuptime.vim', cmd = 'StartupTime' })
  use({
    'svermeulen/vim-subversive',
    config = function()
      require 'sp.subversive'
    end,
  })
  use 'tpope/vim-repeat'

  -- }}}

  -- Git {{{
  use({
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup()
    end,
  })
  use({
    'TimUntersberger/neogit',
    cmd = { 'Neogit' },
    config = function()
      require('neogit').setup({
        integrations = {
          diffview = true,
        },
      })
    end,
    requires = {
      {
        'sindrets/diffview.nvim',
        after = 'neogit',
        config = function()
          require('diffview').setup({})
        end,
      },
    },
  })
  use({
    'akinsho/git-conflict.nvim',
    config = function()
      require('git-conflict').setup()
    end,
  })
  -- }}}

  -- Enhance experience {{{
  use({
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      require 'sp.indents'
    end,
  })
  use({
    'akinsho/nvim-toggleterm.lua',
    config = function()
      require 'sp.toggle-term'
    end,
  })

  use({
    'numToStr/Comment.nvim',
    config = function()
      require 'sp.comments'
    end,
  })
  use({
    'windwp/nvim-autopairs',
    config = function()
      require 'sp.pairs'
    end,
  })
  use({
    'iamcco/markdown-preview.nvim',
    run = 'cd app && npm install',
    ft = { 'markdown' },
    setup = function()
      vim.g.mkdp_refresh_slow = 1
    end,
    requires = {
      'mzlogin/vim-markdown-toc',
      ft = { 'markdown' },
    },
  })
  use({
    'danymat/neogen',
    config = function()
      require('neogen').setup({})
    end,
    cmd = { 'Neogen' },
  })
  use({
    'mg979/vim-visual-multi',
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
  })
  use({
    'anuvyklack/pretty-fold.nvim',
    config = function()
      require 'sp.folds'
    end,
  })
  use({
    'monaqa/dial.nvim',
    config = function()
      local map = function(mode, lhs, rhs)
        require('sp.helper').map(mode, lhs, rhs, { noremap = false, silent = false })
      end
      local augend = require 'dial.augend'
      require('dial.config').augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.constant.alias.bool,
          augend.integer.alias.hex,
          augend.date.alias['%Y/%m/%d'],
        },
        typescript = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.constant.new({ elements = { 'let', 'const' } }),
        },
        visual = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.integer.alias.hex,
          augend.date.alias['%Y/%m/%d'],
          augend.constant.alias.alpha,
          augend.constant.alias.Alpha,
        },
      })
      map('n', '<C-a>', '<Plug>(dial-increment)')
      map('n', '<C-x>', '<Plug>(dial-decrement)')
      map('v', '<C-a>', '<Plug>(dial-increment)')
      map('v', '<C-x>', '<Plug>(dial-decrement)')
      map('v', 'g<C-a>', 'g<Plug>(dial-increment)')
      map('v', 'g<C-x>', 'g<Plug>(dial-decrement)')
    end,
  })
  -- }}}

  -- IDE bloat {{{
  use({
    'kyazdani42/nvim-tree.lua',
    cmd = { 'NvimTreeToggle', 'NvimTreeFindFileToggle' },
    config = function()
      require 'sp.nvim-tree'
    end,
  })
  use({
    'stevearc/dressing.nvim',
    config = function()
      require('dressing').setup({
        select = {
          backend = { 'fzf_lua', 'telescope', 'builtin' },
        },
      })
    end,
  })
  use({
    'Shatur/neovim-session-manager',
    config = function()
      require('session_manager').setup({
        -- Define what to do when Neovim is started without arguments.
        -- Possible values: Disabled, CurrentDir, LastSession
        autoload_mode = require('session_manager.config').AutoloadMode.CurrentDir,
      })
    end,
  })
  use({
    'folke/which-key.nvim',
    config = function()
      require 'sp.which-key'
    end,
  })
  use({
    'rcarriga/nvim-notify',
    event = 'BufRead',
    config = function()
      local ok, notify = pcall(require, 'notify')
      if not ok then
        return
      end
      notify.setup({
        background_colour = '#001222',
      })
      vim.notify = notify
    end,
  })
  -- }}}
end)
