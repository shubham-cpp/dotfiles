return {
  -- Multi-cursor editing
  {
    url = "jake-stewart/multicursor.nvim",
    version = "1.0",
    config = function()
      local mc = require("multicursor-nvim")
      mc.setup()

      -- ESC: enable cursors / clear cursors / nohl+esc
      vim.keymap.set("n", "<Esc>", function()
        if not mc.cursorsEnabled() then
          mc.enableCursors()
        elseif mc.hasCursors() then
          mc.clearCursors()
        else
          vim.cmd("nohl")
          return "<esc>"
        end
      end, { expr = true })

      -- Hydra for multicursor motions
      local Hydra = require("hydra")
      Hydra({
        name = "MultiCursors",
        mode = { "n", "x" },
        body = "<leader>m",
        heads = {
          {
            "n",
            function()
              mc.matchAddCursor(1)
            end,
            { desc = "next" },
          },
          {
            "p",
            function()
              mc.matchAddCursor(-1)
            end,
            { desc = "prev" },
          },
          {
            "N",
            function()
              mc.matchSkipCursor(1)
            end,
            { desc = "next(skip)" },
          },
          {
            "P",
            function()
              mc.matchSkipCursor(-1)
            end,
            { desc = "prev(skip)" },
          },
          {
            "j",
            function()
              mc.lineAddCursor(1)
            end,
            { desc = "down" },
          },
          {
            "k",
            function()
              mc.lineAddCursor(-1)
            end,
            { desc = "up" },
          },
          {
            "J",
            function()
              mc.lineSkipCursor(1)
            end,
            { desc = "down(skip)" },
          },
          {
            "K",
            function()
              mc.lineSkipCursor(-1)
            end,
            { desc = "up(skip)" },
          },
          {
            "t",
            function()
              mc.toggleCursor()
            end,
            { desc = "toggle" },
          },
          {
            "h",
            function()
              mc.prevCursor()
            end,
            { desc = "prev(goto)" },
          },
          {
            "l",
            function()
              mc.nextCursor()
            end,
            { desc = "next(goto)" },
          },
          {
            "a",
            function()
              mc.matchAllAddCursors()
            end,
            { desc = "all", exit = true },
          },
          { "<Esc>", nil, { exit = true, desc = false } },
        },
      })
    end,
  },

  -- Hydra (dependency for multicursor heads)
  {
    url = "nvimtools/hydra.nvim",
  },

  -- Autopairs
  {
    url = "windwp/nvim-autopairs",
    config = function()
      require("nvim-autopairs").setup({
        enable_check_bracket_line = true,
        disable_in_visualblock = true,
        check_ts = true,
        fast_wrap = {},
      })
    end,
  },

  -- Auto template strings (backtick strings in JS/TS)
  {
    url = "axelvc/template-string.nvim",
    config = function()
      require("template-string").setup({})
    end,
  },

  -- Annotation generator
  {
    url = "danymat/neogen",
    config = function()
      require("neogen").setup({
        languages = {
          lua = { template = { annotation_convention = "emmylua" } },
          typescript = { template = { annotation_convention = "tsdoc" } },
          typescriptreact = { template = { annotation_convention = "tsdoc" } },
        },
      })
    end,
    keys = {
      {
        "<leader>n<CR>",
        function()
          require("neogen").generate({})
        end,
        desc = "Generate annotation (any)",
      },
      {
        "<leader>nc",
        function()
          require("neogen").generate({ type = "class" })
        end,
        desc = "Generate class annotation",
      },
      {
        "<leader>nf",
        function()
          require("neogen").generate({ type = "func" })
        end,
        desc = "Generate func annotation",
      },
      {
        "<leader>nt",
        function()
          require("neogen").generate({ type = "type" })
        end,
        desc = "Generate type annotation",
      },
      {
        "<leader>nF",
        function()
          require("neogen").generate({ type = "file" })
        end,
        desc = "Generate file annotation",
      },
    },
  },

  -- Extended increment/decrement (dates, booleans, semver, etc.)
  {
    url = "monaqa/dial.nvim",
    config = function()
      local augend = require("dial.augend")
      require("dial.config").augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias["%Y/%m/%d"],
          augend.date.alias["%m/%d/%Y"],
          augend.date.alias["%Y-%m-%d"],
          augend.constant.alias.bool,
          augend.semver.alias.semver,
        },
      })
    end,
    keys = {
      {
        "<C-a>",
        function()
          require("dial.map").manipulate("increment", "normal")
        end,
        desc = "Dial increment",
      },
      {
        "<C-x>",
        function()
          require("dial.map").manipulate("decrement", "normal")
        end,
        desc = "Dial decrement",
      },
      {
        "g<C-a>",
        function()
          require("dial.map").manipulate("increment", "gnormal")
        end,
        desc = "Dial increment (mult)",
      },
      {
        "g<C-x>",
        function()
          require("dial.map").manipulate("decrement", "gnormal")
        end,
        desc = "Dial decrement (mult)",
      },
      {
        "<C-a>",
        function()
          require("dial.map").manipulate("increment", "visual")
        end,
        mode = "v",
        desc = "Dial increment",
      },
      {
        "<C-x>",
        function()
          require("dial.map").manipulate("decrement", "visual")
        end,
        mode = "v",
        desc = "Dial decrement",
      },
      {
        "g<C-a>",
        function()
          require("dial.map").manipulate("increment", "gvisual")
        end,
        mode = "v",
        desc = "Dial increment (mult)",
      },
      {
        "g<C-x>",
        function()
          require("dial.map").manipulate("decrement", "gvisual")
        end,
        mode = "v",
        desc = "Dial decrement (mult)",
      },
    },
  },

  -- Winbar code context (used by lualine)
  {
    url = "SmiteshP/nvim-navic",
    config = function()
      require("nvim-navic").setup({
        lsp = { auto_attach = true },
        highlight = true,
      })
    end,
  },

  -- Wiki / personal knowledge base
  {
    url = "echaya/neowiki.nvim",
    config = function()
      local nw = require("neowiki")
      nw.setup({
        wiki_dirs = {
          -- neowiki.nvim supports both absolute and tilde-expanded paths
          { name = "Work", path = "~/Documents/Personal-Vault/Notes/wiki" },
          { name = "Personal", path = "~/Documents/Personal-Vault/Notes/personal-wiki" },
        },
      })
    end,
    keys = {
      { "<leader>pp", "<cmd>lua require('neowiki').open_wiki()<cr>", desc = "Open Wiki" },
      { "<leader>pP", "<cmd>lua require('neowiki').open_wiki_floating()<cr>", desc = "Open Wiki in Floating Window" },
      { "<leader>pT", "<cmd>lua require('neowiki').open_wiki_new_tab()<cr>", desc = "Open Wiki in Tab" },
    },
  },
}
