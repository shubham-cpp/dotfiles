return {
  {
    url = "folke/flash.nvim",
    config = function()
      require("flash").setup({
        modes = {
          char = {
            enabled = false,
            autohide = false,
          },
        },
      })
    end,
    keys = {
      {
        "S",
        function()
          require("flash").treesitter()
        end,
        desc = "Flash Treesitter",
        mode = { "n", "o" },
      },
    },
  },

  {
    url = "cbochs/grapple.nvim",
    config = function()
      require("grapple").setup({
        scope = "git_branch",
        icons = true,
        status = false,
      })
    end,
    keys = {
      { "<leader>A", "<cmd>Grapple toggle<cr>", desc = "Grapple toggle" },
      { "<leader>`", "<cmd>Grapple toggle<cr>", desc = "Grapple toggle" },
      { "<C-e>", "<cmd>Grapple toggle_tags<cr>", desc = "Grapple toggle tags" },
      { "<C-s-n>", "<cmd>Grapple cycle_tags next<cr>", desc = "Grapple cycle next tag" },
      { "<C-s-p>", "<cmd>Grapple cycle_tags prev<cr>", desc = "Grapple cycle prev tag" },
      { "<localleader>1", "<cmd>Grapple select index=1<cr>", desc = "Grapple select 1" },
      { "<localleader>2", "<cmd>Grapple select index=2<cr>", desc = "Grapple select 2" },
      { "<localleader>3", "<cmd>Grapple select index=3<cr>", desc = "Grapple select 3" },
      { "<localleader>4", "<cmd>Grapple select index=4<cr>", desc = "Grapple select 4" },
      { "<localleader>5", "<cmd>Grapple select index=5<cr>", desc = "Grapple select 5" },
      { "<localleader>6", "<cmd>Grapple select index=6<cr>", desc = "Grapple select 6" },
      { "<localleader>7", "<cmd>Grapple select index=7<cr>", desc = "Grapple select 7" },
      { "<localleader>8", "<cmd>Grapple select index=8<cr>", desc = "Grapple select 8" },
      { "<localleader>9", "<cmd>Grapple select index=9<cr>", desc = "Grapple select 9" },
    },
  },

  {
    url = "folke/persistence.nvim",
    config = function()
      require("persistence").setup({
        dir = vim.fn.stdpath("data") .. "/sessions/",
        branch = true,
      })
    end,
    keys = {
      {
        "<leader>ql",
        function()
          require("persistence").load()
        end,
        desc = "Persistence load",
      },
      {
        "<leader>qs",
        function()
          require("persistence").select()
        end,
        desc = "Persistence select",
      },
      {
        "<leader>qL",
        function()
          require("persistence").load({ last = true })
        end,
        desc = "Persistence load last",
      },
      {
        "<leader>qd",
        function()
          require("persistence").stop()
        end,
        desc = "Persistence stop",
      },
    },
  },

  {
    url = "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup({})
    end,
    keys = {
      "ys",
      "ds",
      "cs",
      { "S", desc = "Surround (visual)", mode = "x" },
    },
  },

  {
    url = "unblevable/quick-scope",
    config = function()
      vim.g.qs_highlight_on_keys = { "f", "F", "t", "T" }
      vim.g.qs_buftype_blacklist = { "terminal", "nofile", "dashboard", "startify" }
      vim.g.qs_lazy_highlight = 1
    end,
  },
}
