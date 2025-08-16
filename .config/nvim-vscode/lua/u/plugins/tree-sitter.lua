local add, later = MiniDeps.add, MiniDeps.later

later(function()
  add({
    source = "nvim-treesitter/nvim-treesitter",
    -- Use 'master' while monitoring updates in 'main'
    checkout = "master",
    monitor = "main",
    -- Perform action after every checkout
    hooks = {
      post_checkout = function()
        vim.cmd "TSUpdate"
      end,
    },
  })
  add({ source = "nvim-treesitter/nvim-treesitter-textobjects" })
  add({ source = "andymass/vim-matchup" })

  -- vim.opt.rtp:append(vim.fn.stdpath "data" .. "/site/pack/deps/opt/nvim-treesitter")

  ---@diagnostic disable-next-line: missing-fields
  require("nvim-treesitter.configs").setup({
    highlight = {
      enable = vim.g.vscode == nil,
      disable = function(_, buf)
        local max_filesize = 64 * 1024 -- 64 KB
        local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
        if ok and stats and stats.size > max_filesize then
          return true
        end
      end,
    },
    indent = { enable = vim.g.vscode == nil },
    ensure_installed = {
      "bash",
      "c",
      "diff",
      "html",
      "css",
      "scss",
      "javascript",
      "jsdoc",
      "json",
      "jsonc",
      "lua",
      "luadoc",
      "luap",
      "markdown",
      "markdown_inline",
      "printf",
      "python",
      "query",
      "regex",
      "toml",
      "tsx",
      "typescript",
      "go",
      "cpp",
      "vim",
      "vimdoc",
      "xml",
      "yaml",
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "<C-space>",
        node_incremental = "<C-space>",
        scope_incremental = false,
        node_decremental = "<bs>",
      },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true,
        keymaps = {
          ["ak"] = { query = "@block.outer", desc = "around block" },
          ["ik"] = { query = "@block.inner", desc = "inside block" },
          ["ac"] = { query = "@class.outer", desc = "around class" },
          ["ic"] = { query = "@class.inner", desc = "inside class" },
          ["a?"] = { query = "@conditional.outer", desc = "around conditional" },
          ["i?"] = { query = "@conditional.inner", desc = "inside conditional" },
          ["af"] = { query = "@function.outer", desc = "around function " },
          ["if"] = { query = "@function.inner", desc = "inside function " },
          ["ao"] = { query = "@loop.outer", desc = "around loop" },
          ["io"] = { query = "@loop.inner", desc = "inside loop" },
          ["aa"] = { query = "@parameter.outer", desc = "around argument" },
          ["ia"] = { query = "@parameter.inner", desc = "inside argument" },
        },
      },
      move = {
        enable = true,
        goto_next_start = { ["]f"] = "@function.outer", ["]k"] = "@class.outer", ["]a"] = "@parameter.inner" },
        goto_next_end = { ["]F"] = "@function.outer", ["]K"] = "@class.outer", ["]A"] = "@parameter.inner" },
        goto_previous_start = { ["[f"] = "@function.outer", ["[k"] = "@class.outer", ["[a"] = "@parameter.inner" },
        goto_previous_end = { ["[F"] = "@function.outer", ["[K"] = "@class.outer", ["[A"] = "@parameter.inner" },
      },
      swap = {
        enable = true,
        swap_next = {
          ["<localleader>k"] = { query = "@block.outer", desc = "Swap next block" },
          ["<localleader>f"] = { query = "@function.outer", desc = "Swap next function" },
          ["<localleader>a"] = { query = "@parameter.inner", desc = "Swap next argument" },
        },
        swap_previous = {
          ["<localleader>K"] = { query = "@block.outer", desc = "Swap previous block" },
          ["<localleader>F"] = { query = "@function.outer", desc = "Swap previous function" },
          ["<localleader>A"] = { query = "@parameter.inner", desc = "Swap previous argument" },
        },
      },
    },
  })
  vim.g.matchup_treesitter_stopline = 500
end)
