local u = require "config.utils"

vim.pack.add({
  { src = u.gh "nvim-treesitter/nvim-treesitter", version = "main" },
  { src = u.gh "nvim-treesitter/nvim-treesitter-textobjects", version = "main" },
  u.gh "windwp/nvim-ts-autotag",
})

local group = vim.api.nvim_create_augroup("ConfigTreesitter", { clear = true })
local langs = {
  "bash",
  "c",
  "diff",
  html = { "html", "html_tags" },
  javascript = { "javascript", "jsdoc" },
  "json",
  "json5",
  "go",
  "gomod",
  "gowork",
  "gosum",
  lua = { "lua", "luadoc", "luap" },
  markdown = { "markdown", "markdown_inline", "printf", "query", "regex" },
  "python",
  "toml",
  "rust",
  "ron",
  typescript = { "typescript", "tsx" },
  vim = { "vim", "vimdoc" },
  "vue",
  "svelte",
  "astro",
  "css",
  "scss",
  "xml",
  "yaml",
  "sql",
  "dockerfile",
  "git_config",
  "gitcommit",
  "git_rebase",
  "gitignore",
  "gitattributes",
}

local hooks = function(ev)
  local name, kind = ev.data.spec.name, ev.data.kind
  local is_install_or_update = kind == "install" or kind == "update"

  if name == "nvim-treesitter" and is_install_or_update then
    -- Append `:wait()` if you need synchronous execution
    vim.cmd "TSUpdate"

    local parsers = vim.iter(vim.tbl_values(langs)):flatten(1):totable()

    require("nvim-treesitter").install(parsers)
  end
end

vim.api.nvim_create_autocmd("PackChanged", { callback = hooks })
vim.api.nvim_create_autocmd("FileType", {
  group = group,
  pattern = u.get_keys(langs),
  desc = "Enable treesitter",
  callback = function(args)
    local buf = args.buf

    if vim.b.is_bigfile == true then
      vim.treesitter.stop(buf)
      return
    end

    vim.treesitter.start(buf)
  end,
})

require("nvim-treesitter").setup({})
require("nvim-ts-autotag").setup({})
require("nvim-treesitter-textobjects").setup({
  select = { lookahead = true },
})

vim.keymap.set("n", "<LocalLeader>a", function()
  require("nvim-treesitter-textobjects.swap").swap_next "@parameter.inner"
end, { desc = "Swap next argument" })
vim.keymap.set("n", "<LocalLeader>A", function()
  require("nvim-treesitter-textobjects.swap").swap_previous "@parameter.outer"
end, { desc = "Swap prev argument" })

vim.keymap.set("n", "<LocalLeader>k", function()
  require("nvim-treesitter-textobjects.swap").swap_next "@block.outer"
end, { desc = "Swap next block" })
vim.keymap.set("n", "<LocalLeader>K", function()
  require("nvim-treesitter-textobjects.swap").swap_previous "@block.outer"
end, { desc = "Swap prev block" })

vim.keymap.set("n", "<LocalLeader>f", function()
  require("nvim-treesitter-textobjects.swap").swap_next "@function.outer"
end, { desc = "Swap next function" })
vim.keymap.set("n", "<LocalLeader>f", function()
  require("nvim-treesitter-textobjects.swap").swap_previous "@function.outer"
end, { desc = "Swap prev function" })
