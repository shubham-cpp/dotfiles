local u = require("config.utils")

local group = vim.api.nvim_create_augroup("ConfigTreesitter", { clear = true })
local langs = {
  "bash",
  "c",
  "diff",
  "fish",
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

local function parser_names()
  return vim.iter(vim.tbl_values(langs)):flatten(1):totable()
end

local function ensure_treesitter_cli_on_path()
  if vim.fn.executable("tree-sitter") == 1 then
    return true
  end

  local mason_bin = vim.fs.joinpath(vim.fn.stdpath("data"), "mason", "bin")
  if vim.uv.fs_stat(mason_bin) then
    vim.env.PATH = mason_bin .. ":" .. vim.env.PATH
  end

  return vim.fn.executable("tree-sitter") == 1
end

local function start_treesitter(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return
  end

  if vim.b[buf].is_bigfile == true then
    pcall(vim.treesitter.stop, buf)
    return
  end

  local ft = vim.bo[buf].filetype
  if ft == "" then
    return
  end

  local lang = vim.treesitter.language.get_lang(ft) or ft
  if not vim.treesitter.language.add(lang) then
    return
  end

  vim.treesitter.start(buf, lang)
end

local function start_on_open_buffers()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].buftype == "" then
      start_treesitter(buf)
    end
  end
end

local function ensure_parsers()
  if not ensure_treesitter_cli_on_path() then
    vim.notify("tree-sitter CLI not found; parsers were not installed", vim.log.levels.WARN)
    return
  end

  require("nvim-treesitter").install(parser_names()):await(function(err)
    if err then
      vim.notify("nvim-treesitter install failed: " .. tostring(err), vim.log.levels.ERROR)
      return
    end
    vim.schedule(start_on_open_buffers)
  end)
end

vim.api.nvim_create_autocmd("PackChanged", {
  group = group,
  desc = "Update treesitter parsers after nvim-treesitter changes",
  callback = function(ev)
    local spec = ev.data.spec
    if spec and spec.name == "nvim-treesitter" and ev.data.kind == "update" then
      vim.schedule(function()
        require("nvim-treesitter").update()
      end)
    end
  end,
})

vim.pack.add({
  { src = u.gh("nvim-treesitter/nvim-treesitter"), version = "main" },
  { src = u.gh("nvim-treesitter/nvim-treesitter-textobjects"), version = "main" },
  -- u.gh "windwp/nvim-ts-autotag",
  u.gh("tronikelis/ts-autotag.nvim"),
})

vim.api.nvim_create_autocmd("FileType", {
  group = group,
  desc = "Enable treesitter",
  callback = function(args)
    start_treesitter(args.buf)
  end,
})

require("nvim-treesitter").setup({})
ensure_parsers()
require("ts-autotag").setup({})
-- require("nvim-ts-autotag").setup({})
require("nvim-treesitter-textobjects").setup({
  select = { lookahead = true },
})

vim.keymap.set("n", "<LocalLeader>a", function()
  require("nvim-treesitter-textobjects.swap").swap_next("@parameter.inner")
end, { desc = "Swap next argument" })
vim.keymap.set("n", "<LocalLeader>A", function()
  require("nvim-treesitter-textobjects.swap").swap_previous("@parameter.outer")
end, { desc = "Swap prev argument" })

vim.keymap.set("n", "<LocalLeader>k", function()
  require("nvim-treesitter-textobjects.swap").swap_next("@block.outer")
end, { desc = "Swap next block" })
vim.keymap.set("n", "<LocalLeader>K", function()
  require("nvim-treesitter-textobjects.swap").swap_previous("@block.outer")
end, { desc = "Swap prev block" })

vim.keymap.set("n", "<LocalLeader>f", function()
  require("nvim-treesitter-textobjects.swap").swap_next("@function.outer")
end, { desc = "Swap next function" })
vim.keymap.set("n", "<LocalLeader>F", function()
  require("nvim-treesitter-textobjects.swap").swap_previous("@function.outer")
end, { desc = "Swap prev function" })
