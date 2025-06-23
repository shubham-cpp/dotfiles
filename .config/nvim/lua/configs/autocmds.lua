local function augroup(name)
  return vim.api.nvim_create_augroup("sp_" .. name, { clear = true })
end

vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
  group = augroup "checktime",
  desc = "Check if we need to reload the file when it changed",
  callback = function()
    if vim.o.buftype ~= "nofile" then
      vim.cmd "checktime"
    end
  end,
})

vim.api.nvim_create_autocmd("TextYankPost", {
  group = augroup "highlight_yank",
  desc = "Highlight on yank",
  callback = function()
    (vim.hl or vim.highlight).on_yank()
  end,
})

vim.api.nvim_create_autocmd({ "VimResized" }, {
  group = augroup "resize_splits",
  desc = "Resize splits if window got resized",
  callback = function()
    local current_tab = vim.fn.tabpagenr()
    vim.cmd "tabdo wincmd ="
    vim.cmd("tabnext " .. current_tab)
  end,
})

vim.api.nvim_create_autocmd("BufReadPost", {
  group = augroup "last_loc",
  desc = "go to last loc when opening a buffer",
  callback = function(event)
    local exclude = { "gitcommit" }
    local buf = event.buf
    if vim.tbl_contains(exclude, vim.bo[buf].filetype) or vim.b[buf].lazyvim_last_loc then
      return
    end
    vim.b[buf].lazyvim_last_loc = true
    local mark = vim.api.nvim_buf_get_mark(buf, '"')
    local lcount = vim.api.nvim_buf_line_count(buf)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = augroup "close_with_q",
  desc = "close some filetypes with <q>",
  pattern = {
    "PlenaryTestPopup",
    "checkhealth",
    "dbout",
    "gitsigns-blame",
    "grug-far",
    "help",
    "lspinfo",
    "neotest-output",
    "neotest-output-panel",
    "neotest-summary",
    "notify",
    "qf",
    "spectre_panel",
    "startuptime",
    "tsplayground",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.schedule(function()
      vim.keymap.set("n", "q", function()
        vim.cmd "close"
        pcall(vim.api.nvim_buf_delete, event.buf, { force = true })
      end, {
        buffer = event.buf,
        silent = true,
        desc = "Quit buffer",
      })
    end)
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = augroup "man_unlisted",
  desc = "make it easier to close man-files when opened inline",
  pattern = { "man" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = augroup "wrap_spell",
  desc = "wrap and check for spell in text filetypes",
  pattern = { "text", "plaintex", "typst", "gitcommit", "markdown" },
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
  end,
})

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = augroup "json_conceal",
  desc = "Fix conceallevel for json files",
  pattern = { "json", "jsonc", "json5" },
  callback = function()
    vim.opt_local.conceallevel = 0
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  group = augroup "buffer",
  desc = "Fix Comment Continuation",
  callback = function()
    vim.opt_local.formatoptions = "jcrqlnt"
  end,
})

vim.cmd.packadd "cfilter"

vim.filetype.add {
  extension = {
    fish = "fish",
    ocaml = "ocaml",
    rasi = "rasi",
    roc = "roc",
    pcss = "postcss",
    postcss = "postcss",
  },
  filename = {
    vimfrc = "vim",
    dwm_sxhkdrc = "sxhkdrc",
    [".env"] = "conf",
    [".env.*"] = "conf",
    ["package.json"] = "jsonc",
    ["docker-compose.yaml"] = "yaml.docker-compose",
  },
  pattern = {
    [".*profile"] = "sh",
    ["*.kbd"] = "lisp",
    [".eslintrc"] = "jsonc",
    ["tsconfig.*.json"] = "jsonc",
    [".*/hyprland%.conf"] = "hyprlang",
  },
}
