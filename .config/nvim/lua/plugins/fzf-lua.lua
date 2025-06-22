return {
  "ibhagwan/fzf-lua",
  dependencies = { "echasnovski/mini.icons" },
  opts = function()
    local actions = require "fzf-lua.actions"

    require("fzf-lua").register_ui_select(function(_, items)
      local min_h, max_h = 0.15, 0.70
      local h = (#items + 4) / vim.o.lines
      if h < min_h then
        h = min_h
      elseif h > max_h then
        h = max_h
      end
      return { winopts = { height = h, width = 0.60, row = 0.40 } }
    end)

    return {
      { "telescope", "hide" },
      fzf_colors = true,
      fzf_opts = { ["--layout"] = "reverse" },
      winopts = {
        width = 0.6,
        preview = { layout = "vertical" },
      },
      defaults = {
        formatter = { "path.filename_first", 2 },
        -- formatter = "path.dirname_first",
      },
      keymap = {
        fzf = {
          true,
          ["alt-bspace"] = "unix-line-discard",
        },
      },
      files = {
        fzf_opts = { ["--history"] = vim.fn.stdpath "data" .. "/fzf-lua-files-history" },
        winopts = {
          row = 0.5,
          height = 0.5,
          width = 0.5,
          preview = { hidden = true },
        },
        actions = { ["ctrl-x"] = actions.file_split },
      },
      buffers = {
        actions = {
          ["ctrl-x"] = { fn = actions.file_split },
          ["alt-x"] = { fn = actions.buf_del, reload = true },
        },
        winopts = {
          row = 0.5,
          height = 0.5,
          width = 0.5,
          preview = { hidden = true },
        },
      },
      grep = {
        fzf_opts = { ["--history"] = vim.fn.stdpath "data" .. "/fzf-lua-grep-history" },
        rg_glob = true,
        -- first returned string is the new search query
        -- second returned string are (optional) additional rg flags
        -- @return string, string?
        rg_glob_fn = function(query, opts)
          local regex, flags = query:match "^(.-)%s%-%-(.*)$"
          -- If no separator is detected will return the original query
          return (regex or query), flags
        end,
      },
      git = {
        files = {
          cmd = "git ls-files --exclude-standard --cached --others",
          winopts = {
            row = 0.5,
            height = 0.5,
            width = 0.5,
            preview = { hidden = true },
          },
          actions = { ["ctrl-x"] = actions.file_split },
        },
      },
      lsp = {
        jump1 = true,
        child_prefix = false,
        code_actions = {
          previewer = vim.fn.executable "delta" == 1 and "codeaction_native" or nil,
        },
      },
    }
  end,
  keys = {
    { "<leader>fb", "<cmd>FzfLua buffers sort_mru=true<cr>", desc = "Buffer" },
    { "<leader>,", "<cmd>FzfLua buffers sort_mru=true<cr>", desc = "Switch Buffer" },
    { "<leader>fB", "<cmd>FzfLua blines<cr>", desc = "Buffer Lines" },
    {
      "<C-p>",
      function()
        if vim.g.gitsigns_head then
          vim.cmd.FzfLua "git_files"
        else
          vim.cmd.FzfLua "files"
        end
      end,
      desc = "Files",
    },
    {
      "<leader><leader>",
      function()
        if vim.g.gitsigns_head then
          vim.cmd.FzfLua "git_files"
        else
          vim.cmd.FzfLua "files"
        end
      end,
      desc = "Files",
    },
    { "<leader>ff", "<cmd>FzfLua files<cr>", desc = "Files" },
    { "<leader>fn", "<cmd>FzfLua files cwd=" .. vim.fn.stdpath "config" .. "<cr>", desc = "Find Config File" },
    {
      "<leader>fd",
      "<cmd>FzfLua files cwd=" .. vim.fn.expand "~/Documents/dotfiles" .. "<cr>",
      desc = "Find Config File",
    },
    { "<leader>fw", "<cmd>FzfLua grep_cword<cr>", desc = "Grep Cword" },
    { "<leader>fs", "<cmd>FzfLua grep_visual<cr>", desc = "Grep Visual", mode = { "x" } },
    {
      "<leader>fS",
      function()
        vim.cmd("FzfLua grep_visual cwd=" .. vim.fn.expand "%:p:h")
      end,
      desc = "Grep Visual(Cwd)",
      mode = { "x" },
    },
    { "<leader>fs", "<cmd>FzfLua live_grep<cr>", desc = "Grep (Root Dir)" },
    {
      "<leader>fS",
      function()
        vim.cmd("FzfLua live_grep cwd=" .. vim.fn.expand "%:p:h")
      end,
      desc = "Grep (Cwd Dir)",
    },
    { "<leader>/", "<cmd>FzfLua live_grep<cr>", desc = "Grep (Root Dir)" },
    { "<leader>:", "<cmd>FzfLua command_history<cr>", desc = "Command History" },
    { "<leader>fh", "<cmd>FzfLua helptags<cr>", desc = "Help Page" },
    { "<leader>fH", "<cmd>FzfLua highlights<cr>", desc = "Highlights" },
    { "<leader>fm", "<cmd>FzfLua man_pages<cr>", desc = "Man Pages" },
    { "<leader>fq", "<cmd>FzfLua quickfix<cr>", desc = "Quickfix List" },
    { "<leader>fr", "<cmd>FzfLua resume<cr>", desc = "Resume" },
    { '<leader>f"', "<cmd>FzfLua registers<cr>", desc = "Registers" },
    { "<leader>fR", "<cmd>FzfLua registers<cr>", desc = "Registers" },
    { "<leader>fz", "<cmd>FzfLua zoxider<cr>", desc = "Zoxide" },
    --- GIT
    { "<leader>gb", "<cmd>FzfLua git_branches<cr>", desc = "Git Branches" },
    { "<leader>gc", "<cmd>FzfLua git_commits<cr>", desc = "Git Commits" },
    { "<leader>gC", "<cmd>FzfLua git_bcommits<cr>", desc = "Git Commits(Buffer)" },
    { "<leader>gS", "<cmd>FzfLua git_stash<cr>", desc = "Git Stash" },
    { "<leader>gS", "<cmd>FzfLua git_stash<cr>", desc = "Git Stash" },
  },
}
