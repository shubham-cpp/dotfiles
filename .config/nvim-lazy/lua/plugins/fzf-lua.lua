local vscode = {
  height = 0.55,
  width = 0.6,
  row = 0,
}
-- local actions = require("fzf-lua.actions")
-- local action_keys = {
--   ["ctrl-q"] = {
--     fn = actions.file_edit_or_qf,
--     prefix = "select-all+",
--   },
-- }

---@type LazySpec
return {
  "ibhagwan/fzf-lua",
  opts = {
    -- { "border-fused", "hide" },
    { "border-fused", "skim" },
    fzf_opts = { ["--algo"] = "frizbee" },
    defaults = {
      formatter = "path.filename_first",
      -- formatter = {"path.filename_first",2},
    },
    keymap = {
      builtin = {
        true,
        ["<C-d>"] = "preview-page-down",
        ["<C-u>"] = "preview-page-up",
      },
      fzf = {
        true,
        ["ctrl-d"] = "preview-page-down",
        ["ctrl-u"] = "preview-page-up",
        ["ctrl-q"] = "select-all+accept",
      },
    },
    winopts = { preview = { layout = "vertical" } },
    files = {
      -- actions = action_keys,
      previewer = false,
      winopts = vscode,
    },
    git = {
      files = {
        cmd = "git ls-files --cached --others --exclude-standard",
        -- actions = action_keys,
        previewer = false,
        winopts = vscode,
      },
      branches = {
        cmd_add = { "git", "switch", "-c" },
      },
    },
    grep = {
      -- actions = action_keys,
      fzf_opts = { ["--scheme"] = "path" },
      rg_glob = true,
      ---@param query string - first returned string is the new search query
      ---@param opts table - second returned string are (optional) additional rg flags
      ---@return string, string?
      rg_glob_fn = function(query)
        local regex, flags = query:match("^(.-)%s%-%-(.*)$")
        -- If no separator is detected will return the original query
        return (regex or query), flags
      end,
    },
  },
  keys = {
    { "gr",  false },
    { "grr", "<Cmd>FzfLua lsp_references jump1=true ignore_current_line=true includeDeclaration=false<CR>" },
    {
      "<leader>fu",
      "<Cmd>FzfLua undotree<CR>",
      desc = "Undo tree",
    },
    {
      "<leader>fz",
      "<Cmd>FzfLua zoxide<CR>",
      desc = "Zoxide",
    },
    {
      "<leader>fn",
      LazyVim.pick("files", { cwd = vim.fn.stdpath("config") }),
      desc = "Neovim config",
    },
    {
      "<leader>fd",
      LazyVim.pick("files", { cwd = vim.fn.expand("~/Documents/dotfiles/") }),
      desc = "Dotfiles",
    },
    {
      "<c-p>",
      "<cmd>FzfLua files<cr>",
      desc = "Find files",
    },
    { "<leader>fq", "<Cmd>FzfLua quickfix<CR>",     desc = "Quickfix List" },
    { "<leader>fQ", "<Cmd>FzfLua loclist<CR>",      desc = "Location List" },
    { "<leader>fl", "<Cmd>FzfLua lines<CR>",        desc = "Buffer Lines" },
    { "<leader>fG", "<Cmd>FzfLua lgrep_curbuf<CR>", desc = "Grep Open Buffers" },
    { '<leader>f"', "<Cmd>FzfLua registers<CR>",    desc = "Registers" },
    { "<leader>fa", "<Cmd>FzfLua autocmds<CR>",     desc = "Autocmds" },
    { "<leader>fc", "<Cmd>FzfLua commands<CR>",     desc = "Commands" },
    -- { "<leader>ld", "<Cmd>FzfLua diagnostics_workspace<CR>", desc = "Diagnostics" },
    -- Diagnostics (document/buffer)
    -- { "<leader>lD", "<Cmd>FzfLua diagnostics_document<CR>", desc = "Buffer Diagnostics" },
    -- Help tags
    { "<leader>fh", "<Cmd>FzfLua helptags<CR>",     desc = "Help Pages" },
    { "<leader>fH", "<Cmd>FzfLua highlights<CR>",   desc = "Highlights" },
    { "<leader>fk", "<Cmd>FzfLua keymaps<CR>",      desc = "Keymaps" },
    { "<leader>fm", "<Cmd>FzfLua manpages<CR>",     desc = "Man Pages" },
    { "<leader>fM", "<Cmd>FzfLua marks<CR>",        desc = "Marks" },
    { "<leader>fr", "<Cmd>FzfLua resume<CR>",       desc = "Resume" },
    { "<leader>fo", "<Cmd>FzfLua oldfiles<CR>",     desc = "Recent Files" },
    { "<leader>fj", "<Cmd>FzfLua jumps<CR>",        desc = "Jumps" },

    {
      "<Leader>fG",
      function()
        require("fzf-lua").live_grep({
          cmd = "git grep -i --line-number --column --color=always",
          fn_transform_cmd = function(query, cmd, _)
            local search_query, glob_str = query:match("(.-)%s-%-%-(.*)")
            if not glob_str then
              return
            end
            local new_cmd = string.format("%s %s %s", cmd, vim.fn.shellescape(search_query), glob_str)
            return new_cmd, search_query
          end,
        })
      end,
      desc = "Git grep",
    },
  },
  specs = {
    {
      "folke/todo-comments.nvim",
      optional = true,
      keys = {
        { "<leader>st", false },
        { "<leader>sT", false },
        {
          "<leader>ft",
          "<cmd>TodoFzfLua<cr>",
          desc = "Todo",
        },
      },
    },
  },
}
