local vscode_layout = {
  height = 0.55,
  width = 0.6,
  row = 0,
}

local rg_glob_fn = function(query)
  local split_index = query:find(" --")
  if split_index then
    local search = query:sub(1, split_index - 1)
    local glob_str = query:sub(split_index + 3)
    return search, glob_str
  end
  return query
end

local function dotfiles_cwd()
  return vim.fn.expand("~/Documents/dotfiles")
end

return {
  "ibhagwan/fzf-lua",
  keys = {
    { "<C-p>", "<cmd>FzfLua files<cr>", desc = "Find files" },
    { "<leader>ff", "<cmd>FzfLua files<cr>", desc = "Find files" },
    { "<leader>fg", "<cmd>FzfLua git_files<cr>", desc = "Find git files" },
    { "<leader>fo", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Document symbols" },
    { "<leader>fO", "<cmd>FzfLua lsp_live_workspace_symbols<cr>", desc = "Workspace symbols" },
    { "<leader>fb", "<cmd>FzfLua buffers<cr>", desc = "Find buffers" },
    { "<leader>fr", "<cmd>FzfLua resume<cr>", desc = "Resume picker" },
    { "<leader>fs", "<cmd>FzfLua live_grep<cr>", desc = "Grep" },
    { "<leader>fw", "<cmd>FzfLua grep_cword<cr>", desc = "Find word under cursor" },
    {
      "<leader>fz",
      function()
        require("fzf-lua").zoxide({
          scope = "tab",
          formatter = { "path.dirname_first", 2 },
          winopts = { fullscreen = true, preview = { hidden = true } },
          actions = {
            ["ctrl-t"] = function(sel, opts)
              return FzfLua.actions.zoxide_cd(sel, vim.tbl_extend("force", opts, { scope = "global" }))
            end,
          },
        })
      end,
      desc = "Zoxide",
    },
    {
      "<leader>fn",
      function()
        require("fzf-lua").files({ cwd = vim.fn.stdpath("config") })
      end,
      desc = "Find nvim config files",
    },
    {
      "<leader>fd",
      function()
        require("fzf-lua").files({ cwd = dotfiles_cwd() })
      end,
      desc = "Find dotfiles",
    },
    { "<leader>fq", "<cmd>FzfLua quickfix<cr>", desc = "Quickfix list" },
    { "<leader>fQ", "<cmd>FzfLua loclist<cr>", desc = "Location list" },
    { "<leader>fl", "<cmd>FzfLua lines<cr>", desc = "Buffer lines" },
    { '<leader>f"', "<cmd>FzfLua registers<cr>", desc = "Registers" },
    { "<leader>fa", "<cmd>FzfLua autocmds<cr>", desc = "Autocmds" },
    { "<leader>fh", "<cmd>FzfLua helptags<cr>", desc = "Help tags" },
    { "<leader>fH", "<cmd>FzfLua highlights<cr>", desc = "Highlights" },
    { "<leader>fk", "<cmd>FzfLua keymaps<cr>", desc = "Keymaps" },
    { "<leader>fm", "<cmd>FzfLua manpages<cr>", desc = "Man pages" },
    { "<leader>fM", "<cmd>FzfLua marks<cr>", desc = "Marks" },
    { "<leader>fj", "<cmd>FzfLua jumps<cr>", desc = "Jumps" },
    { "<leader>ft", "<cmd>TodoFzfLua<cr>", desc = "Todo comments" },
    { "<leader>fG", "<cmd>FzfLua live_grep<cr>", desc = "Live grep" },
    { "<leader>gb", "<cmd>FzfLua git_branches<cr>", desc = "Git branches" },
    { "<leader>gc", "<cmd>FzfLua git_commits<cr>", desc = "Git commits (project)" },
    { "<leader>gC", "<cmd>FzfLua git_bcommits<cr>", desc = "Git commits (buffer)" },
    { "<leader>gd", "<cmd>FzfLua git_diff<cr>", desc = "Git diff" },
    { "<leader>gs", "<cmd>FzfLua git_status<cr>", desc = "Git status" },
    { "<leader>gS", "<cmd>FzfLua git_stash<cr>", desc = "Git stash" },
    { "<leader>gt", "<cmd>FzfLua git_tags<cr>", desc = "Git tags" },
    { "<leader>gw", "<cmd>FzfLua git_worktree<cr>", desc = "Git worktree" },
  },
  config = function()
    require("fzf-lua").setup({
      { "border-fused", "skim" },
      defaults = {
        formatter = { "path.filename_first", 2 },
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
      files = {
        previewer = false,
        winopts = vscode_layout,
        actions = {
          ["ctrl-x"] = require("fzf-lua").actions.file_split,
          ["ctrl-t"] = require("fzf-lua").actions.file_tabedit,
        },
      },
      git = {
        files = {
          previewer = false,
          winopts = vscode_layout,
          cmd = "git ls-files --cached --others --exclude-standard",
        },
        branches = {
          cmd_add = { "git", "switch", "-c" },
        },
      },
      grep = {
        rg_glob = true,
        rg_glob_fn = rg_glob_fn,
      },
    })
    require("core.package-fzf")
  end,
}
