local function vscode_layout(title)
  return {
    height = 0.55,
    width = 0.6,
    row = 0,
    title = " " .. title .. " ",
    title_pos = "center",
  }
end

local function docs_layout(title)
  return {
    height = 0.70,
    width = 0.65,
    row = 0.5,
    col = 0.5,
    title = " " .. title .. " ",
    title_pos = "center",
    preview = {
      layout = "vertical",
      vertical = "up:50%",
      scrollbar = "border",
      winopts = {
        number = false,
        relativenumber = false,
        signcolumn = "no",
      },
    },
  }
end

local function default_preview_layout()
  return {
    height = 0.70,
    width = 0.65,
    row = 0.5,
    col = 0.5,
    preview = {
      layout = "vertical",
      vertical = "down:38%",
      scrollbar = "border",
      winopts = {
        number = false,
        relativenumber = false,
        signcolumn = "no",
      },
    },
  }
end

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
  cmd = { "FzfLua", "PackManage", "PackManageNonActive" },
  keys = {
    { "<C-p>", "<cmd>FzfLua files<cr>", desc = "Find files" },
    { "<leader>ff", "<cmd>FzfLua files<cr>", desc = "Find files" },
    { "<leader>fg", "<cmd>FzfLua git_files<cr>", desc = "Find git files" },
    { "<leader>fo", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Document symbols" },
    { "<leader>fO", "<cmd>FzfLua lsp_live_workspace_symbols<cr>", desc = "Workspace symbols" },
    { "<leader>fb", "<cmd>FzfLua buffers<cr>", desc = "Find buffers" },
    { "<leader>fr", "<cmd>FzfLua resume<cr>", desc = "Resume picker" },
    { "<leader>fs", "<cmd>FzfLua live_grep<cr>", desc = "Grep" },
    {
      "<leader>fS",
      function()
        require("fzf-lua").live_grep({ cwd = vim.fn.expand("%:p:h") })
      end,
      desc = "Grep current dir",
    },
    { "<leader>fw", "<cmd>FzfLua grep_cword<cr>", desc = "Find word under cursor" },
    {
      "<leader>fW",
      function()
        require("fzf-lua").grep_cword({ cwd = vim.fn.expand("%:p:h") })
      end,
      desc = "Find word current dir",
    },
    {
      "<leader>fz",
      function()
        local fzf = require("fzf-lua")
        fzf.zoxide({
          scope = "tab",
          formatter = { "path.dirname_first", 2 },
          winopts = vscode_layout("Zoxider"),
          actions = {
            ["ctrl-t"] = function(sel, opts)
              return fzf.actions.zoxide_cd(sel, vim.tbl_extend("force", opts, { scope = "global" }))
            end,
          },
        })
      end,
      desc = "Zoxide",
    },
    {
      "<leader>fn",
      function()
        require("fzf-lua").files({ cwd = vim.fn.stdpath("config"), winopts = vscode_layout("Neovim") })
      end,
      desc = "Neovim files",
    },
    {
      "<leader>fd",
      function()
        require("fzf-lua").files({ cwd = dotfiles_cwd(), winopts = vscode_layout("Dotfiles") })
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
    {
      "<leader>fG",
      function()
        require("fzf-lua").live_grep({
          cmd = "git grep -i --line-number --column --color=always",
          fn_transform_cmd = function(query, cmd, _)
            local search_query, glob_str = query:match("(.-)%s-%-%-(.*)")
            if not glob_str then
              return
            end
            return string.format("%s %s %s", cmd, vim.fn.shellescape(search_query), glob_str), search_query
          end,
        })
      end,
      desc = "Git grep",
    },
    {
      "<leader>fS",
      function()
        require("fzf-lua").grep_visual({ cwd = vim.fn.expand("%:p:h") })
      end,
      mode = "v",
      desc = "Grep selection current dir",
    },
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
      { "border-fused", "skim", "hide" },
      ui_select = true,
      defaults = { formatter = { "path.filename_first", 2 }, fzf_args = { "--ellipsis= " } },
      fzf_opts = { ["--algo"] = "fzy" },
      winopts = default_preview_layout(),
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
        winopts = vscode_layout("Files"),
        actions = {
          ["ctrl-x"] = require("fzf-lua").actions.file_split,
          ["ctrl-t"] = require("fzf-lua").actions.file_tabedit,
        },
      },
      git = {
        files = {
          previewer = false,
          winopts = vscode_layout("Git Files"),
          cmd = "git ls-files --cached --others --exclude-standard",
        },
        branches = { cmd_add = { "git", "switch", "-c" } },
      },
      grep = { rg_glob = true, rg_glob_fn = rg_glob_fn },
      buffers = { prompt = "Buffers> ", winopts = docs_layout("Buffers") },
      helptags = { prompt = "Help> ", winopts = docs_layout("Help") },
      manpages = { prompt = "Man> ", winopts = docs_layout("Man") },
      keymaps = { prompt = "Keymaps> ", winopts = docs_layout("Keymaps") },
      autocmds = { prompt = "Autocmds> ", winopts = docs_layout("Autocmds") },
    })
  end,
}
