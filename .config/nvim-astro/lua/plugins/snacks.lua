---@param picker snacks.Picker
local function copy_path_full(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then return end
  vim.schedule(function()
    local full_path = vim.fn.fnamemodify(selected.file, ":p")
    vim.fn.setreg("+", full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = "File Path Copied" })
  end)
end
---FIXME: not working currently
---@param picker snacks.Picker
local function copy_path_relative(picker)
  local selected = picker:selected({ fallback = true })[1]
  if not selected or selected == nil then return end
  vim.schedule(function()
    local full_path = vim.fs.normalize(selected.file)
    vim.fn.setreg("+", full_path)
    vim.notify(full_path, vim.log.levels.INFO, { title = "File Path Copied" })
  end)
end

---@type LazySpec
return {
  {
    "snacks.nvim",
    ---@type snacks.Config
    opts = {
      picker = {
        enabled = true,
        layout = { preset = "dropdown" },
        matcher = { frecency = true, history_bonus = true },
        ---@class snacks.picker.formatters.Config
        formatters = { file = { filename_first = true } },
        sources = {
          explorer = {
            layout = { layout = { position = "right" }, cycle = false },
            actions = {
              copy_path_full = copy_path_full,
              copy_path_relative = copy_path_relative,
            },
            win = {
              list = {
                keys = {
                  ["/"] = false,
                  ["f"] = { "toggle_focus", mode = { "n" } },
                  ["Y"] = { "copy_path_full", mode = { "n" } },
                  ["gy"] = { "copy_path_relative", mode = { "n" } },
                },
              },
            },
          },
        },
        win = {
          -- input window
          input = {
            keys = {
              ["<c-x>"] = { "edit_split", mode = { "i", "n" } },
              ["<c-t>"] = { "edit_tab", mode = { "i", "n" } },
              ["<c-c>"] = { "copy", mode = { "i", "n" } },
            },
          },
          list = { keys = { ["<c-x>"] = "edit_split" } },
        },
      },
      lazygit = {},
    },
    keys = {
      {
        "<C-g>",
        function() Snacks.lazygit() end,
        desc = "Toggle Lazygit",
      },
      {
        "<C-w>m",
        function() Snacks.zen.zoom() end,
        desc = "Toggle Zoom",
      },
      {
        "<C-p>",
        function() Snacks.picker.files { layout = { preset = "vscode" } } end,
        desc = "Find Files",
      },
      {
        "<leader>ff",
        function() Snacks.picker.files { layout = { preset = "vscode" } } end,
        desc = "Find Files",
      },
      {
        "<leader>fF",
        function() Snacks.picker.files { layout = { preset = "vscode" }, hidden = true, ignored = true } end,
        desc = "Find all files",
      },
      {
        "<leader>fn",
        function() Snacks.picker.files { cwd = vim.fn.stdpath "config", layout = { preset = "vscode" } } end,
        desc = "Find Config File",
      },
      {
        "<leader>fN",
        function() Snacks.picker.files { cwd = vim.fn.stdpath "data" .. "/lazy", layout = { preset = "vscode" } } end,
        desc = "Neovim Data dir",
      },
      {
        "<leader>fd",
        function()
          Snacks.picker.files { cwd = vim.fn.expand "~/Documents/dotfiles/.config", layout = { preset = "vscode" } }
        end,
        desc = "Find Dotfiles",
      },
      {
        "<leader>fg",
        function() Snacks.picker.git_files { layout = { preset = "vscode" } } end,
        desc = "Find Git Files",
      },
      {
        "<leader>fs",
        function() Snacks.picker.grep() end,
        desc = "Search/Grep",
      },
      {
        "<leader>fS",
        function() Snacks.picker.grep_buffers() end,
        desc = "Search/Grep in Open Buffers",
      },
      {
        "<leader>fo",
        function() Snacks.picker.smart() end,
        desc = "Find buffers/recent/files",
      },
      {
        "<leader>fr",
        function() Snacks.picker.resume() end,
        desc = "Resume",
      },
      {
        "<leader>fO",
        function() Snacks.picker.recent { layout = { preset = "vertical" } } end,
        desc = "Old Files",
      },
      {
        "<leader>f,",
        function() Snacks.picker.recent { layout = { preset = "vertical" }, filter = { cwd = true } } end,
        desc = "Old Files(cwd)",
      },
      {
        "<leader>fz",
        function() Snacks.picker.zoxide { layout = { preset = "vertical" } } end,
        desc = "Zoxided",
      },
      {
        "<leader>fw",
        function() Snacks.picker.grep_word() end,
        desc = "Visual selection or word",
        mode = { "n", "x" },
      },
      {
        "<leader>fW",
        function() Snacks.picker.grep_word { hidden = true, ignored = true } end,
        desc = "Visual selection or word",
        mode = { "n", "x" },
      },
      {
        "<leader>-",
        function() Snacks.explorer() end,
        desc = "Explorer",
      },
    },
  },
  {
    "AstroNvim/astrolsp",
    ---@param opts AstroLSPOpts
    opts = function(_, opts)
      if opts.mappings.n.gd then
        opts.mappings.n.gd[1] = function()
          require("snacks").picker.lsp_definitions { layout = { preset = "ivy_split" } }
        end
      end
      if opts.mappings.n.gI then
        opts.mappings.n.gI[1] = function()
          require("snacks").picker.lsp_implementations { layout = { preset = "ivy_split" } }
        end
      end
      if opts.mappings.n.gy then
        opts.mappings.n.gy[1] = function()
          require("snacks").picker.lsp_type_definitions { layout = { preset = "ivy_split" } }
        end
      end
      if opts.mappings.n["<Leader>lG"] then
        opts.mappings.n["<Leader>lG"][1] = require("snacks").picker.lsp_workspace_symbols
      end
      if opts.mappings.n["<Leader>lR"] then
        opts.mappings.n["<Leader>lR"][1] = function()
          require("snacks").picker.lsp_references { layout = { preset = "ivy_split" } }
        end
      end
      opts.mappings.n["grr"] = {
        function() require("snacks").picker.lsp_references { layout = { preset = "ivy_split" } } end,
        desc = "LSP References",
      }
      -- opts.mappings.n["gra"][1] = require("snacks").picker.lsp_references
    end,
  },
}
