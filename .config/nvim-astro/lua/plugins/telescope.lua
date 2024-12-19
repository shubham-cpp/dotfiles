local function filename_first(_, path)
  local tail = vim.fs.basename(path)
  local parent = vim.fs.dirname(path)
  if parent == "." then return tail end
  return string.format("%s\t\t%s", tail, parent)
end

---@type LazySpec
return {
  {
    "nvim-telescope/telescope.nvim",
    opts = {
      extensions = {
        egrepify = {
          prefixes = {
            --- trying to use flag `--type`
            ["@"] = {
              flag = "type",
              cb = function(input)
                -- check if the input has any commas
                if string.find(input, ",") then return string.format([[{%s}]], input) end
                return string.format([[%s]], input)
              end,
            },
            ["<"] = {
              flag = "type-not",
              cb = function(input)
                -- check if the input has any commas
                if string.find(input, ",") then return string.format([[{%s}]], input) end
                return string.format([[%s]], input)
              end,
            },
          },
        },
      },
      pickers = {
        find_files = { path_display = filename_first, previewer = false },
        buffers = {
          ignore_current_buffer = true,
          sort_lastused = true,
          mappings = {
            i = { ["<c-d>"] = "delete_buffer" },
            n = { d = "delete_buffer" },
          },
        },
      },
      defaults = {
        vimgrep_arguments = {
          "rg",
          "--color=never",
          "--no-heading",
          "--with-filename",
          "--line-number",
          "--column",
          "--smart-case",
          "--hidden",
          "--trim", -- add this value
          "--glob=!.git/",
          "--glob=!node_modules/",
          "--glob=!.venv/",
          "--glob=!venv/",
        },
      },
    },
    keys = {
      { "<c-p>", function() require("telescope.builtin").find_files() end, desc = "Find files" },
      {
        "<leader>fn",
        function() require("telescope.builtin").find_files { cwd = vim.fn.stdpath "config" } end,
        desc = "Neovim Config",
      },
      {
        "<leader>fd",
        function()
          require("telescope.builtin").find_files { cwd = vim.fn.expand "~/Documents/dotfiles", hidden = true }
        end,
        desc = "Dotfiles",
      },
      {
        "<leader>fl",
        function() require("telescope.builtin").find_files { cwd = vim.fn.stdpath "data" .. "/lazy" } end,
        desc = "All Plugins",
      },
      { "<leader>fN", "<cmd>Telescope notify<CR>", desc = "Notification" },
    },
  },
  {
    "fdschmidt93/telescope-egrepify.nvim",
    lazy = true,
    dependencies = "nvim-lua/plenary.nvim",
    specs = {
      {
        "nvim-telescope/telescope.nvim",
        dependencies = {
          { "fdschmidt93/telescope-egrepify.nvim", dependencies = "nvim-lua/plenary.nvim" },
          {
            "AstroNvim/astrocore",
            opts = {
              mappings = {
                n = {
                  ["<Leader>fs"] = { "<Cmd>Telescope egrepify<CR>", desc = "Search" },
                },
              },
            },
          },
        },
        opts = function() require("telescope").load_extension "egrepify" end,
      },
    },
  },
  -- {
  --   "natecraddock/telescope-zf-native.nvim",
  --   lazy = true,
  --   specs = {
  --     {
  --       "nvim-telescope/telescope.nvim",
  --       dependencies = {
  --         "natecraddock/telescope-zf-native.nvim",
  --         { "nvim-telescope/telescope-fzf-native.nvim", build = "make", enabled = false },
  --       },
  --       opts = function() require("telescope").load_extension "zf-native" end,
  --     },
  --   },
  -- },
}
