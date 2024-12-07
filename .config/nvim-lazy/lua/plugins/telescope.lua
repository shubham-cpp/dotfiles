local au_telescope = vim.api.nvim_create_augroup("au_telescope", { clear = true })
local dropdown = {
  layout_strategy = "vertical",
  layout_config = { width = 0.6, preview_cutoff = 1, prompt_position = "top" },
}
local function filename_first(_, path)
  local tail = vim.fs.basename(path)
  local parent = vim.fs.dirname(path)
  if parent == "." then
    return tail
  end
  return string.format("%s\t\t%s", tail, parent)
end
---@type LazySpec
return {
  "fdschmidt93/telescope-egrepify.nvim",
  dependencies = {
    {
      "nvim-telescope/telescope.nvim",
      opts = {
        defaults = {
          sorting_strategy = "ascending",
          layout_config = {
            horizontal = { prompt_position = "top", preview_width = 0.40 },
            vertical = { mirror = false },
            width = 0.87,
            height = 0.80,
            preview_cutoff = 120,
          },
          mappings = {
            i = {
              ["<C-j>"] = function(...)
                require("telescope.actions").move_selection_next(...)
              end,
              ["<C-k>"] = function(...)
                require("telescope.actions").move_selection_previous(...)
              end,
              ["<C-n>"] = function(...)
                require("telescope.actions").cycle_history_next(...)
              end,
              ["<C-p>"] = function(...)
                require("telescope.actions").cycle_history_prev(...)
              end,
              ["<C-t>"] = function(...)
                require("telescope.actions").select_tab(...)
              end,
              ["<a-t>"] = function(...)
                require("telescope.actions").select_tab(...)
              end,
              ["<a-cr>"] = function(...)
                require("telescope.actions").select_tab(...)
              end,
              ["<C-c>"] = function(...)
                require("telescope.actions").close(...)
              end,
              ["<A-p>"] = function(...)
                require("telescope.actions").toggle_preview(...)
              end,
            },
          },
        },
        extensions = {
          egrepify = {
            prefixes = {
              --- trying to use flag `--type`
              [">"] = {
                flag = "type",
                cb = function(input)
                  -- check if the input has any commas
                  if string.find(input, ",") then
                    return string.format([[{%s}]], input)
                  end
                  return string.format([[%s]], input)
                end,
              },
              ["<"] = {
                flag = "type-not",
                cb = function(input)
                  -- check if the input has any commas
                  if string.find(input, ",") then
                    return string.format([[{%s}]], input)
                  end
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
          lsp_references = dropdown,
          lsp_definitions = dropdown,
          git_branches = dropdown,
          git_commits = dropdown,
          git_bcommits = dropdown,
          lsp_document_symbols = dropdown,
          lsp_workspace_symbols = dropdown,
          grep_string = vim.tbl_extend("force", dropdown, {
            path_display = { "truncate" },
          }),
        },
      },
    },
  },
  config = function()
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "TelescopeResults",
      group = au_telescope,
      callback = function(ctx)
        vim.api.nvim_buf_call(ctx.buf, function()
          vim.fn.matchadd("TelescopeParent", "\t\t.*$")
          vim.api.nvim_set_hl(0, "TelescopeParent", { link = "Comment" })
        end)
      end,
    })
    LazyVim.on_load("telescope.nvim", function()
      require("telescope").load_extension("egrepify")
    end)
  end,
  keys = {
    {
      "<c-p>",
      function()
        LazyVim.pick.open("files", {
          find_command = {
            "rg",
            "--files",
            "--color",
            "never",
            "--ignore",
            "--hidden",
            "--sortr",
            "modified",
            "--glob",
            "!{" .. vim.iter(vim.opt.wildignore:get()):join(",") .. "}",
          },
        })
      end,
      desc = "Find Files (Root Dir)",
    },
    { "<leader>fn", LazyVim.pick.config_files(), desc = "Find Config File" },
    {
      "<leader>fd",
      LazyVim.pick("files", { cwd = vim.fn.expand("~/Documents/dotfiles/.config/") }),
      desc = "Find Dotfiles",
    },
    { "<leader>fN", LazyVim.pick("files", { cwd = vim.fn.stdpath("data") .. "/lazy" }), desc = "Find Data files" },
    { "<leader>fs", "<cmd>Telescope egrepify<cr>", desc = "Egrepify" },
    { "<leader>sf", "<cmd>Telescope egrepify<cr>", desc = "Egrepify" },
  },
  -- -- NOTE: in insert-mode(and when fzf-native plugin is installed) you can use <c-space> to refine the results i.e further search on searched results
  -- opts.defaults.mappings.i = vim.tbl_extend("force", opts.defaults.mappings.i, {
  --   ["<C-j>"] = actions.move_selection_next,
  --   ["<C-k>"] = actions.move_selection_previous,
  --   ["<C-n>"] = actions.cycle_history_next,
  --   ["<C-p>"] = actions.cycle_history_prev,
  --   ["<C-t>"] = actions.select_tab,
  --   ["<C-c>"] = actions.close,
  --   ["<A-p>"] = action_layout.toggle_preview,
  --   ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
  -- })
  -- opts.defaults.mappings.n = vim.tbl_extend("force", opts.defaults.mappings.n, {
  --   q = actions.close,
  --   ["<A-p>"] = action_layout.toggle_preview,
  --   ["<Space>"] = actions.toggle_selection,
  -- })
  --
}
