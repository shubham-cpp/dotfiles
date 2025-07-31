---@type LazySpec
return {
  "hrsh7th/nvim-cmp",
  dependencies = { "lukas-reineke/cmp-rg" },
  optional = true,
  opts = function(_, opts)
    ---@return cmp.ComparatorFunction
    local function lower_emmet()
      return function(e1, e2)
        local is_e1_emmet = e1.source:get_debug_name() == "nvim_lsp:emmet_ls"
          or e1.source:get_debug_name() == "nvim_lsp:emmet_language_server"
        local is_e2_emmet = e2.source:get_debug_name() == "nvim_lsp:emmet_ls"
          or e2.source:get_debug_name() == "nvim_lsp:emmet_language_server"
        if is_e1_emmet then return false end
        if is_e2_emmet then return true end
        return nil
      end
    end
    local luasnip, cmp = require "luasnip", require "cmp"
    local defaults = require "cmp.config.default"()

    local function is_visible(_cmp) return _cmp.core.view:visible() or vim.fn.pumvisible() == 1 end

    opts.mapping["<C-x><C-x>"] = cmp.mapping.complete {
      config = { sources = { { name = "luasnip" } } },
    }
    opts.mapping["<C-y>"] = cmp.mapping {
      i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
      c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
    }
    opts.mapping["<CR>"] = cmp.mapping {
      i = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
      c = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = false },
    }
    opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
      if is_visible(cmp) then
        cmp.select_next_item()
      elseif vim.api.nvim_get_mode().mode ~= "c" and luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { "i", "s" })
    opts.mapping["<S-Tab>"] = cmp.mapping(function(fallback)
      if is_visible(cmp) then
        cmp.select_prev_item()
      elseif vim.api.nvim_get_mode().mode ~= "c" and luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" })

    opts.sources = cmp.config.sources {
      { name = "lazydev", priority = 1000, group_index = 0 },
      { name = "nvim_lsp", priority = 1000, group_index = 1 },
      { name = "path", priority = 1000, group_index = 1 },
      { name = "luasnip", priority = 950, group_index = 1 },
      {
        name = "buffer",
        group_index = 2,
        priority = 350,
        option = {
          get_bufnrs = function() return vim.fn.tabpagebuflist() end,
        },
      },
      {
        name = "rg",
        keyword_length = 3,
        max_item_count = 10,
        group_index = 2,
        priority = 200,
        option = { additional_arguments = "--smart-case" },
      },
    }
    opts.sorting = {
      priority_weight = defaults.sorting.priority_weight,
      comparators = vim.tbl_extend("keep", {
        -- deprio(types.lsp.CompletionItemKind.Text),
        lower_emmet(),
      }, defaults.sorting.comparators),
    }
  end,
}
