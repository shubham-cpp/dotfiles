local function win_config()
  local height = math.floor(0.618 * vim.o.lines)
  local width = math.floor(0.618 * vim.o.columns)
  return {
    anchor = 'NW',
    height = height,
    width = width,
    row = math.floor(0.5 * (vim.o.lines - height)),
    col = math.floor(0.5 * (vim.o.columns - width)),
  }
end

local function pick_references(bufnr, winnr)
  local bufnr = bufnr or 0
  local winnr = winnr or 0
  local params = vim.lsp.util.make_position_params(winnr)
  params.context = { includeDeclaration = false }
  local MiniPick = require 'mini.pick'

  vim.lsp.buf_request(bufnr, 'textDocument/references', params, function(err, result)
    if err then
      vim.print 'err\n'
      vim.print(err)
      return
    end
    if result == nil or vim.tbl_isempty(result) then
      vim.print 'empty result'
      return
    end
    local items = {}
    local locations = vim.lsp.util.locations_to_items(result, 'utf-8')
    vim.print(locations)
    for _, location in ipairs(locations) do
      local path = vim.fn.fnamemodify(location.filename, ':p:.')
      table.insert(items, string.format('%s:%d:%d:%s', path, location.lnum, location.col, location.text))
    end
    MiniPick.start({ source = { items = items, name = 'LSP References' } })
    -- vim.lsp.util.jump_to_location(result[1], 'utf-8', true)
  end)
end

-- https://github.com/echasnovski/nvim/raw/7ae7e0007fbd82c802bf1fc02164079034da3bd7/lua/mini-dev/extra.lua
local function p()
  local MiniPick = require 'mini.pick'
  local pick_clear_namespace = function(buf_id)
    pcall(vim.api.nvim_buf_clear_namespace, buf_id, 0, -1)
  end
  local is_valid_buf = function(buf_id)
    return type(buf_id) == 'number' and vim.api.nvim_buf_is_valid(buf_id)
  end
  local pick_prepend_position = function(item)
    local path
    if item.path ~= nil then
      path = item.path
    elseif is_valid_buf(item.bufnr) then
      local name = vim.api.nvim_buf_get_name(item.bufnr)
      if name ~= '' then
        path = name
      end
    end
    if path == nil then
      return item
    end

    path = vim.fn.fnamemodify(path, ':p:.')
    local cur_text = item.text
    local suffix = (cur_text == nil or cur_text == '') and '' or (': ' .. item.text)
    item.text = string.format('%s:%s:%s%s', path, item.lnum or 1, item.col or 1, suffix)
    return item
  end

  local lsp_items_compare = function(a, b)
    local a_path, b_path = a.path or '', b.path or ''
    if a_path < b_path then
      return true
    end
    if a_path > b_path then
      return false
    end

    local a_lnum, b_lnum = a.lnum or 1, b.lnum or 1
    if a_lnum < b_lnum then
      return true
    end
    if a_lnum > b_lnum then
      return false
    end

    local a_col, b_col = a.col or 1, b.col or 1
    if a_col < b_col then
      return true
    end
    if a_col > b_col then
      return false
    end

    return tostring(a) < tostring(b)
  end
  local ns_id = {
    pickers = vim.api.nvim_create_namespace 'MiniExtraPickers',
  }
  local pick_highlight_line = function(buf_id, line, hl_group, priority)
    local opts = { end_row = line, end_col = 0, hl_mode = 'blend', hl_group = hl_group, priority = priority }
    vim.api.nvim_buf_set_extmark(buf_id, ns_id.pickers, line - 1, 0, opts)
  end
  local lsp_make_on_list = function(source, opts)
    -- Prepend file position info to item and sort
    local process = function(items)
      if source ~= 'document_symbol' then
        items = vim.tbl_map(pick_prepend_position, items)
      end
      table.sort(items, lsp_items_compare)
      return items
    end

    -- Highlight symbol kind on Neovim>=0.9 (when `@lsp.type` groups introduced)
    local show
    if source == 'document_symbol' or source == 'workspace_symbol' then
      show = function(buf_id, items_to_show, query)
        MiniPick.default_show(buf_id, items_to_show, query)
        pick_clear_namespace(buf_id)
        for i, item in ipairs(items_to_show) do
          local s = vim.fn.split(item.text, ' ')
          local kind_symbol = require('sp.util').symbols.cmp_kinds[string.sub(s[1], 2, -2)]
          vim.print(s[1] .. ' ' ..  kind_symbol)
          item.text = kind_symbol .. ' ' .. item.text
          -- Highlight using '@...' style highlight group with similar name
          local hl_group = string.format('@%s', string.lower(item.kind or 'unknown'))
          pick_highlight_line(buf_id, i, hl_group, 199)
        end
      end
    end

    return function(data)
      local items = data.items
      for _, item in ipairs(data.items) do
        item.text, item.path = item.text or '', item.filename or nil
      end
      items = process(items)

      return MiniPick.start({
        source = {
          items = items,
          name = string.format('LSP (%s)', source),
          show = show,
        },
      }, opts)
    end
  end
  return lsp_make_on_list
end
local function pick_definition()
  vim.lsp.buf.definition({ on_list = p() 'definition' })
end
local function pick_document_symbol()
  vim.lsp.buf.document_symbol({ on_list = p() 'document_symbol' })
end

vim.api.nvim_create_user_command('PickReferences', function()
  pick_references()
end, {})

local config = {
  {
    'echasnovski/mini.pick',
    version = false,
    keys = {
      { '<leader>pp', "<cmd>Pick files tool='fd'<cr>", desc = '[P]ick [F]ile' },
      { '<leader>pf', "<cmd>Pick files tool='rg'<cr>", desc = '[P]ick [F]ile' },
      { '<leader>pP', "<cmd>Pick files tool='git'<cr>", desc = '[P]ick [F]ile(Git)' },
      { '<leader>ps', '<cmd>Pick grep<cr>', desc = '[P]ick [S]earch' },
      { '<leader>pb', '<cmd>Pick buffers<cr>', desc = '[P]ick [B]uffers' },
      { '<leader>pr', '<cmd>Pick resume<cr>', desc = '[P]ick [R]esume' },
      { '<leader>pld', pick_definition, desc = '[P]ick [L]SP [D]efinition' },
      { '<leader>pls', pick_document_symbol, desc = '[P]ick [L]SP [S]ymbols' },
      { '<leader>plr', pick_references, desc = '[P]ick [L]SP [R]eferences' },
    },
    config = function()
      require('mini.pick').setup({
        mappings = {
          move_down = '<C-j>',
          move_up = '<C-k>',
        },
        options = {
          -- Whether to show content from bottom to top
          content_from_bottom = false,
          -- Whether to cache matches (more speed and memory on repeated prompts)
          use_cache = true,
        },
        window = { config = win_config },
      })
    end,
  },
}
return config
