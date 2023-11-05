local M = {}

M.bun_path = function()
  local bun = os.getenv 'BUN_INSTALL'
  local xdg_data = os.getenv 'XDG_DATA_HOME' or os.getenv 'HOME' .. '/.local/share'
  if not bun then
    return xdg_data .. '/bun/bin'
  end
  return bun .. '/bin'
end

--- A wrapper around `vim.keymap.set`
---@param mode string|table 'n'|'v'|'o'
---@param lhs string|function
---@param rhs string|function
---@param opts table<string,boolean | string>? (default is `{noremap = true, silent = true}`)
M.map = function(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts and next(opts) ~= nil then
    vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end

-- Picked up from Astronvim

--- Close a given buffer
---@param bufnr? number The buffer to close or the current buffer if not provided
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close(bufnr, force)
  vim.cmd((force and 'bd!' or 'confirm bd') .. (bufnr == nil and '' or bufnr))
end

--- Close all buffers
---@param keep_current? boolean Whether or not to keep the current buffer (default: false)
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close_all(keep_current, force)
  if keep_current == nil then
    keep_current = false
  end
  local current = vim.api.nvim_get_current_buf()
  for _, bufnr in ipairs(vim.t.bufs) do
    if not keep_current or bufnr ~= current then
      M.close(bufnr, force)
    end
  end
end

--- Close buffers to the left of the current buffer
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close_left(force)
  local current = vim.api.nvim_get_current_buf()
  for _, bufnr in ipairs(vim.t.bufs) do
    if bufnr == current then
      break
    end
    M.close(bufnr, force)
  end
end

--- Close buffers to the right of the current buffer
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function M.close_right(force)
  local current = vim.api.nvim_get_current_buf()
  local after_current = false
  for _, bufnr in ipairs(vim.t.bufs) do
    if after_current then
      M.close(bufnr, force)
    end
    if bufnr == current then
      after_current = true
    end
  end
end

--- Check if a buffer is valid
---@param bufnr number The buffer to check
---@return boolean # Whether the buffer is valid or not
function M.is_valid(bufnr)
  if not bufnr or bufnr < 1 then
    return false
  end
  return vim.api.nvim_buf_is_valid(bufnr) and vim.bo[bufnr].buflisted
end

M.symbols = {
  cmp_kinds = {
    Text = '  ',
    Method = '  ',
    Function = '  ',
    Field = '  ',
    Variable = '  ',
    Interface = '  ',
    Module = '  ',
    Property = '  ',
    Value = '  ',
    Enum = '  ',
    Keyword = '  ',
    Color = '  ',
    File = '  ',
    Folder = '  ',
    EnumMember = '  ',
    Constant = '  ',
    Struct = '  ',
    Event = '  ',
    Operator = '  ',
    TypeParameter = '  ',
    Array = '󰅪',
    Boolean = '⊨',
    Class = '󰌗',
    Constructor = '',
    Key = '󰌆',
    Namespace = '󰅪',
    Null = 'NULL',
    Number = '#',
    Object = '󰀚',
    Package = '󰏗',
    Reference = '',
    Snippet = '',
    String = '󰀬',
    Unit = '',
  },
  lsp_kinds = {
    mode = 'symbol',
    symbol_map = {
      Array = '󰅪',
      Boolean = '⊨',
      Class = '󰌗',
      Constructor = '',
      Key = '󰌆',
      Namespace = '󰅪',
      Null = 'NULL',
      Number = '#',
      Object = '󰀚',
      Package = '󰏗',
      Property = '',
      Reference = '',
      Snippet = '',
      String = '󰀬',
      TypeParameter = '󰊄',
      Unit = '',
    },
    menu = {},
  },
}

--- Get a hash of the current directory + git branch
---@return string
function M.get_hash()
  local str = 'echo "dir:' .. vim.fn.getcwd()
  if vim.b.gitsigns_head then
    str = str .. ';git:' .. vim.b.gitsigns_head
  end
  -- vim.print(str)
  local hash = vim.fn.system(str .. "\" | md5sum | awk '{print $1}'")
  --[[ Without awk
  local hash = vim.fn.system(str .. "\" | md5sum")
  local first_space_index = string.find(hash, " ")
  if first_space_index then
    return string.sub(hash, 1, first_space_index - 1)
  else
    return hash
  end
  --]]
  return hash
end

return M
