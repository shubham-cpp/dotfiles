-- Picked up from Astronvim

--- Close a given buffer
---@param bufnr? number The buffer to close or the current buffer if not provided
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function close(bufnr, force)
  if not bufnr or bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end

  local buftype = vim.api.nvim_get_option_value('buftype', { buf = bufnr })
  vim.cmd(('silent! %s %d'):format((force or buftype == 'terminal') and 'bdelete!' or 'confirm bdelete', bufnr))
end

--- Close all buffers
---@param keep_current? boolean Whether or not to keep the current buffer (default: false)
---@param force? boolean Whether or not to foce close the buffers or confirm changes (default: false)
function close_all(keep_current, force)
  if keep_current == nil then
    keep_current = false
  end
  local current = vim.api.nvim_get_current_buf()
  for _, bufnr in ipairs(vim.t.bufs) do
    if not keep_current or bufnr ~= current then
      close(bufnr, force)
    end
  end
end

return {
  'folke/which-key.nvim',
  -- event = "VeryLazy",
  keys = { '<leader>', ']', '[', '<LocalLeader>' },
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,
  opts = {
    disable = { filetypes = { 'TelescopePrompt' } },
    hidden = { '<silent>', '<cmd>', '<Cmd>', '<CR>', 'call', 'lua', '^:', '^ ', 'require' },
    window = {
      winblend = 20,
    },
  },
  config = function(_, opts)
    local wk = require 'which-key'
    wk.setup(opts)
    wk.register({
      f = { name = 'Files' },
      l = { name = 'LSP', r = 'Rename', a = 'Code Action', I = 'Lsp Info' },
      t = { name = 'Terminal' },
      g = { name = 'Git' },
      o = { name = 'Open' },
      n = { name = 'Neogen' },
      s = { name = 'Sessions' },
      z = { name = 'Zk(Notes)' },
      p = { name = '[P]ick', g = '[G]it' },
      b = {
        name = 'Buffers',
        a = { ':badd<space>', 'Add', silent = false },
        c = {
          function()
            close_all(true, true)
          end,
          'Close(except current)',
        },
        C = {
          function()
            close_all(false, true)
          end,
          'Close All(yes ALL)',
        },
        D = { '<cmd>bd<cr>', 'Delete' },
        n = { '<cmd>bn<cr>', 'Next' },
        p = { '<cmd>bp<cr>', 'Prev' },
      },
      ['1'] = { '1gt', 'Goto tab 1' },
      ['2'] = { '2gt', 'Goto tab 2' },
      ['3'] = { '3gt', 'Goto tab 3' },
      ['4'] = { '4gt', 'Goto tab 4' },
      ['5'] = { '5gt', 'Goto tab 5' },
      ['6'] = { '6gt', 'Goto tab 6' },
      ['7'] = { '7gt', 'Goto tab 7' },
      ['8'] = { '8gt', 'Goto tab 8' },
      ['9'] = { '9gt', 'Goto tab 9' },
    }, { prefix = '<leader>' })
    wk.register({
      [']q'] = { '<cmd>cnext<cr>', 'Quickfix next' },
      ['[q'] = { '<cmd>cprev<cr>', 'Quickfix prev' },
      [']l'] = { '<cmd>lnext<cr>', 'Local next' },
      ['[l'] = { '<cmd>lprev<cr>', 'Local prev' },
      [']b'] = { '<cmd>bnext<cr>', 'Buffer next' },
      ['[b'] = { '<cmd>bprev<cr>', 'Buffer prev' },
    })
  end,
}
