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
  event = 'VeryLazy',
  keys = { '<leader>', ']', '[', '<LocalLeader>' },
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
  end,
  opts = {
    -- win = { winblend = 20 },
  },
  config = function(_, opts)
    local wk = require 'which-key'
    wk.setup(opts)
    wk.add({
      { '<leader>f', group = 'Files' },
      { '<leader>l', group = 'LSP' },
      { '<leader>lr', desc = '[R]ename' },
      { '<leader>la', desc = 'Code [A]ction' },
      { '<leader>lI', desc = 'Lsp [I]nfo' },
      { '<leader>t', group = 'Terminal' },
      { '<leader>g', group = '[G]it' },
      { '<leader>o', group = '[O]pen' },
      { '<leader>n', group = '[N]eogen' },
      { '<leader>s', group = 'Sessions' },
      { '<leader>r', group = 'Refactor' },
      { '<leader>w', group = 'Window' },
      { '<leader>u', group = 'Ui' },
      { '<leader>z', group = 'Zk(Notes)' },
      -- { '<leader>p', group = '[P]ick' },
      { '<leader>b', group = 'Buffers' },
      { '<leader>ba', ':badd<space>', desc = 'Add', silent = false },
      {
        '<leader>bc',
        function()
          close_all(true, true)
        end,
        desc = 'Close(except current)',
      },
      {
        '<leader>bC',
        function()
          close_all(false, true)
        end,
        desc = 'Close All(yes ALL)',
      },
      { '<leader>bD', '<cmd>bd<cr>', desc = 'Delete' },
      { '<leader>bn', '<cmd>bn<cr>', desc = 'Next' },
      { '<leader>bp', '<cmd>bp<cr>', desc = 'Prev' },
      { '<leader>1', '1gt', desc = 'Goto tab 1' },
      { '<leader>2', '2gt', desc = 'Goto tab 2' },
      { '<leader>3', '3gt', desc = 'Goto tab 3' },
      { '<leader>4', '4gt', desc = 'Goto tab 4' },
      { '<leader>5', '5gt', desc = 'Goto tab 5' },
      { '<leader>6', '6gt', desc = 'Goto tab 6' },
      { '<leader>7', '7gt', desc = 'Goto tab 7' },
      { '<leader>8', '8gt', desc = 'Goto tab 8' },
      { '<leader>9', '9gt', desc = 'Goto tab 9' },
    })
    wk.add({
      { ']q', '<cmd>cnext<cr>', desc = 'Quickfix next' },
      { '[q', '<cmd>cprev<cr>', desc = 'Quickfix prev' },
      { ']l', '<cmd>lnext<cr>', desc = 'Local next' },
      { '[l', '<cmd>lprev<cr>', desc = 'Local prev' },
      { ']b', '<cmd>bnext<cr>', desc = 'Buffer next' },
      { '[b', '<cmd>bprev<cr>', desc = 'Buffer prev' },
    })
  end,
}
