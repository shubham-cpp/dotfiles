local M = {}

---An alias to nvim_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string the command to execute
---@param opts ?table contains options like noremap,silent
---@see nvim_set_keymap
function M.map(mode, lhs, rhs, opts)
  local opts = opts == nil and { silent = true, noremap = true } or opts
  vim.keymap.set(mode, lhs, rhs, opts)
end

---An alias to nvim_buf_set_keymap
---@param mode string (n)ormal,(v)isual,etc
---@param lhs string the key bind/sequence to press
---@param rhs string the command to execute
---@param opts ?table contains options like noremap,silent
---@param bufnr ?number buffer id
---@see nvim_buf_set_keymap
function M.bmap(mode, lhs, rhs, opts, bufnr)
  local opts = opts == nil and { silent = true, noremap = true } or opts
  opts.buffer = bufnr or true
  vim.keymap.set(mode, lhs, rhs, opts)
end

---Wrapper function to print lua table
---@param value table Table to print
function _G.p(value)
  print(vim.inspect(value))
end

function NewName()
  local position_params = vim.lsp.util.make_position_params()
  vim.ui.input({ prompt = 'Enter new name', default = vim.fn.expand('<cword>') }, function(newName)
    if newName == '' or not newName then
      print('Please provide a new name')
      return
    end
    position_params.newName = newName
    local entries = {}
    vim.lsp.buf_request(vim.fn.bufnr(), 'textDocument/rename', position_params, function(err, method, result, ...)
      p({
        err = err,
        method = method,
        result = result,
      })
      -- TODO: Handle err properly
      if method.changes == nil then
        print('Error while renaming')
        p({ err = err })
        return
      end
      local bufnr = result.bufnr
      for file_uri, edits in pairs(method.changes) do
        for _, edit in ipairs(edits) do
          local col = edit.range.start.character + 1
          local lnum = edit.range.start.line + 1
          local text = vim.api.nvim_buf_get_lines(bufnr, lnum - 1, lnum, false)[1]
          table.insert(entries, {
            bufnr = bufnr,
            col = col,
            lnum = lnum,
            text = text,
          })
        end
      end
      vim.fn.setqflist(entries, 'r')
      vim.lsp.handlers['textDocument/rename'](err, method, result, ...)
    end)
  end)
end

return M
