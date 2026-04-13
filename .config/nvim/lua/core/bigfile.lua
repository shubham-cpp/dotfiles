local M = {}

M.config = {
  filesize = 1 * 1024 * 1024, -- 1 MB
  line_count = 25000, -- 25K lines
  avg_line_length = 1000, -- 1000 chars average
  min_size_for_linecheck = 64 * 1024, -- Only check avg line length for files >= 64KB
}

function M.is_big(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  local buftype = vim.bo[buf].buftype
  if buftype ~= "" then
    return false
  end
  local filename = vim.api.nvim_buf_get_name(buf)
  if filename == "" then
    return false
  end

  local stat = vim.uv.fs_stat(filename)
  if stat and stat.size > M.config.filesize then
    return true
  end

  if vim.api.nvim_buf_is_loaded(buf) then
    local lines = vim.api.nvim_buf_line_count(buf)
    if lines > M.config.line_count then
      return true
    end
    if stat and stat.size >= M.config.min_size_for_linecheck and lines > 0 then
      if (stat.size / lines) > M.config.avg_line_length then
        return true
      end
    end
  end

  return false
end

function M.setup(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  if vim.b[buf].bigfile then
    return
  end
  vim.b[buf].bigfile = true

  -- Buffer-local options
  vim.bo[buf].undolevels = -1
  vim.bo[buf].swapfile = false

  -- Window-local options
  vim.wo[0].conceallevel = 0
  vim.wo[0].relativenumber = false
  vim.wo[0].cursorline = false
  vim.wo[0].spell = false
  vim.wo[0].foldmethod = "indent"

  -- Disable matchparen (expensive bracket matching on every cursor move)
  if vim.fn.exists(":NoMatchParen") ~= 0 then
    vim.cmd([[NoMatchParen]])
  end

  -- Plugin disable flags (checked natively by each plugin)
  vim.b[buf].miniindentscope_disable = true
  -- vim.b[buf].completion = false
  vim.b[buf].disable_auto_format = true
  vim.bo[buf].formatexpr = ""

  -- Stop treesitter if already running
  pcall(vim.treesitter.stop, buf)

  -- Enable basic syntax highlighting as lightweight fallback
  vim.schedule(function()
    if vim.api.nvim_buf_is_valid(buf) then
      local ft = vim.filetype.match({ buf = buf }) or ""
      if ft ~= "" then
        vim.bo[buf].syntax = ft
      end
    end
  end)

  vim.notify(
    "Big file detected (" .. vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t") .. ")",
    vim.log.levels.WARN
  )
end

return M
