local MiniPick = require 'mini.pick'
local bufnr = 0
local winnr = 0
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
_G.lsp_make_on_list = function(source, opts)
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

local get_hash = function()
  local str = 'echo "dir:' .. vim.fn.getcwd()
  if vim.b.gitsigns_head then
    str = str .. ';git:' .. vim.b.gitsigns_head
  end
  local hash = vim.fn.system(str .. "\" | md5sum | awk '{print $1}'")
  return hash
end

local mini_mru = function()
  local hash = get_hash()
  local cmd =
    string.format("command cat <(fre --sorted --store_name %s) <(fd -t f --color never) | awk '!x[$0]++'", hash)

  vim.fn.jobstart(cmd, {
    stdout_buffered = true,
    on_stdout = function(_, data)
      table.remove(data, #data)
      local show_with_icons = function(buf_id, items, query)
        MiniPick.default_show(buf_id, items, query, { show_icons = true })
      end
      local function wipe_out()
        -- local picker_items = MiniPick.get_picker_items()
        local items = MiniPick.get_picker_matches().all
        local sel = MiniPick.get_picker_matches().current
        local sel_index = MiniPick.get_picker_matches().current_ind
        -- local bufnr = picker_items[sel_index].bufnr

        vim.fn.system('fre --delete ' .. sel .. ' --store_name ' .. hash)
        -- vim.api.nvim_buf_delete(bufnr, {})
        table.remove(items, sel_index)

        MiniPick.set_picker_items(items)
      end
      MiniPick.start({
        source = {
          items = data,
          name = 'MRU',
          choose = function(item)
            if vim.fn.filereadable(item) == 0 then
              return
            end
            vim.fn.system('fre --add ' .. item .. ' --store_name ' .. hash)
            MiniPick.default_choose(item)
          end,
          show = show_with_icons,
        },
        mappings = {
          wipe_out = {
            char = '<C-d>',
            func = wipe_out,
          },
        },
      })
    end,
  })
  -- local items = vim.fn.system(cmd)
end

local function fzf_mru(opts)
  local fzf = require 'fzf-lua'
  local actions = require 'fzf-lua.actions'
  opts = fzf.config.normalize_opts(opts, fzf.config.globals.files)
  opts.fzf_opts = vim.tbl_extend('force', opts.fzf_opts, {
    ['--tiebreak'] = 'index',
  })
  local hash = get_hash()
  -- opts.cmd = string.format("command cat <(mru_tracker --store %s --list)| awk '!x[$0]++'", hash)
  -- opts.cmd = string.format("command cat <(fre --sorted --store_name %s) <(fd -t f --color never) | awk '!x[$0]++'", hash)
  vim.print('hash : ' .. hash)
  opts.cmd = 'command cat <(mru_tracker --store_list ' .. hash .. ') <(fd -t f --color never) | my_uniq'
  -- opts.cmd = string.format("awk '!x[$0]++' <(cat <(fre --sorted --store_name %s) <(fd -t f --color never))", hash)
  opts.actions = vim.tbl_extend('force', opts.actions or {}, {
    ['ctrl-d'] = {
      function(sel)
        vim.print(sel)
        if #sel < 1 then
          return
        end
        -- vim.fn.system('fre --delete ' .. sel[1] .. ' --store_name ' .. hash)
        vim.print('sel : ' .. sel[1])
        local c = string.format('mru_tracker --store_delete %s %s', hash, sel[1])
        c = c:gsub('\n', '')
        vim.print('delc : ' .. c)
        vim.fn.jobstart(c, {
          stdout_buffered = true,
          on_stdout = function(_, data)
            vim.print(data)
          end,
          on_stderr = function(_, data)
            vim.print(data)
          end,
        })
      end,
      actions.resume,
    },
    ['default'] = {
      function(sel, opts)
        vim.print(sel)

        -- actions.file_edit_or_qf({sel[1]},opts)
        if #sel < 2 then
          return
        end
        -- vim.fn.system('fre --add ' .. selected[2] .. ' --store_name ' .. hash)
        -- local c = string.format('mru_tracker --store_add %s %s', hash, selected[2])
        -- c = c:gsub('\n', '')
        -- vim.print('c : ' .. c)
        -- vim.fn.jobstart(c, {
        --   stdout_buffered = true,
        --   on_stdout = function(_, data)
        --     vim.print(data)
        --   end,
        --   on_stderr = function(_, data)
        --     vim.print(data)
        --   end,
        -- })
        -- print('exec:', selected[2])
        -- vim.fn.system('fre --add ' .. selected[2] .. ' --store_name ' .. hash)
      end,
      actions.file_edit,
    },
  })

  -- fzf.core.fzf_wrap(opts, opts.cmd, function(selected)
  --   if not selected or #selected < 2 then
  --     return
  --   end
  --   -- vim.fn.system('fre --add ' .. selected[2] .. ' --store_name ' .. hash)
  --   -- local c = string.format('mru_tracker --store_add %s %s', hash, selected[2])
  --   -- c = c:gsub('\n', '')
  --   -- vim.print('c : ' .. c)
  --   -- vim.fn.jobstart(c, {
  --   --   stdout_buffered = true,
  --   --   on_stdout = function(_, data)
  --   --     vim.print(data)
  --   --   end,
  --   --   on_stderr = function(_, data)
  --   --     vim.print(data)
  --   --   end,
  --   -- })
  --   -- vim.fn.system('mru_tracker --store ' .. hash .. ' --add ' .. selected[2])
  --   fzf.actions.act(opts.actions, selected, opts)
  -- end)()
  -- fzf.files({cmd = opts.cmd, actions = opts.actions})
  fzf.files({
    cmd = opts.cmd,
    actions = {
      ['default'] = function(selected, opts)
        local path = require 'fzf-lua.path'
        local filename = path.entry_to_file(selected[1], opts, opts.force_uri).path
        local cmd = string.format('mru_tracker --store %s --add %s', hash, filename)
        cmd = string.gsub(cmd, '[\n\r]+', ' ')
        vim.fn.jobstart(cmd, {
          stdout_buffered = true,
          on_stderr = function(...)
            vim.print(...)
          end,
        })
        fzf.actions.file_edit(selected, opts)
      end,
      ['ctrl-d'] = {
        fn = function(selected)
          if #selected < 1 then
            return
          end
          local path = require 'fzf-lua.path'
          local filename = path.entry_to_file(selected[1], opts, opts.force_uri).path
          -- vim.fn.system('fre --delete ' .. sel[1] .. ' --store_name ' .. hash)
          local c = string.format('mru_tracker --store %s --delete %s', hash, filename)
          c = c:gsub('[\n\t]+', ' ')
          vim.fn.system(c)
          -- vim.fn.jobstart(c, {
          --   stdout_buffered = true,
          --   on_stdout = function(_, data)
          --     vim.print(data)
          --   end,
          --   on_stderr = function(_, data)
          --     vim.print(data)
          --   end,
          -- })
        end,
        reload = true,
      },
    },
  })
end

vim.api.nvim_create_user_command('MRU', fzf_mru, {})
vim.api.nvim_create_user_command('MiniMRU', mini_mru, {})

vim.api.nvim_create_user_command('References', function()
  vim.lsp.buf.references(nil, { on_list = lsp_make_on_list 'references' })
end, {})
vim.api.nvim_create_user_command('Definition', function()
  vim.lsp.buf.definition({ on_list = lsp_make_on_list 'definition' })
end, {})
local au_group = vim.api.nvim_create_augroup('my_sam', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  group = au_group,
  pattern = 'sample.lua',
  command = 'source <afile> | messages clear',
})
