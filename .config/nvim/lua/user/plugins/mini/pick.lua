local function pick_cmd(cmd)
  return '<cmd>Pick ' .. cmd .. '<cr>'
end
---@type LazySpec
return {
  'echasnovski/mini.pick',
  enabled = false,
  dependencies = {
    'echasnovski/mini.icons',
    {
      'echasnovski/mini.extra',
      config = function()
        require('mini.extra').setup()
      end,
    },
  },
  cmd = { 'Pick' },
  keys = {
    { '<C-p>', pick_cmd 'files', desc = '[F]ile' },
    { '<leader>f', '', desc = '+pick' },
    {
      '<leader>fb',
      pick_cmd 'buffers include_current = false',
      desc = '[B]uffer',
    },
    { '<leader>ff', pick_cmd 'files', desc = '[F]ile' },
    {
      '<leader>fn',
      pick_cmd(string.format("files cwd='%s'", vim.fn.stdpath 'config')),
      desc = '[N]eovim config',
    },
    {
      '<leader>fN',
      pick_cmd(string.format("files cwd='%s'", vim.fn.stdpath 'data' .. '/lazy')),
      desc = '[N]eovim data directory',
    },
    {
      '<leader>fd',
      pick_cmd(string.format("files cwd='%s'", vim.fn.expand '~/Documents/dotfiles/.config')),
      desc = '[D]ot config',
    },
    { '<leader>fs', pick_cmd 'grep_live', desc = '[S]earch' },
    { '<leader>fw', pick_cmd "grep pattern='<cword>'", desc = '[W]ord under cursor' },
    { '<leader>fh', pick_cmd 'help', desc = '[H]elp' },
    { '<leader>fr', pick_cmd 'resume', desc = '[R]esume' },
    { '<leader>fo', pick_cmd 'oldfiles', desc = '[O]ldfiles' },
    { '<leader>fk', pick_cmd 'keymaps', desc = '[K]eymaps' },
    { '<leader>fO', pick_cmd 'oldfiles current_dir = true', desc = '[O]ldfiles(Current Dir)' },
  },
  opts = function()
    local picker = require 'mini.pick'
    return {
      mappings = {
        choose_in_split = '<C-x>',
        mark = '<C-l>',
        -- choose_marked   = '<C-q>',
        move_down = '<C-j>',
        move_up = '<C-k>',
        send_to_qflist = {
          char = '<C-q>',
          func = function()
            local list = {}
            local matches = picker.get_picker_matches().all

            for _, match in ipairs(matches) do
              if type(match) == 'table' then
                table.insert(list, match)
              else
                local path, lnum, col, search = string.match(match, '(.-)%z(%d+)%z(%d+)%z%s*(.+)')
                local text = path and string.format('%s [%s:%s]  %s', path, lnum, col, search)
                local filename = path or vim.trim(match):match '%s+(.+)'

                table.insert(list, {
                  filename = filename or match,
                  lnum = lnum or 1,
                  col = col or 1,
                  text = text or match,
                })
              end
            end

            vim.fn.setqflist(list, 'r')
          end,
        },
      },
      options = { use_cache = true },
      -- source = {
      --   show = function(buf_id, items, query, opts)
      --     picker.default_show(
      --       buf_id,
      --       items,
      --       query,
      --       vim.tbl_deep_extend('force', { show_icons = false, icons = {} }, opts or {})
      --     )
      --   end,
      -- },
      -- Window related options
      window = {
        config = function()
          local height, width, starts, ends
          local win_width = vim.o.columns
          local win_height = vim.o.lines

          if win_height <= 25 then
            height = math.min(win_height, 18)
            width = win_width
            starts = 1
            ends = win_height
          else
            width = math.floor(win_width * 0.5) -- 50%
            height = math.floor(win_height * 0.6) -- 30%
            starts = math.floor((win_width - width) / 2)
            -- center prompt: height * (50% + 30%)
            -- center window: height * [50% + (30% / 2)]
            ends = math.floor(win_height * 0.85)
          end

          return {
            col = starts,
            row = ends,
            height = height,
            width = width,
            style = 'minimal',
            border = { ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ' },
          }
        end,
      },
    }
  end,
  config = function(_, opts)
    local picker = require 'mini.pick'
    picker.setup(opts)
    -- Make `:Pick files` accept `cwd`
    MiniPick.registry.files = function(local_opts)
      local opts = { source = { cwd = local_opts.cwd } }
      local_opts.cwd = nil
      return MiniPick.builtin.files(local_opts, opts)
    end
    vim.ui.select = picker.ui_select
  end,
}
