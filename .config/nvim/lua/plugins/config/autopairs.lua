local M = {}

local function rule_add_spaces_between_paren()
  local npairs = require 'nvim-autopairs'
  local Rule = require 'nvim-autopairs.rule'
  local cond = require 'nvim-autopairs.conds'

  local brackets = { { '(', ')' }, { '[', ']' }, { '{', '}' } }
  npairs.add_rules({
    -- Rule for a pair with left-side ' ' and right side ' '
    Rule(' ', ' ')
      -- Pair will only occur if the conditional function returns true
      :with_pair(function(opts)
        -- We are checking if we are inserting a space in (), [], or {}
        local pair = opts.line:sub(opts.col - 1, opts.col)
        return vim.tbl_contains({
          brackets[1][1] .. brackets[1][2],
          brackets[2][1] .. brackets[2][2],
          brackets[3][1] .. brackets[3][2],
        }, pair)
      end)
      :with_move(cond.none())
      :with_cr(cond.none())
      -- We only want to delete the pair of spaces when the cursor is as such: ( | )
      :with_del(function(opts)
        local col = vim.api.nvim_win_get_cursor(0)[2]
        local context = opts.line:sub(col - 1, col + 2)
        return vim.tbl_contains({
          brackets[1][1] .. '  ' .. brackets[1][2],
          brackets[2][1] .. '  ' .. brackets[2][2],
          brackets[3][1] .. '  ' .. brackets[3][2],
        }, context)
      end),
  })
  -- For each pair of brackets we will add another rule
  for _, bracket in pairs(brackets) do
    npairs.add_rules({
      -- Each of these rules is for a pair with left-side '( ' and right-side ' )' for each bracket type
      Rule(bracket[1] .. ' ', ' ' .. bracket[2])
        :with_pair(cond.none())
        :with_move(function(opts)
          return opts.char == bracket[2]
        end)
        :with_del(cond.none())
        :use_key(bracket[2])
        -- Removes the trailing whitespace that can occur without this
        :replace_map_cr(function(_)
          return '<C-c>2xi<CR><C-c>O'
        end),
    })
  end
end

local function rule_add_space_on_equal()
  local Rule = require 'nvim-autopairs.rule'
  local cond = require 'nvim-autopairs.conds'
  Rule('=', '')
    :with_pair(cond.not_inside_quote())
    :with_pair(function(opts)
      local last_char = opts.line:sub(opts.col - 1, opts.col - 1)
      if last_char:match '[%w%=%s]' then
        return true
      end
      return false
    end)
    :replace_endpair(function(opts)
      local prev_2char = opts.line:sub(opts.col - 2, opts.col - 1)
      local next_char = opts.line:sub(opts.col, opts.col)
      next_char = next_char == ' ' and '' or ' '
      if prev_2char:match '%w$' then
        return '<bs> =' .. next_char
      end
      if prev_2char:match '%=$' then
        return next_char
      end
      if prev_2char:match '=' then
        return '<bs><bs>=' .. next_char
      end
      return ''
    end)
    :set_end_pair_length(0)
    :with_move(cond.none())
    :with_del(cond.none())
end
-- https://github.com/rstacruz/vim-closer/blob/master/autoload/closer.vim
local get_closing_for_line = function(line)
  local i = -1
  local clo = ''

  while true do
    i, _ = string.find(line, '[%(%)%{%}%[%]]', i + 1)
    if i == nil then
      break
    end
    local ch = string.sub(line, i, i)
    local st = string.sub(clo, 1, 1)

    if ch == '{' then
      clo = '}' .. clo
    elseif ch == '}' then
      if st ~= '}' then
        return ''
      end
      clo = string.sub(clo, 2)
    elseif ch == '(' then
      clo = ')' .. clo
    elseif ch == ')' then
      if st ~= ')' then
        return ''
      end
      clo = string.sub(clo, 2)
    elseif ch == '[' then
      clo = ']' .. clo
    elseif ch == ']' then
      if st ~= ']' then
        return ''
      end
      clo = string.sub(clo, 2)
    end
  end

  return clo
end

local function rule_expand_multiple_on_enter(autopairs)
  local Rule = require 'nvim-autopairs.rule'

  autopairs.remove_rule '('
  autopairs.remove_rule '{'
  autopairs.remove_rule '['

  autopairs.add_rule(Rule('[%(%{%[]', '')
    :use_regex(true)
    :replace_endpair(function(opts)
      return get_closing_for_line(opts.line)
    end)
    :end_wise(function(opts)
      -- Do not endwise if there is no closing
      return get_closing_for_line(opts.line) ~= ''
    end))
end

function M.config(_, opts)
  local options = vim.tbl_deep_extend('force', {
    disable_filetype = { 'TelescopePrompt' },
    ignored_next_char = '[%w%.]',
    check_ts = false,
    map_c_w = true,
    fast_wrap = {},
  }, opts)
  local npairs = require 'nvim-autopairs'
  npairs.setup(options)

  rule_add_spaces_between_paren()
  rule_add_space_on_equal()
  -- rule_expand_multiple_on_enter(npairs)

  local ok, cmp = pcall(require, 'cmp')
  if ok then
    local cmp_autopairs = require 'nvim-autopairs.completion.cmp'
    cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
  end
end

return M
