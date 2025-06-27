---@param bufname string
---@return string[]
local function eslint_cmd(bufname)
  local cmd = {}
  if vim.fn.executable "eslint_d" == 1 then
    cmd = { "eslint_d" }
  elseif vim.fn.executable(vim.uv.cwd() .. "/node_modules/.bin/eslint") == 1 then
    cmd = { "npx", "eslint" }
  else
    cmd = { "eslint" }
  end

  table.insert(cmd, "--format=json")
  table.insert(cmd, "--stdin")
  table.insert(cmd, "--stdin-filename=" .. bufname)

  return cmd
end
---@type LazySpec
return {
  "YaroSpace/dev-tools.nvim",
  event = "LspAttach",
  dependencies = {
    { "ThePrimeagen/refactoring.nvim", dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" } },
  },
  opts = {
    ui = { group_actions = false },
    ---@type Action[]| fun():Action[]
    actions = {
      {
        name = "Eslint Suggestion",
        ft = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
        fn = function(action)
          local ctx = action.ctx
          local all_lines = vim.api.nvim_buf_get_lines(ctx.buf, 0, -1, false)

          local obj = vim.system(eslint_cmd(ctx.bufname), { text = true, stdin = all_lines }):wait()
          if not obj.stdout then
            return
          end

          for _, out in ipairs(vim.json.decode(obj.stdout)) do
            for _, message in ipairs(out.messages) do
              if message.suggestions then
                for _, suggestion in ipairs(message.suggestions) do
                  ctx.edit:set_range(
                    { suggestion.fix.text },
                    message.line - 1,
                    message.column - 1,
                    message.endLine - 1,
                    message.endColumn - 1
                  )
                end
              else
                ctx.edit:set_range(
                  { message.fix.text },
                  message.line - 1,
                  message.column - 1,
                  message.endLine - 1,
                  message.endColumn - 1
                )
              end
            end
          end
        end,
      },
    },
  },
}
