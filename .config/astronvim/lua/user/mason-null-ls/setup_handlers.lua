local null_ls = require 'null-ls'
local config = {
  function(source_name, methods)
    -- all sources with no handler get passed here

    -- To keep the original functionality of `automatic_setup = true`,
    -- please add the below.
    require 'mason-null-ls.automatic_setup' (source_name, methods)
  end,
  black = function(source_name, methods)
    null_ls.register(null_ls.builtins.formatting.black.with({ extra_args = { '-l', '80', '--fast' } }))
  end,
  shfmt = function(source_name, methods)
    null_ls.register(null_ls.builtins.formatting.shfmt.with({ extra_args = { '-i', '2', '-ci' } }))
  end,
}
return config
