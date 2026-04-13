local M = {}

M.operations = {
  willRenameFiles = true,
  didRenameFiles = true,
  willCreateFiles = false,
  didCreateFiles = false,
  willDeleteFiles = false,
  didDeleteFiles = false,
}

function M.capabilities()
  return {
    workspace = {
      fileOperations = {
        willRename = M.operations.willRenameFiles,
        didRename = M.operations.didRenameFiles,
        willCreate = M.operations.willCreateFiles,
        didCreate = M.operations.didCreateFiles,
        willDelete = M.operations.willDeleteFiles,
        didDelete = M.operations.didDeleteFiles,
      },
    },
  }
end

return M
