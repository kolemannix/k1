if vim.g.loaded_k1_nvim == 1 then
  return
end
vim.g.loaded_k1_nvim = 1

local k1 = require("k1")

vim.api.nvim_create_user_command("K1LspStart", function(cmd_opts)
  k1.start(0, { force_new = cmd_opts.bang })
end, {
  bang = true,
  desc = "Start k1 LSP (use ! to force a new client)",
})

vim.api.nvim_create_user_command("K1LspRestart", function()
  k1.restart(0)
end, {
  desc = "Restart k1 LSP",
})
