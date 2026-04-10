local M = {}
local uv = vim.uv or vim.loop

local defaults = {
  name = "k1-lsp",
  auto_start = true,
  k1_home = nil,
  lsp_binary = nil,
  cmd_env = {
    RUST_BACKTRACE = "1",
  },
}

M.options = vim.deepcopy(defaults)

local function normalize(path)
  local expanded = vim.fn.expand(path)
  local abs = vim.fn.fnamemodify(expanded, ":p")
  return abs:gsub("/$", "")
end

local function current_options()
  if not M.options then
    M.options = vim.deepcopy(defaults)
  end
  return M.options
end

local function resolve_k1_home(opts)
  local o = opts or current_options()
  local configured = o.k1_home or vim.env.K1_HOME or "~/.k1"
  return normalize(configured)
end

local function resolve_lsp_binary(opts)
  local o = opts or current_options()
  if o.lsp_binary and o.lsp_binary ~= "" then
    return normalize(o.lsp_binary)
  end
  local k1_home = resolve_k1_home(o)
  return normalize(k1_home .. "/bin/k1lsp")
end

local function find_root(bufnr)
  if not bufnr or bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local filename = vim.api.nvim_buf_get_name(bufnr)
  if filename == "" then
    return uv.cwd()
  end
  return vim.fs.dirname(filename)
end

local function is_executable(path)
  return vim.fn.executable(path) == 1
end

function M.setup(opts)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(defaults), opts or {})
end

function M.start(bufnr, opts)
  if not bufnr or bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local o = vim.tbl_deep_extend("force", vim.deepcopy(current_options()), opts or {})

  local lsp_binary = resolve_lsp_binary(o)
  if not is_executable(lsp_binary) then
    local k1_home = resolve_k1_home(o)
    vim.notify(
      ("k1.nvim: k1lsp not found/executable at '%s' (K1_HOME='%s')"):format(lsp_binary, k1_home),
      vim.log.levels.ERROR
    )
    return nil
  end

  local k1_home = resolve_k1_home(o)
  local cmd_env = vim.tbl_extend("force", { K1_HOME = k1_home }, o.cmd_env or {})
  local root_dir = o.root_dir or find_root(bufnr)

  local start_opts = { bufnr = bufnr }
  if o.force_new then
    start_opts.reuse_client = function()
      return false
    end
  end

  return vim.lsp.start({
    name = o.name,
    cmd = { lsp_binary },
    cmd_env = cmd_env,
    root_dir = root_dir,
  }, start_opts)
end

function M.restart(bufnr)
  if not bufnr or bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  local name = current_options().name
  local clients = {}
  if vim.lsp.get_clients then
    clients = vim.lsp.get_clients({ name = name })
  else
    for _, client in ipairs(vim.lsp.get_active_clients()) do
      if client.name == name then
        table.insert(clients, client)
      end
    end
  end
  for _, client in ipairs(clients) do
    client:stop(true)
  end
  return M.start(bufnr, { force_new = true })
end

function M.on_ftplugin(bufnr)
  if not bufnr or bufnr == 0 then
    bufnr = vim.api.nvim_get_current_buf()
  end
  if current_options().auto_start then
    M.start(bufnr)
  end
end

return M
