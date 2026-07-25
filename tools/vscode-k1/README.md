# K1 VS Code Extension

This extension starts `k1lsp` and wires it to VS Code using LSP. Diagnostics, hover, and any other server-provided features are handled through the language server.

## Project roots

`k1lsp` compiles its root directory as one module, so the extension starts one
server instance per k1 project rather than one per VS Code workspace. When a
`.k1` file is opened, its project root is the nearest ancestor directory
containing `main.k1`, `proj.k1`, or `.git` (the same markers k1.nvim uses),
falling back to the file's own directory. The server runs with that root as its
working directory.

## Server discovery order

If `k1.languageServer.path` is not set, the extension tries:

1. `$K1_HOME/bin/k1lsp` (when `K1_HOME` is set in the VS Code extension host environment)
2. `~/.k1/bin/k1lsp`

## Extension settings

- `k1.languageServer.path`: optional absolute path to `k1lsp`
- `k1.languageServer.args`: optional array of arguments passed to `k1lsp`

## Development

```bash
cd /Users/knix/dev/k1/tools/vscode-k1
npm install
npm run compile
```

Then in VS Code:

1. Open this folder.
2. Press `F5` to launch an Extension Development Host.
3. Open a `.k1` file and confirm diagnostics/hover from the language server.
