# K1 VS Code Extension

This extension starts `k1lsp` and wires it to VS Code using LSP. Diagnostics, hover, and any other server-provided features are handled through the language server.

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
