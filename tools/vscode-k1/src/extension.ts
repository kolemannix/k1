import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from "vscode-languageclient/node";

const ROOT_MARKERS = ["main.k1", "module.k1", "setup.k1"];

const clients = new Map<string, LanguageClient>();
let missingServerReported = false;

function expandPath(rawPath: string): string {
  const home = os.homedir();
  let expanded = rawPath;

  if (expanded.startsWith("~/")) {
    expanded = path.join(home, expanded.slice(2));
  }

  if (expanded.includes("$K1_HOME") && process.env.K1_HOME) {
    expanded = expanded.replace("$K1_HOME", process.env.K1_HOME);
  }

  return expanded;
}

function isRunnableFile(filePath: string): boolean {
  if (!fs.existsSync(filePath)) {
    return false;
  }

  try {
    fs.accessSync(filePath, fs.constants.X_OK);
    return true;
  } catch {
    return process.platform === "win32";
  }
}

function resolveServerPath(): { path?: string; searched: string[] } {
  const config = vscode.workspace.getConfiguration("k1");
  const configuredPath = (config.get<string>("languageServer.path") || "").trim();

  const searched: string[] = [];
  if (configuredPath.length > 0) {
    const candidate = expandPath(configuredPath);
    searched.push(candidate);
    if (isRunnableFile(candidate)) {
      return { path: candidate, searched };
    }
    return { searched };
  }

  if (process.env.K1_HOME) {
    const fromK1Home = path.join(process.env.K1_HOME, "bin", "k1lsp");
    searched.push(fromK1Home);
    if (isRunnableFile(fromK1Home)) {
      return { path: fromK1Home, searched };
    }
  }

  const fromHome = path.join(os.homedir(), ".k1", "bin", "k1lsp");
  searched.push(fromHome);
  if (isRunnableFile(fromHome)) {
    return { path: fromHome, searched };
  }

  return { searched };
}

// The server compiles its root_uri directory as one module, so the root must be
// the k1 project containing the file, not the VS Code workspace folder. Same
// markers as k1.nvim: nearest ancestor containing main.k1, proj.k1, or .git.
function findProjectRoot(filePath: string): string {
  let dir = path.dirname(filePath);
  while (true) {
    if (ROOT_MARKERS.some((marker) => fs.existsSync(path.join(dir, marker)))) {
      return dir;
    }
    const parent = path.dirname(dir);
    if (parent === dir) {
      return path.dirname(filePath);
    }
    dir = parent;
  }
}

function reportMissingServer(searched: string[]): void {
  if (missingServerReported) {
    return;
  }
  missingServerReported = true;
  const searchedPaths = searched.length > 0 ? searched.join(", ") : "none";
  void vscode.window
    .showErrorMessage(
      `k1 language server was not found. Looked in: ${searchedPaths}`,
      "Open Settings"
    )
    .then((selection) => {
      if (selection === "Open Settings") {
        void vscode.commands.executeCommand(
          "workbench.action.openSettings",
          "k1.languageServer.path"
        );
      }
    });
}

function ensureClientForDocument(document: vscode.TextDocument): void {
  if (document.languageId !== "k1" || document.uri.scheme !== "file") {
    return;
  }

  const root = findProjectRoot(document.uri.fsPath);
  if (clients.has(root)) {
    return;
  }

  const resolved = resolveServerPath();
  if (!resolved.path) {
    reportMissingServer(resolved.searched);
    return;
  }

  const serverArgs = vscode.workspace
    .getConfiguration("k1")
    .get<string[]>("languageServer.args", []);
  const serverOptions: ServerOptions = {
    command: resolved.path,
    args: serverArgs,
    options: { cwd: root }
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { language: "k1", scheme: "file", pattern: `${root}/**/*.k1` }
    ],
    workspaceFolder: {
      uri: vscode.Uri.file(root),
      name: path.basename(root),
      index: 0
    },
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher(
        new vscode.RelativePattern(root, "**/*.k1")
      )
    }
  };

  const client = new LanguageClient(
    "k1LanguageServer",
    `k1 Language Server (${path.basename(root)})`,
    serverOptions,
    clientOptions
  );
  clients.set(root, client);
  void client.start();
}

export function activate(context: vscode.ExtensionContext): void {
  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(ensureClientForDocument)
  );
  for (const document of vscode.workspace.textDocuments) {
    ensureClientForDocument(document);
  }
}

export async function deactivate(): Promise<void> {
  const stopping = [...clients.values()].map((client) => client.stop());
  clients.clear();
  await Promise.all(stopping);
}
