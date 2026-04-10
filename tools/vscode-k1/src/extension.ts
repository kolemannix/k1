import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

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

export function activate(context: vscode.ExtensionContext): void {
  const resolved = resolveServerPath();
  if (!resolved.path) {
    const searchedPaths = resolved.searched.length > 0 ? resolved.searched.join(", ") : "none";
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
    return;
  }

  const serverArgs = vscode.workspace
    .getConfiguration("k1")
    .get<string[]>("languageServer.args", []);
  const serverOptions: ServerOptions = {
    command: resolved.path,
    args: serverArgs
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: "k1", scheme: "file" }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.k1")
    }
  };

  client = new LanguageClient("k1LanguageServer", "k1 Language Server", serverOptions, clientOptions);
  context.subscriptions.push(client);
  void client.start();
}

export async function deactivate(): Promise<void> {
  if (!client) {
    return;
  }
  await client.stop();
  client = undefined;
}
