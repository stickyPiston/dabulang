import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient, LanguageClientOptions, ServerOptions, TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(_: ExtensionContext) {
	const serverOptions: ServerOptions = {
		command: "cabal",
		args: ["run", "lsp"],
		options: {},
		transport: TransportKind.stdio
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "plaintext" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/.clientrc")
		}
	};

	client = new LanguageClient(
		"dabulanglsp",
		"Dabulang Language Server",
		serverOptions,
		clientOptions
	);

	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) return undefined;
	return client.stop();
}
