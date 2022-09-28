import { ExtensionContext } from 'vscode';
import {
	LanguageClient, LanguageClientOptions, ServerOptions, TransportKind
} from 'vscode-languageclient/node';
import { join } from 'path';
import { sync } from 'fast-glob';

let client : LanguageClient;

export function activate(_ : ExtensionContext) {
	const executable_path = process.platform === "win32"
		? sync([join(__dirname, "../../dist-newstyle/build/**/build/lsp/lsp.exe").replace(/\\/g, "/")])
		: sync([join(__dirname, "../../dist-newstyle/build/**/build/lsp/lsp")])

	const server_options : ServerOptions = {
		command: executable_path[0],
		options: {
			cwd: join(__dirname, "../../")
		},
		transport: TransportKind.stdio
	};

	const client_options : LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "plaintext" }]
	};

	client = new LanguageClient(
		"dabulanglsp",
		"Dabulang Language Server",
		server_options,
		client_options
	);

	client.start();
}

export function deactivate() : Thenable<void> | undefined {
	if (!client) return undefined;
	return client.stop();
}
