import { ExtensionContext } from 'vscode';

import {
	LanguageClient, LanguageClientOptions, ServerOptions, TransportKind
} from 'vscode-languageclient/node';
import { join } from 'path';
import { glob } from 'glob';

let client: LanguageClient;

const glob_promise = (path : string) : Promise<string[]> =>
	new Promise((resolve, reject) => {
		glob(path, (err, matches) => {
			if (err) reject(err); else resolve(matches);
		});
	});

export async function activate(_ : ExtensionContext) {
	const executable_path = process.platform === "win32"
		? await glob_promise(join(__dirname, "../../dist-newstyle/build/**/build/lsp/lsp.exe"))
		: await glob_promise(join(__dirname, "../../dist-newstyle/build/**/build/lsp/lsp"))
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
