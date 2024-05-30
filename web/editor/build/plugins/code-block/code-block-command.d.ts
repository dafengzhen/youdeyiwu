import { Command, Editor } from '@ckeditor/ckeditor5-core';
import { IOpenArgs } from './code-block';
export default class CodeBlockCommand extends Command {
    private isOn;
    constructor(editor: Editor);
    execute(args: Pick<IOpenArgs, 'id'>): void;
    refresh(): void;
    private _initListeners;
    destroy(): void;
}
