import { Plugin } from '@ckeditor/ckeditor5-core';
import { Widget } from '@ckeditor/ckeditor5-widget';
export default class CodeBlockEditing extends Plugin {
    static get requires(): (typeof Widget)[];
    init(): void;
    private _defineConfig;
    private _defineCommand;
    private _defineSchema;
    private _defineConverters;
    private _getAllTextContent;
    private _getAllTextNodes;
    private _getAllText;
}
