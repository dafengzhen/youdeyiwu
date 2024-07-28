import { Plugin } from 'ckeditor5';
import CodeBlockEditing from './code-block-editing';
import CodeBlockUI from './code-block-ui';

export interface ISelectArgs {
  id: string;
  language: string;
}

export interface IOpenArgs {
  id?: string;
  select: (args: ISelectArgs) => void;
  close: () => void;
}

export interface IRendererArgs {
  id: string;
  language: string;
  el: HTMLElement;
  value: string | undefined | null;
}

export interface IYwCodeBlockConfig {
  open: (args: IOpenArgs) => void;
  renderer: (args: IRendererArgs) => void;
}

export default class CodeBlock extends Plugin {
  static get requires() {
    return [CodeBlockEditing, CodeBlockUI];
  }
}
