import { Command, Editor } from '@ckeditor/ckeditor5-core';
import { IOpenArgs, IYwCodeBlockConfig } from './code-block';

export default class CodeBlockCommand extends Command {
  private isOn = false;

  constructor(editor: Editor) {
    super(editor);
    this._initListeners();
  }

  override execute(args: Pick<IOpenArgs, 'id'>) {
    this.fire('yw:codeBlock:open', args);
  }

  override refresh() {
    this.value = this.isOn;
    this.isEnabled = true;
  }

  private _initListeners() {
    const config = this.editor.config.get('ywCodeBlock') as IYwCodeBlockConfig;
    const open = config.open;
    const model = this.editor.model;

    this.on('yw:codeBlock:open', (eventInfo, args) => {
      if (typeof open === 'function') {
        this.isOn = true;
        open({
          id: args.id,
          select: (_args) => {
            this.fire('yw:codeBlock:select', _args);
          },
          close: () => {
            this.fire('yw:codeBlock:close');
          },
        });
      } else {
        console.error('No callback function configured');
      }
    });

    this.on('yw:codeBlock:close', (eventInfo, args) => {
      this.isOn = false;
    });

    this.on('yw:codeBlock:select', (eventInfo, args) => {
      const id = args.id;
      const language = args.language;
      model.change((writer) => {
        model.insertContent(
          writer.createElement('ywCodeBlock', {
            id,
            language,
          }),
        );
      });

      this.fire('yw:codeBlock:close');
    });
  }

  override destroy() {
    super.destroy();
    this.fire('yw:codeBlock:close');
  }
}
