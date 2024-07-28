import { ButtonView, icons, Plugin } from 'ckeditor5';

import './theme/code-block.css';

export default class CodeBlockUi extends Plugin {
  init() {
    const editor = this.editor;
    const t = editor.t;

    editor.ui.componentFactory.add('CodeBlock', (locale) => {
      const buttonView = new ButtonView(locale);

      buttonView.set({
        label: t('Code Block'),
        withText: false,
        tooltip: true,
        icon: icons.codeBlock,
      });

      const command = editor.commands.get('ywCodeBlock');
      buttonView
        .bind('isOn', 'isEnabled')
        .to(command as any, 'value', 'isEnabled');

      this.listenTo(buttonView, 'execute', () => {
        const selectedElement =
          editor.editing.view.document.selection.getSelectedElement();
        let id;
        if (selectedElement) {
          id = selectedElement.getCustomProperty('id');
        }

        editor.execute('ywCodeBlock', { id });
      });
      return buttonView;
    });
  }
}
