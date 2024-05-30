import { HighlightCommand } from './highlightCommand';
import { ButtonView } from '@ckeditor/ckeditor5-ui';

export function Highlight(editor: any) {
  console.log('Highlight plugin has been registered');

  editor.config.define('highlight', {
    keystroke: 'Ctrl+Alt+H',
  });

  const keystroke = editor.config.get('highlight.keystroke');

  editor.model.schema.extend('$text', {
    allowAttributes: 'highlight',
  });

  // Convert the input `<mark>` HTML element to model attribute.
  editor.conversion.for('upcast').elementToAttribute({
    model: 'highlight',
    view: 'mark',
  });

  // Convert model attribute to output `<mark>` HTML element.
  editor.conversion.for('dataDowncast').attributeToElement({
    model: 'highlight',
    view: 'mark',
  });

  // Convert model attribute to `<mark>` in editing view.
  editor.conversion.for('editingDowncast').attributeToElement({
    model: 'highlight',
    view: 'mark',
  });

  editor.commands.add('highlight', new HighlightCommand(editor));

  editor.ui.componentFactory.add('highlight', (locale: any) => {
    const button = new ButtonView(locale);
    const command = editor.commands.get('highlight');
    const t = editor.t;

    button.set({
      label: t('Highlight'),
      withText: true,
      tooltip: true,
      isToggleable: true,
      keystroke,
    });

    button.on('execute', () => {
      editor.execute('highlight');
      editor.editing.view.focus();
    });

    // button.bind('isOn', 'isEnabled').to(command, 'value', 'isEnabled');

    command.on(
      'change:value',
      (event: any, propName: any, newValue: any, oldValue: any) => {
        button.isOn = newValue;
      },
    );

    command.on(
      'change:isEnabled',
      (event: any, propName: any, newValue: any, oldValue: any) => {
        button.isEnabled = newValue;
      },
    );

    return button;
  });

  // editor.keystrokes.set('Ctrl+Alt+H', 'highlight');

  editor.keystrokes.set(keystroke, (event: any, cancel: any) => {
    editor.execute('highlight');
    cancel();
  });

  // 5570632 is the code for the 'Ctrl+Alt+H' keyboard shortcut.
  // editor.editing.view.document.on('keydown:5570632', (event, data) => {
  //   // Call the `highlight` command.
  //   editor.execute('highlight');
  //
  //   // Stop the event in the DOM.
  //   data.preventDefault();
  //   data.stopPropagation();
  //
  //   // Stop the event in the framework.
  //   event.stop();
  //
  //   // Mark this event as handled.
  //   event.return = true;
  // });

  const t = editor.t;
  editor.accessibility.addKeystrokeInfos({
    keystrokes: [
      {
        label: t('Highlight text'),
        keystroke: 'Ctrl+Alt+H',
      },
    ],
  });
}
