export function Highlight(editor: any) {
  console.log('Highlight plugin has been registered');

  editor.model.schema.extend('$text', {
    allowAttributes: 'highlight',
  });

  editor.conversion.attributeToElement({
    model: 'highlight',
    view: 'mark',
  });
}
