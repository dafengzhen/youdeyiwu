CKSource.create(document.querySelector('.editor'), {})
  .then((editor) => {
    window.editor = editor;
    CKEditorInspector.attach(editor);
    return editor;
  })
  .catch(handleSampleError);

function handleSampleError(error) {
  const issueUrl = 'https://github.com/ckeditor/ckeditor5/issues';

  const message = [
    'Oops, something went wrong!',
    `Please, report the following error on ${issueUrl} with the build id "lf6x3kqt8pz-bpoqm4axsk3d" and the error stack trace:`,
  ].join('\n');

  console.error(message);
  console.error(error);
}
