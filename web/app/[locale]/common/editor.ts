import sanitizeHtml from 'sanitize-html';
import type { MutableRefObject, RefObject } from 'react';
import type { IToastRef } from '@/app/[locale]/common/toasts';

declare const CKSource: any;

export const getContent = (editorRef: MutableRefObject<any>) => {
  const currentEditor = editorRef.current;
  if (!currentEditor) {
    return '';
  }

  const value = currentEditor.getData().trim();
  if (!value) {
    return '';
  }

  return sanitizeHtmlContent(value);
};

export const setContent = (
  content: string,
  editorRef: MutableRefObject<any>,
) => {
  const currentEditor = editorRef.current;
  if (!currentEditor) {
    console.error('Failed to set editor content');
    return;
  }
  currentEditor.setData(content.trim());
};

export const getFocus = (editorRef: MutableRefObject<any>) => {
  editorRef.current?.focus();
};

export const onLoadEditor = (
  editorElementRef: RefObject<HTMLDivElement>,
  editorRef: MutableRefObject<any>,
  callback?: () => void,
) => {
  const currentElement = editorElementRef.current;
  if (!currentElement) {
    console.error('Editor element node does not exist');
    return;
  }

  if (!CKSource) {
    console.error('Editor initialization failed');
    return;
  }

  const watchdog = new CKSource.EditorWatchdog();
  watchdog.create(currentElement).catch(console.error);
  watchdog.on('error', console.error);
  watchdog.setDestructor((editor: any) => editor.destroy());
  watchdog.setCreator((element: any, config: any) =>
    CKSource.Editor.create(element, config).then((editor: any) => {
      editorRef.current = editor;
      return editor;
    }),
  );

  if (typeof callback === 'function') {
    callback();
  }
};

export const onErrorEditor = (
  e: any,
  toast: MutableRefObject<IToastRef>,
  callback?: () => void,
) => {
  console.error(e);
  toast.current.show({
    type: 'danger',
    message: e.message ?? 'Failed to load the editorRef',
  });

  if (typeof callback === 'function') {
    callback();
  }
};

export const sanitizeHtmlContent = (value: string) => {
  return sanitizeHtml(value, {
    allowedTags: false,
    allowedAttributes: false,
    allowVulnerableTags: true,
  });
};
