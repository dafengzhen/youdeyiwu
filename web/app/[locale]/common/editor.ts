import sanitizeHtml from 'sanitize-html';
import type { MutableRefObject } from 'react';
import type { ClassicEditor } from '@ckeditor/ckeditor5-editor-classic';

export const getContent = (
  editorRef: MutableRefObject<ClassicEditor | null>,
) => {
  const value = editorRef.current?.getData().trim();
  if (!value) {
    return '';
  }

  return sanitizeHtmlContent(value);
};

export const setContent = (
  content: string,
  editorRef: MutableRefObject<ClassicEditor | null>,
) => {
  editorRef.current?.setData(content.trim());
};

export const getFocus = (editorRef: MutableRefObject<ClassicEditor | null>) => {
  editorRef.current?.focus();
};

export const sanitizeHtmlContent = (value: string) => {
  return sanitizeHtml(value, {
    allowedTags: false,
    allowedAttributes: false,
    allowVulnerableTags: true,
  });
};
