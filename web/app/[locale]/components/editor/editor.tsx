import { CKEditor } from '@ckeditor/ckeditor5-react';
import type { EditorConfig } from '@ckeditor/ckeditor5-core';
import type { EventInfo } from '@ckeditor/ckeditor5-utils';
import { useContext, useEffect, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import CodeBlockCard from '@/app/[locale]/components/editor/code-block';
import CodeBlockPreview from '@/app/[locale]/components/editor/code-block-preview';
import { renderToString } from 'react-dom/server';
import CodeBlock, {
  type IYwCodeBlockConfig,
} from '@/app/[locale]/components/editor/plugins/code-block/code-block';
import {
  Alignment,
  Autoformat,
  AutoImage,
  AutoLink,
  Autosave,
  BlockQuote,
  BlockToolbar,
  Bold,
  ClassicEditor,
  Code,
  DataFilter,
  DataSchema,
  Essentials,
  FindAndReplace,
  FontBackgroundColor,
  FontColor,
  FontSize,
  GeneralHtmlSupport,
  Heading,
  Highlight,
  HorizontalLine,
  HtmlComment,
  HtmlEmbed,
  ImageBlock,
  ImageCaption,
  ImageInline,
  ImageInsert,
  ImageInsertViaUrl,
  ImageResize,
  ImageStyle,
  ImageTextAlternative,
  ImageToolbar,
  ImageUpload,
  Indent,
  IndentBlock,
  Italic,
  Link,
  LinkImage,
  List,
  ListProperties,
  MediaEmbed,
  PageBreak,
  Paragraph,
  PasteFromMarkdownExperimental,
  PasteFromOffice,
  RemoveFormat,
  SelectAll,
  SimpleUploadAdapter,
  Strikethrough,
  Style,
  Subscript,
  Superscript,
  Table,
  TableCaption,
  TableCellProperties,
  TableColumnResize,
  TableProperties,
  TableToolbar,
  TextTransformation,
  Underline,
  Undo,
} from 'ckeditor5';
import clsx from 'clsx';

export interface EditorErrorDetails {
  phase: 'initialization' | 'runtime';
  willEditorRestart?: boolean;
}

export type TEditorConfiguration = EditorConfig & {
  ywCodeBlock: IYwCodeBlockConfig;
};

export interface ICodeBlockData {
  language: string;
  code: string;
  value: string;
}

const editorConfiguration = {
  toolbar: {
    items: [
      'heading',
      'style',
      '|',
      'fontBackgroundColor',
      'fontColor',
      'fontSize',
      '|',
      'bold',
      'italic',
      'underline',
      'strikethrough',
      '|',
      'alignment',
      'bulletedList',
      'numberedList',
      'outdent',
      'indent',
      '|',
      'horizontalLine',
      'highlight',
      'code',
      'codeBlock',
      'blockQuote',
      '|',
      'link',
      'insertImage',
      'mediaEmbed',
      'insertTable',
      'htmlEmbed',
      '|',
      'findAndReplace',
      'removeFormat',
    ],
    shouldNotGroupWhenFull: true,
  },
  plugins: [
    // AccessibilityHelp,
    // Markdown,
    Alignment,
    AutoImage,
    AutoLink,
    Autoformat,
    Autosave,
    BlockQuote,
    BlockToolbar,
    Bold,
    Code,
    CodeBlock,
    DataFilter,
    DataSchema,
    Essentials,
    FindAndReplace,
    FontBackgroundColor,
    FontColor,
    FontSize,
    GeneralHtmlSupport,
    Heading,
    Highlight,
    HorizontalLine,
    HtmlComment,
    HtmlEmbed,
    ImageBlock,
    ImageCaption,
    ImageInline,
    ImageInsert,
    ImageInsertViaUrl,
    ImageResize,
    ImageStyle,
    ImageTextAlternative,
    ImageToolbar,
    ImageUpload,
    Indent,
    IndentBlock,
    Italic,
    Link,
    LinkImage,
    List,
    ListProperties,
    MediaEmbed,
    // Mention,
    PageBreak,
    Paragraph,
    PasteFromMarkdownExperimental,
    PasteFromOffice,
    RemoveFormat,
    SelectAll,
    SimpleUploadAdapter,
    Strikethrough,
    Style,
    Subscript,
    Superscript,
    Table,
    TableCaption,
    TableCellProperties,
    TableColumnResize,
    TableProperties,
    TableToolbar,
    TextTransformation,
    Underline,
    Undo,
  ],
  blockToolbar: [
    'bold',
    'italic',
    '|',
    'link',
    'insertImage',
    'insertTable',
    '|',
    'bulletedList',
    'numberedList',
    'outdent',
    'indent',
  ],
  htmlSupport: {
    allow: [
      {
        name: /^.*$/,
        styles: true,
        attributes: true,
        classes: true,
      },
    ],
  },
  link: {
    addTargetToExternalLinks: true,
    defaultProtocol: 'https://',
    decorators: {
      toggleDownloadable: {
        mode: 'manual',
        label: 'Downloadable',
        attributes: {
          download: 'file',
        },
      },
    },
  },
  list: {
    properties: {
      styles: true,
      startIndex: true,
      reversed: true,
    },
  },
  mention: {
    feeds: [
      {
        marker: '@',
        feed: [
          /* See: https://ckeditor.com/docs/ckeditor5/latest/features/mentions.html */
        ],
      },
    ],
  },
  style: {
    definitions: [
      {
        name: 'Marker',
        element: 'span',
        classes: ['marker'],
      },
      {
        name: 'Spoiler',
        element: 'span',
        classes: ['spoiler'],
      },
    ],
  },
  table: {
    contentToolbar: [
      'tableColumn',
      'tableRow',
      'mergeTableCells',
      'tableProperties',
      'tableCellProperties',
    ],
  },
  heading: {
    options: [
      { model: 'paragraph', title: 'Paragraph', class: '' },
      {
        model: 'heading1',
        view: { name: 'h1', classes: 'h1' },
        title: 'Heading 1',
        class: 'h1',
      },
      {
        model: 'heading2',
        view: { name: 'h2', classes: 'h2' },
        title: 'Heading 2',
        class: 'h2',
      },
      {
        model: 'heading3',
        view: { name: 'h3', classes: 'h3' },
        title: 'Heading 3',
        class: 'h3',
      },
      {
        model: 'heading4',
        view: { name: 'h4', classes: 'h4' },
        title: 'Heading 4',
        class: 'h4',
      },
      {
        model: 'heading5',
        view: { name: 'h5', classes: 'h5' },
        title: 'Heading 5',
        class: 'h5',
      },
      {
        model: 'heading6',
        view: { name: 'h6', classes: 'h6' },
        title: 'Heading 6',
        class: 'h6',
      },
    ],
  },
  fontSize: {
    options: [
      'default',
      {
        title: '40',
        model: '2.5rem',
      },
      {
        title: '32',
        model: '2rem',
      },
      {
        title: '28',
        model: '1.75rem',
      },
      {
        title: '24',
        model: '1.5rem',
      },
      {
        title: '20',
        model: '1.25rem',
      },
      {
        title: '16',
        model: '1rem',
      },
    ],
    supportAllValues: true,
  },
  fontColor: {
    colors: [
      'rgb(0, 0, 0)',
      'rgb(33, 37, 41)',
      'rgba(33, 37, 41, 0.75)',
      'rgba(33, 37, 41, 0.5)',
      'rgb(255, 255, 255)',
      'rgb(248, 249, 250)',
      'rgb(233, 236, 239)',
      'rgb(222, 226, 230)',
      'rgb(13, 110, 253)',
      'rgb(207, 226, 255)',
      'rgb(25, 135, 84)',
      'rgb(209, 231, 221)',
      'rgb(220, 53, 69)',
      'rgb(248, 215, 218)',
      'rgb(255, 193, 7)',
      'rgb(255, 243, 205)',
      'rgb(13, 202, 240)',
      'rgb(207, 244, 252)',
      'rgb(248, 249, 250)',
      'rgb(252, 252, 253)',
      'rgb(33, 37, 41)',
      'rgb(206, 212, 218)',
      'rgb(5, 44, 101)',
      'rgb(10, 54, 34)',
      'rgb(88, 21, 28)',
      'rgb(102, 77, 3)',
      'rgb(5, 81, 96)',
      'rgb(73, 80, 87)',
    ],
    columns: 9,
    documentColors: 18,
  },
  fontBackgroundColor: {
    colors: [
      'rgb(13, 110, 253)',
      'rgb(207, 226, 255)',
      'rgb(158, 197, 254)',
      'rgb(110, 168, 254)',
      'rgb(61, 139, 253)',
      'rgb(13, 110, 253)',
      'rgb(10, 88, 202)',
      'rgb(8, 66, 152)',
      'rgb(5, 44, 101)',
      'rgb(3, 22, 51)',
      'rgb(102, 16, 242)',
      'rgb(224, 207, 252)',
      'rgb(194, 159, 250)',
      'rgb(163, 112, 247)',
      'rgb(133, 64, 245)',
      'rgb(102, 16, 242)',
      'rgb(82, 13, 194)',
      'rgb(61, 10, 145)',
      'rgb(41, 6, 97)',
      'rgb(20, 3, 48)',
      'rgb(111, 66, 193)',
      'rgb(226, 217, 243)',
      'rgb(197, 179, 230)',
      'rgb(169, 142, 218)',
      'rgb(140, 104, 205)',
      'rgb(111, 66, 193)',
      'rgb(89, 53, 154)',
      'rgb(67, 40, 116)',
      'rgb(44, 26, 77)',
      'rgb(22, 13, 39)',
      'rgb(214, 51, 132)',
      'rgb(247, 214, 230)',
      'rgb(239, 173, 206)',
      'rgb(230, 133, 181)',
      'rgb(222, 92, 157)',
      'rgb(214, 51, 132)',
      'rgb(171, 41, 106)',
      'rgb(128, 31, 79)',
      'rgb(86, 20, 53)',
      'rgb(43, 10, 26)',
      'rgb(220, 53, 69)',
      'rgb(248, 215, 218)',
      'rgb(241, 174, 181)',
      'rgb(234, 134, 143)',
      'rgb(227, 93, 106)',
      'rgb(220, 53, 69)',
      'rgb(176, 42, 55)',
      'rgb(132, 32, 41)',
      'rgb(88, 21, 28)',
      'rgb(44, 11, 14)',
      'rgb(253, 126, 20)',
      'rgb(255, 229, 208)',
      'rgb(254, 203, 161)',
      'rgb(254, 178, 114)',
      'rgb(253, 152, 67)',
      'rgb(253, 126, 20)',
      'rgb(202, 101, 16)',
      'rgb(152, 76, 12)',
      'rgb(101, 50, 8)',
      'rgb(51, 25, 4)',
      'rgb(255, 193, 7)',
      'rgb(255, 243, 205)',
      'rgb(255, 230, 156)',
      'rgb(255, 218, 106)',
      'rgb(255, 205, 57)',
      'rgb(255, 193, 7)',
      'rgb(204, 154, 6)',
      'rgb(153, 116, 4)',
      'rgb(102, 77, 3)',
      'rgb(51, 39, 1)',
      'rgb(25, 135, 84)',
      'rgb(209, 231, 221)',
      'rgb(163, 207, 187)',
      'rgb(117, 183, 152)',
      'rgb(71, 159, 118)',
      'rgb(25, 135, 84)',
      'rgb(20, 108, 67)',
      'rgb(15, 81, 50)',
      'rgb(10, 54, 34)',
      'rgb(5, 27, 17)',
      'rgb(32, 201, 151)',
      'rgb(210, 244, 234)',
      'rgb(166, 233, 213)',
      'rgb(121, 223, 193)',
      'rgb(77, 212, 172)',
      'rgb(32, 201, 151)',
      'rgb(26, 161, 121)',
      'rgb(19, 121, 91)',
      'rgb(13, 80, 60)',
      'rgb(6, 40, 30)',
      'rgb(13, 202, 240)',
      'rgb(207, 244, 252)',
      'rgb(158, 234, 249)',
      'rgb(110, 223, 246)',
      'rgb(61, 213, 243)',
      'rgb(13, 202, 240)',
      'rgb(10, 162, 192)',
      'rgb(8, 121, 144)',
      'rgb(5, 81, 96)',
      'rgb(3, 40, 48)',
      'rgb(173, 181, 189)',
      'rgb(248, 249, 250)',
      'rgb(233, 236, 239)',
      'rgb(222, 226, 230)',
      'rgb(206, 212, 218)',
      'rgb(173, 181, 189)',
      'rgb(108, 117, 125)',
      'rgb(73, 80, 87)',
      'rgb(52, 58, 64)',
      'rgb(33, 37, 41)',
      'rgb(0, 0, 0)',
      'rgb(255, 255, 255)',
    ],
    columns: 9,
    documentColors: 18,
  },
  highlight: {
    options: [
      {
        model: 'Primary',
        class: 'bg-primary',
        title: 'Primary marker',
        color: '#0d6efd',
        type: 'marker',
      },
      {
        model: 'Secondary',
        class: 'bg-secondary',
        title: 'Secondary marker',
        color: '#6c757d',
        type: 'marker',
      },
      {
        model: 'Success',
        class: 'bg-success',
        title: 'Success marker',
        color: '#198754',
        type: 'marker',
      },
      {
        model: 'Danger',
        class: 'bg-danger',
        title: 'Danger marker',
        color: '#dc3545',
        type: 'marker',
      },
      {
        model: 'Warning',
        class: 'bg-warning',
        title: 'Warning marker',
        color: '#ffc008',
        type: 'marker',
      },
      {
        model: 'Info',
        class: 'bg-info',
        title: 'Info marker',
        color: '#11caf0',
        type: 'marker',
      },
      {
        model: 'Light',
        class: 'bg-light',
        title: 'Light marker',
        color: '#f8f9fa',
        type: 'marker',
      },
      {
        model: 'Dark',
        class: 'bg-dark',
        title: 'Dark marker',
        color: '#212529',
        type: 'marker',
      },
      {
        model: 'primaryPen',
        class: 'text-primary',
        title: 'Primary pen',
        color: '#0d6efd',
        type: 'pen',
      },
      {
        model: 'SecondaryPen',
        class: 'text-secondary',
        title: 'Secondary pen',
        color: '#6c757d',
        type: 'pen',
      },
      {
        model: 'SuccessPen',
        class: 'text-success',
        title: 'Success pen',
        color: '#198754',
        type: 'pen',
      },
      {
        model: 'DangerPen',
        class: 'text-danger',
        title: 'Danger pen',
        color: '#dc3545',
        type: 'pen',
      },
      {
        model: 'WarningPen',
        class: 'text-warning',
        title: 'Warning pen',
        color: '#ffc008',
        type: 'pen',
      },
      {
        model: 'InfoPen',
        class: 'text-info',
        title: 'Info pen',
        color: '#11caf0',
        type: 'pen',
      },
      {
        model: 'LightPen',
        class: 'text-light',
        title: 'Light pen',
        color: '#f8f9fa',
        type: 'pen',
      },
      {
        model: 'DarkPen',
        class: 'text-dark',
        title: 'Dark pen',
        color: '#212529',
        type: 'pen',
      },
    ],
  },
  image: {
    toolbar: [
      'toggleImageCaption',
      'imageTextAlternative',
      '|',
      'imageStyle:inline',
      'imageStyle:wrapText',
      'imageStyle:breakText',
      '|',
      'resizeImage',
    ],
    upload: {
      types: ['jpeg', 'png', 'gif'],
    },
  },
  simpleUpload: {
    uploadUrl: location.origin + '/api/files/images',
  },
};

export default function CustomEditor(props: {
  initialData?: string | null | undefined;
  onReady?: ((editor: ClassicEditor) => void) | undefined;
  onChange?:
    | ((event: EventInfo<string, unknown>, editor: ClassicEditor) => void)
    | undefined;
  onError?: ((error: Error, details: EditorErrorDetails) => void) | undefined;
  onBlur?:
    | ((event: EventInfo<string, unknown>, editor: ClassicEditor) => void)
    | undefined;
  onFocus?:
    | ((event: EventInfo<string, unknown>, editor: ClassicEditor) => void)
    | undefined;
}) {
  const { modal } = useContext(GlobalContext);
  const codeBlockData = new Map<string, ICodeBlockData>();
  const [isLayoutReady, setIsLayoutReady] = useState(false);
  const [isFocus, setIsFocus] = useState(false);

  useEffect(() => {
    setIsLayoutReady(true);
    return () => setIsLayoutReady(false);
  }, []);

  return (
    <div
      className={clsx('editor-container__editor', {
        'ms-4': isFocus,
      })}
    >
      {isLayoutReady && (
        <CKEditor
          editor={ClassicEditor}
          config={
            {
              ...editorConfiguration,
              ywCodeBlock: {
                open: (args) => {
                  const modalId = modal.current.show({
                    content: (
                      <CodeBlockCard
                        args={args}
                        data={codeBlockData}
                        closeModal={() => {
                          modal.current.hide(modalId);
                        }}
                      />
                    ),
                    centered: true,
                  });
                },
                renderer: ({ id, language, el, value }) => {
                  if (value) {
                    codeBlockData.set(id, {
                      language,
                      code: '',
                      value,
                    });

                    el.innerHTML = value;
                  } else {
                    const item = codeBlockData.get(id);
                    const _value = item?.value ?? '';
                    const _language = item?.language ?? '';
                    el.innerHTML = renderToString(
                      <CodeBlockPreview
                        key={id}
                        value={_value}
                        language={_language}
                      />,
                    );
                  }
                },
              },
            } as TEditorConfiguration
          }
          data={props.initialData}
          onChange={props.onChange}
          onReady={props.onReady}
          onError={props.onError}
          onBlur={props.onBlur}
          onFocus={(event, editor) => {
            setIsFocus(true);
            if (typeof props.onFocus === 'function') {
              props.onFocus(event, editor);
            }
          }}
        />
      )}
    </div>
  );
}
