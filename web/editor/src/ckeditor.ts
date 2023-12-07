/**
 * @license Copyright (c) 2014-2023, CKSource Holding sp. z o.o. All rights reserved.
 * For licensing, see LICENSE.md or https://ckeditor.com/legal/ckeditor-oss-license
 */

// import { BalloonEditor } from '@ckeditor/ckeditor5-editor-balloon';
import { ClassicEditor } from '@ckeditor/ckeditor5-editor-classic';

import { Alignment } from '@ckeditor/ckeditor5-alignment';
import { Autoformat } from '@ckeditor/ckeditor5-autoformat';
import {
  Bold,
  Code,
  Italic,
  Strikethrough,
  Underline,
} from '@ckeditor/ckeditor5-basic-styles';
import { BlockQuote } from '@ckeditor/ckeditor5-block-quote';
import { CodeBlock } from '@ckeditor/ckeditor5-code-block';
import { Essentials } from '@ckeditor/ckeditor5-essentials';
import { FontColor, FontSize } from '@ckeditor/ckeditor5-font';
import { HorizontalLine } from '@ckeditor/ckeditor5-horizontal-line';
import { AutoImage, Image, ImageUpload } from '@ckeditor/ckeditor5-image';
import { Indent, IndentBlock } from '@ckeditor/ckeditor5-indent';
import { AutoLink, Link, LinkImage } from '@ckeditor/ckeditor5-link';
import {
  DocumentList,
  DocumentListProperties,
  TodoDocumentList,
} from '@ckeditor/ckeditor5-list';
import { MediaEmbed, MediaEmbedToolbar } from '@ckeditor/ckeditor5-media-embed';
import { PageBreak } from '@ckeditor/ckeditor5-page-break';
import { Paragraph } from '@ckeditor/ckeditor5-paragraph';
import { PasteFromOffice } from '@ckeditor/ckeditor5-paste-from-office';
import { RemoveFormat } from '@ckeditor/ckeditor5-remove-format';
import {
  Table,
  TableCaption,
  TableCellProperties,
  TableColumnResize,
  TableProperties,
  TableToolbar,
} from '@ckeditor/ckeditor5-table';
import { TextTransformation } from '@ckeditor/ckeditor5-typing';
import { Base64UploadAdapter } from '@ckeditor/ckeditor5-upload';
import { EditorWatchdog } from '@ckeditor/ckeditor5-watchdog';

// You can read more about extending the build with additional plugins in the "Installing plugins" guide.
// See https://ckeditor.com/docs/ckeditor5/latest/installation/plugins/installing-plugins.html for details.

class Editor extends ClassicEditor {
  public static override builtinPlugins = [
    Alignment,
    AutoImage,
    AutoLink,
    Autoformat,
    Base64UploadAdapter,
    BlockQuote,
    Bold,
    Code,
    CodeBlock,
    DocumentList,
    DocumentListProperties,
    Essentials,
    FontColor,
    FontSize,
    HorizontalLine,
    Image,
    ImageUpload,
    Indent,
    IndentBlock,
    Italic,
    Link,
    LinkImage,
    MediaEmbed,
    MediaEmbedToolbar,
    PageBreak,
    Paragraph,
    PasteFromOffice,
    RemoveFormat,
    Strikethrough,
    Table,
    TableCaption,
    TableCellProperties,
    TableColumnResize,
    TableProperties,
    TableToolbar,
    TextTransformation,
    TodoDocumentList,
    Underline,
  ];

  public static override defaultConfig = {
    toolbar: {
      items: [
        'bold',
        'italic',
        'link',
        'bulletedList',
        'numberedList',
        'removeFormat',
        '|',
        'indent',
        'outdent',
        'underline',
        'strikethrough',
        'alignment',
        'todoList',
        '|',
        'fontSize',
        'fontColor',
        'horizontalLine',
        'code',
        'codeBlock',
        '|',
        'imageUpload',
        'blockQuote',
        'insertTable',
        'mediaEmbed',
        'pageBreak',
      ],
    },
    language: 'en',
    table: {
      contentToolbar: [
        'tableColumn',
        'tableRow',
        'mergeTableCells',
        'tableCellProperties',
        'tableProperties',
      ],
    },
  };
}

export default { Editor, EditorWatchdog };
