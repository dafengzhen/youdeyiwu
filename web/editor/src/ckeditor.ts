/**
 * @license Copyright (c) 2014-2024, CKSource Holding sp. z o.o. All rights reserved.
 * For licensing, see LICENSE.md or https://ckeditor.com/legal/ckeditor-oss-license
 */

import {ClassicEditor} from '@ckeditor/ckeditor5-editor-classic';

import {Alignment} from '@ckeditor/ckeditor5-alignment';
import {Autoformat} from '@ckeditor/ckeditor5-autoformat';
import {Autosave} from '@ckeditor/ckeditor5-autosave';
import {Bold, Code, Italic, Strikethrough, Subscript, Superscript, Underline} from '@ckeditor/ckeditor5-basic-styles';
import {BlockQuote} from '@ckeditor/ckeditor5-block-quote';
import {CodeBlock} from '@ckeditor/ckeditor5-code-block';
import type {EditorConfig} from '@ckeditor/ckeditor5-core';
import {Essentials} from '@ckeditor/ckeditor5-essentials';
import {FindAndReplace} from '@ckeditor/ckeditor5-find-and-replace';
import {FontBackgroundColor, FontColor, FontSize} from '@ckeditor/ckeditor5-font';
import {Heading} from '@ckeditor/ckeditor5-heading';
import {Highlight} from '@ckeditor/ckeditor5-highlight';
import {HorizontalLine} from '@ckeditor/ckeditor5-horizontal-line';
import {HtmlEmbed} from '@ckeditor/ckeditor5-html-embed';
import {DataFilter, DataSchema, GeneralHtmlSupport, HtmlComment} from '@ckeditor/ckeditor5-html-support';
import {AutoImage, Image, ImageCaption, ImageInsert, ImageResize, ImageStyle, ImageToolbar, ImageUpload} from '@ckeditor/ckeditor5-image';
import {Indent, IndentBlock} from '@ckeditor/ckeditor5-indent';
import {AutoLink, Link, LinkImage} from '@ckeditor/ckeditor5-link';
import {List, ListProperties} from '@ckeditor/ckeditor5-list';
// import {Markdown} from '@ckeditor/ckeditor5-markdown-gfm';
import {MediaEmbed, MediaEmbedToolbar} from '@ckeditor/ckeditor5-media-embed';
import {PageBreak} from '@ckeditor/ckeditor5-page-break';
import {Paragraph} from '@ckeditor/ckeditor5-paragraph';
import {PasteFromOffice} from '@ckeditor/ckeditor5-paste-from-office';
import {RemoveFormat} from '@ckeditor/ckeditor5-remove-format';
import {SpecialCharacters, SpecialCharactersArrows, SpecialCharactersCurrency, SpecialCharactersEssentials, SpecialCharactersLatin, SpecialCharactersMathematical, SpecialCharactersText} from '@ckeditor/ckeditor5-special-characters';
import {Table, TableCaption, TableCellProperties, TableColumnResize, TableProperties, TableToolbar} from '@ckeditor/ckeditor5-table';
import {TextTransformation} from '@ckeditor/ckeditor5-typing';
import {Undo} from '@ckeditor/ckeditor5-undo';
import {Base64UploadAdapter} from '@ckeditor/ckeditor5-upload';
import {EditorWatchdog} from '@ckeditor/ckeditor5-watchdog';

// You can read more about extending the build with additional plugins in the "Installing plugins" guide.
// See https://ckeditor.com/docs/ckeditor5/latest/installation/plugins/installing-plugins.html for details.

class Editor extends ClassicEditor {
    public static override builtinPlugins = [
        Alignment,
        AutoImage,
        AutoLink,
        Autoformat,
        Autosave,
        Base64UploadAdapter,
        BlockQuote,
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
        Image,
        ImageCaption,
        ImageInsert,
        ImageResize,
        ImageStyle,
        ImageToolbar,
        ImageUpload,
        Indent,
        IndentBlock,
        Italic,
        Link,
        LinkImage,
        List,
        ListProperties,
        // Markdown,
        MediaEmbed,
        MediaEmbedToolbar,
        PageBreak,
        Paragraph,
        PasteFromOffice,
        RemoveFormat,
        SpecialCharacters,
        SpecialCharactersArrows,
        SpecialCharactersCurrency,
        SpecialCharactersEssentials,
        SpecialCharactersLatin,
        SpecialCharactersMathematical,
        SpecialCharactersText,
        Strikethrough,
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
        Undo
    ];

    public static override defaultConfig: EditorConfig = {
        toolbar: {
            items: [
                'heading',
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
                'indent',
                'outdent',
                'bulletedList',
                'numberedList',
                'alignment',
                '|',
                'horizontalLine',
                'highlight',
                'code',
                'codeBlock',
                'blockQuote',
                '|',
                'link',
                'imageInsert',
                'insertTable',
                'mediaEmbed',
                'htmlEmbed',
                '|',
                'findAndReplace',
                'removeFormat'
            ]
        },
        language: 'en',
        image: {
            toolbar: [
                'imageTextAlternative',
                'toggleImageCaption',
                'imageStyle:inline',
                'imageStyle:block',
                'imageStyle:side',
                'linkImage'
            ]
        },
        table: {
            contentToolbar: [
                'tableColumn',
                'tableRow',
                'mergeTableCells',
                'tableCellProperties',
                'tableProperties'
            ]
        }
    };
}

export default {Editor, EditorWatchdog};
