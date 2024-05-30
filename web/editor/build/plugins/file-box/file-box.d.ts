import { Plugin } from '@ckeditor/ckeditor5-core';
import FileBoxEditing from './file-box-editing';
export default class FileBox extends Plugin {
    static get requires(): (typeof FileBoxEditing)[];
}
