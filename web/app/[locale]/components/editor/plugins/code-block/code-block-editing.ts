import { Plugin, toWidget, ViewNode, Widget } from 'ckeditor5';
import CodeBlockCommand from './code-block-command';
import { IYwCodeBlockConfig } from './code-block';

export default class CodeBlockEditing extends Plugin {
  static get requires() {
    return [Widget];
  }

  init() {
    this._defineConfig();
    this._defineSchema();
    this._defineConverters();
    this._defineCommand();
  }

  private _defineConfig() {
    this.editor.config.define('ywCodeBlock', {
      open: () => {},
      renderer: () => {},
    } as IYwCodeBlockConfig);
  }

  private _defineCommand() {
    const codeBlockCommand = new CodeBlockCommand(this.editor);
    this.editor.commands.add('ywCodeBlock', codeBlockCommand);
  }

  private _defineSchema() {
    const schema = this.editor.model.schema;
    schema.register('ywCodeBlock', {
      isObject: true,
      allowWhere: '$block',
      allowAttributes: ['id', 'value', 'language'],
    });
  }

  private _defineConverters() {
    const editor = this.editor;
    const conversion = editor.conversion;
    const config = this.editor.config.get('ywCodeBlock') as IYwCodeBlockConfig;

    editor.data.registerRawContentMatcher({
      name: 'div',
      classes: 'yw-code-block-wrapper',
    });
    conversion.for('upcast').elementToElement({
      view: {
        name: 'div',
        classes: 'yw-code-block',
      },
      model: (viewElement, { writer: modelWriter }) => {
        const wrapperElement = viewElement.getChild(0);
        if (!wrapperElement || !wrapperElement.is('element')) {
          return null;
        }

        const value = wrapperElement.getCustomProperty('$rawContent');
        const id = viewElement.getAttribute('data-id');
        const language = viewElement.getAttribute('data-language');
        return modelWriter.createElement('ywCodeBlock', {
          id,
          value,
          language,
        });
      },
    });
    conversion.for('downcast').elementToElement({
      model: 'ywCodeBlock',
      view: (modelElement, { writer: viewWriter }) => {
        const id = modelElement.getAttribute('id') as string;
        const language = modelElement.getAttribute('language') as string;

        const value = modelElement.getAttribute('value') as
          | string
          | undefined
          | null;

        const div = viewWriter.createContainerElement('div', {
          class: 'yw-code-block',
          'data-id': id,
          'data-language': language,
        });

        const elWrapper = viewWriter.createRawElement(
          'div',
          {
            class: 'yw-code-block-wrapper',
          },
          (el) => {
            if (typeof config.renderer === 'function') {
              config.renderer({ id, language, el, value });
            } else {
              el.innerHTML = 'No callback function configured';
            }
          },
        );

        viewWriter.setCustomProperty('id', id, div);
        viewWriter.insert(viewWriter.createPositionAt(div, 0), elWrapper);

        return toWidget(div, viewWriter, {
          label: 'code block widget',
        });
      },
    });
  }

  private _getAllTextContent(node: ViewNode) {
    let text = '';
    if (node.is('element')) {
      for (const childNode of node.getChildren()) {
        text += this._getAllTextContent(childNode);
      }
    } else if (node.is('$text')) {
      text += node.data;
    }
    return text;
  }

  private _getAllTextNodes(element: HTMLElement | ChildNode) {
    let textNodes: any[] = [];

    if (element.nodeType === Node.TEXT_NODE) {
      textNodes.push(element);
    } else {
      const childNodes = element.childNodes;
      for (let i = 0; i < childNodes.length; i++) {
        var child = childNodes[i];
        var childTextNodes = this._getAllTextNodes(child);
        textNodes = textNodes.concat(childTextNodes);
      }
    }

    return textNodes;
  }

  private _getAllText(
    element: HTMLElement | ChildNode,
    attr: 'textContent' | 'nodeValue' | 'data' | 'wholeText',
  ) {
    let textNodes = this._getAllTextNodes(element);
    let allText = '';

    for (let i = 0; i < textNodes.length; i++) {
      const textNode = textNodes[i];
      allText += textNode[attr];
    }

    return allText;
  }
}
