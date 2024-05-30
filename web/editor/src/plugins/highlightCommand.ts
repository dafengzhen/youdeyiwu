import { Command } from 'ckeditor5/src/core';

export class HighlightCommand extends Command {
  override refresh() {
    // Handle state.
    const { document, schema } = this.editor.model;

    // Check if selection is already highlighted.
    this.value = document.selection.getAttribute('highlight');

    // Check if command is allowed on current selection.
    this.isEnabled = schema.checkAttributeInSelection(
      document.selection,
      'highlight',
    );
  }

  override execute() {
    // Command logic.

    const model = this.editor.model;
    const selection = model.document.selection;
    const newValue = !this.value;

    model.change((writer) => {
      if (!selection.isCollapsed) {
        const ranges = model.schema.getValidRanges(
          selection.getRanges(),
          'highlight',
        );

        for (const range of ranges) {
          if (newValue) {
            writer.setAttribute('highlight', newValue, range);
          } else {
            writer.removeAttribute('highlight', range);
          }
        }
      }

      if (newValue) {
        return writer.setSelectionAttribute('highlight', true);
      }

      return writer.removeSelectionAttribute('highlight');
    });
  }
}
