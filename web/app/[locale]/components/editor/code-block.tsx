import { type ChangeEvent, useState } from 'react';
import hljs from 'highlight.js';
import { nanoid } from 'nanoid';
import type { ICodeBlockData } from '@/app/[locale]/components/editor/editor';
import type { IOpenArgs } from '@/app/[locale]/components/editor/plugins/code-block/code-block';

const languages = [
  'C',
  'C#',
  'C++',
  'CSS',
  'Diff',
  'HTML',
  'Java',
  'JavaScript',
  'PHP',
  'Python',
  'Ruby',
  'Typescript',
  'XML',
];

export default function CodeBlock({
  args: { id: dataId, select, close },
  data,
  closeModal,
}: {
  args: IOpenArgs;
  data: Map<string, ICodeBlockData>;
  closeModal: () => void;
}) {
  let _language = '';
  let _code = '';
  let _value = '';
  if (dataId && data.has(dataId)) {
    const item = data.get(dataId)!;
    _language = item.language;

    if (item.code) {
      _code = item.code;
    } else {
      _value = item.value;
    }
  }

  const [form, setForm] = useState({
    language: _language,
    code: _code,
    value: _value,
  });
  const [errorMessage, setErrorMessage] = useState('');
  const [isEdit, setIsEdit] = useState(false);

  function onChangeForm(
    e: ChangeEvent<HTMLTextAreaElement | HTMLSelectElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  function onClickEdit() {
    setIsEdit(!isEdit);
  }

  function onClickConfirm() {
    const { language, code } = form;
    if (!code) {
      setErrorMessage('Please enter code');
      return;
    } else {
      setErrorMessage('');
    }

    let result;
    if (language) {
      result = hljs.highlight(code, {
        language,
      });
    } else {
      result = hljs.highlightAuto(code);
    }

    if (!result || result.value === '') {
      setErrorMessage('No data to generate');
      return;
    } else {
      setErrorMessage('');
    }

    const value = result.value;
    const id = nanoid();
    const _language = language.toLowerCase();
    data.set(id, {
      language: _language,
      code,
      value,
    });
    select({
      id,
      language: _language,
    });
    closeModal();
    close();
  }

  function onClickClose() {
    closeModal();
    close();
  }

  return (
    <div className="modal-content">
      <div className="modal-header">
        <div className="modal-title h5">Code Block</div>
      </div>
      <div className="modal-body">
        {errorMessage && (
          <div className="alert alert-danger" role="alert">
            {errorMessage}
          </div>
        )}

        <div className="input-group">
          <label className="input-group-text">Lang</label>
          <select
            onChange={onChangeForm}
            className="form-select"
            value={form.language}
            name="language"
            aria-label="language"
          >
            <option value="">Select...</option>
            {languages.map((item) => {
              return (
                <option key={item} value={item.toLowerCase()}>
                  {item}
                </option>
              );
            })}
          </select>
        </div>

        <div className="mt-3">
          <div className="input-group">
            <span className="input-group-text">Code</span>
            <textarea
              required
              value={form.code}
              onChange={onChangeForm}
              rows={9}
              className="form-control"
              aria-label="code"
              name="code"
            ></textarea>
          </div>

          {form.value && (
            <div
              className="mt-3"
              dangerouslySetInnerHTML={{ __html: form.value }}
            ></div>
          )}
        </div>
      </div>
      <div className="modal-footer">
        <div className="d-flex gap-2 justify-content-end">
          <button
            onClick={onClickClose}
            type="button"
            className="btn btn-secondary"
          >
            Close
          </button>
          <button
            onClick={onClickConfirm}
            type="button"
            className="btn btn-primary"
          >
            Confirm
          </button>
        </div>
      </div>
    </div>
  );
}
