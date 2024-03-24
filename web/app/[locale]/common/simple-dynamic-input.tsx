import {
  type ChangeEvent,
  type Dispatch,
  type SetStateAction,
  useEffect,
  useState,
} from 'react';
import { nanoid } from 'nanoid';
import { useTranslations } from 'next-intl';

interface IValue {
  id: string;
  value: string;
}

export default function SimpleDynamicInput({
  items = [],
  setItems = () => {},
  useTextarea = false,
  showSourceInfo,
}: {
  items: string[];
  setItems: Dispatch<SetStateAction<string[]>>;
  useTextarea?: boolean;
  showSourceInfo?: { id: number; name: string }[];
}) {
  const [values, setValues] = useState<IValue[]>(
    items.map((item) => ({ id: nanoid(), value: item })),
  );
  const t = useTranslations();

  useEffect(() => {
    setItems(values.map((item) => item.value));
  }, [values]);

  function onClickAdd() {
    setValues([...values, { id: nanoid(), value: '' }]);
  }

  function onClickDelete(item: IValue) {
    setValues(values.filter((value) => value.id !== item.id));
  }

  function onClickPop() {
    values.pop();
    setValues([...values]);
  }

  function onChange(
    item: IValue,
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const find = values.find((value) => value.id === item.id);
    if (find) {
      find.value = e.target.value;
      setValues([...values]);
    }
  }

  return (
    <div className="d-flex flex-column gap-3">
      {showSourceInfo && (
        <div className="d-flex gap-2">
          {showSourceInfo.map((item) => {
            return (
              <div key={item.id}>
                {item.name}&nbsp;(ID.&nbsp;
                <span className="fw-bold">{item.id}</span>)
              </div>
            );
          })}
        </div>
      )}
      <div className="d-flex flex-column gap-2">
        {values.map((item) => {
          return (
            <div className="input-group" key={item.id}>
              {useTextarea ? (
                <textarea
                  rows={1}
                  autoFocus={!item.value}
                  className="form-control"
                  value={item.value}
                  name="value"
                  onChange={(event) => onChange(item, event)}
                />
              ) : (
                <input
                  autoFocus={!item.value}
                  type="text"
                  className="form-control"
                  value={item.value}
                  name="value"
                  onChange={(event) => onChange(item, event)}
                />
              )}
              <button
                type="button"
                onClick={() => onClickDelete(item)}
                className="btn btn-sm btn-outline-secondary"
              >
                <i className="bi bi-dash-lg me-2"></i>
                <span>{t('common.del')}</span>
              </button>
            </div>
          );
        })}
      </div>
      <div className="d-flex gap-2">
        <div>
          <button
            type="button"
            onClick={onClickAdd}
            className="btn btn-sm btn-secondary"
          >
            <i className="bi bi-plus-lg me-2"></i>
            <span>{t('common.add')}</span>
          </button>
        </div>
        {values.length > 0 && (
          <div>
            <button
              type="button"
              onClick={onClickPop}
              className="btn btn-sm btn-secondary"
            >
              <i className="bi bi-dash-lg me-2"></i>
              <span>{t('common.del')}</span>
            </button>
          </div>
        )}
      </div>
    </div>
  );
}
