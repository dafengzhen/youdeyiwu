import {
  forwardRef,
  type ReactNode,
  useContext,
  useEffect,
  useImperativeHandle,
  useState,
} from 'react';
import { nanoid } from 'nanoid';
import { GlobalContext } from '@/app/[locale]/contexts';
import clsx from 'clsx';

interface IModalProps {
  content?: string | ReactNode;
  body?: string | ReactNode;
  footer?: string | ReactNode;
  title?: string;
  backdrop?: boolean | 'static';
  keyboard?: boolean;
  centered?: boolean;
  btnClose?: boolean;
}

interface IModalPropsSuper extends IModalProps {
  id: string;
  ref: HTMLDivElement | null;
  displayed: boolean;
}

export interface IModalRef {
  show: (options: IModalProps) => string;
  hide: (id: string) => void;
}

export default forwardRef(function Modals(props, ref) {
  const { bs } = useContext(GlobalContext);
  const [items, setItems] = useState<IModalPropsSuper[]>([]);

  useImperativeHandle(ref, () => ({
    show,
    hide,
  }));

  useEffect(() => {
    const bootstrap = bs.current;
    if (!bootstrap) {
      return;
    }

    items
      .filter((item) => item.displayed && item.ref)
      .forEach((item) => {
        item.displayed = false;
        const instance = bootstrap.Modal.getOrCreateInstance(item.ref!, {
          backdrop: item.backdrop,
        });
        instance.show();
      });
  }, [bs, items]);

  function show(options: IModalProps) {
    const id = nanoid();
    setItems([
      {
        ...options,
        title: options.title ?? 'Message',
        backdrop: options.backdrop ?? true,
        id,
        ref: null,
        displayed: true,
      },
      ...items,
    ]);
    return id;
  }

  function hide(id: string) {
    const bootstrap = bs.current;
    const item = items.find((item) => item.id === id);
    if (!bootstrap || !item) {
      return;
    }

    const instance = bootstrap.Modal.getOrCreateInstance(item.ref!, {
      backdrop: item.backdrop,
    });
    instance.hide();
  }

  return (
    <div>
      {items.map((item) => {
        return (
          <div
            key={item.id}
            ref={(instance) => {
              item.ref = instance;
            }}
            className="modal fade"
            data-bs-backdrop={item.backdrop === 'static' ? item.backdrop : ''}
            data-bs-keyboard={item.keyboard === true ? 'true' : 'false'}
            tabIndex={-1}
          >
            <div
              className={clsx('modal-dialog modal-dialog-scrollable', {
                'modal-dialog-centered': item.centered,
              })}
            >
              {item.content ? (
                item.content
              ) : (
                <div className="modal-content">
                  <div className="modal-header">
                    <h5 className="modal-title">{item.title}</h5>

                    {item.btnClose && (
                      <button
                        type="button"
                        className="btn-close"
                        aria-label="Close"
                        data-bs-dismiss="modal"
                      ></button>
                    )}
                  </div>

                  <div className="modal-body">{item.body}</div>

                  {item.footer && (
                    <div className="modal-footer">{item.footer}</div>
                  )}
                </div>
              )}
            </div>
          </div>
        );
      })}
    </div>
  );
});
