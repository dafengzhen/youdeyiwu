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

interface IModalProps {
  body: string | ReactNode;
  footer?: string | ReactNode;
  title?: string;
  backdrop?: boolean | 'static';
  keyboard?: boolean;
}

interface IModalPropsSuper extends IModalProps {
  id: string;
  ref: HTMLDivElement | null;
  displayed: boolean;
}

export interface IModalRef {
  show: (options: IModalProps) => void;
}

export default forwardRef(function Modals(props, ref) {
  const { bs } = useContext(GlobalContext);
  const [items, setItems] = useState<IModalPropsSuper[]>([]);

  useImperativeHandle(ref, () => ({
    show,
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
    setItems([
      {
        ...options,
        title: options.title ?? 'Message',
        backdrop: options.backdrop ?? true,
        id: nanoid(),
        ref: null,
        displayed: true,
      },
      ...items,
    ]);
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
            <div className="modal-dialog modal-dialog-scrollable">
              <div className="modal-content">
                <div className="modal-header">
                  <h5 className="modal-title">{item.title}</h5>
                  <button
                    type="button"
                    className="btn-close"
                    aria-label="Close"
                    data-bs-dismiss="modal"
                  ></button>
                </div>
                <div className="modal-body">{item.body}</div>
                <div className="modal-footer">{item.footer}</div>
              </div>
            </div>
          </div>
        );
      })}
    </div>
  );
});
