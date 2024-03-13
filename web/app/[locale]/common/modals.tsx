import {
  forwardRef,
  ReactNode,
  useEffect,
  useImperativeHandle,
  useRef,
  useState,
} from 'react';
import { nanoid } from 'nanoid';

interface IModalProps {
  body: string | ReactNode;
  footer?: string | ReactNode;
  title?: string;
  backdrop?: boolean | 'static';
}

interface IModalPropsSuper extends IModalProps {
  id: string;
}

export interface IModalRef {
  show: (options: IModalProps) => void;
}

export default forwardRef(function Modals(props, ref) {
  const [modals, setModals] = useState<IModalPropsSuper[]>([]);

  useImperativeHandle(ref, () => ({
    show,
  }));

  function show(options: IModalProps) {
    const _modal: IModalPropsSuper = { ...options, id: nanoid() };
    if (modals.length > 11) {
      setModals([_modal]);
    } else {
      setModals([...modals, _modal]);
    }
  }

  return (
    <div>
      {modals.map((item) => {
        return <Modal key={item.id} item={item} />;
      })}
    </div>
  );
});

const Modal = ({ item }: { item: IModalPropsSuper }) => {
  const modalElementRef = useRef<HTMLDivElement>(null);
  const modalRef = useRef<any>(null);
  const title = item.title ?? 'Tips';
  const body = item.body ?? '';
  const footer = item.footer ?? '';
  const backdrop = item.backdrop ?? true;

  useEffect(() => {
    const current = modalElementRef.current;
    if (!current) {
      return;
    }

    let modal: any;
    import('bootstrap')
      .then((value) => {
        modal = value.Modal.getOrCreateInstance(current, { backdrop });
        modal.show();
      })
      .catch((reason) => {
        console.error(reason);
      });

    return () => {
      if (current) {
        modal?.dispose();
      }
    };
  }, []);

  return (
    <div
      ref={modalElementRef}
      className="modal"
      data-bs-backdrop={backdrop + ''}
      tabIndex={-1}
    >
      <div className="modal-dialog modal-dialog-centered modal-dialog-scrollable">
        <div className="modal-content">
          <div className="modal-header">
            <h5 className="modal-title">{title}</h5>
            <button
              type="button"
              className="btn-close"
              aria-label="Close"
              data-bs-dismiss="modal"
            ></button>
          </div>
          <div className="modal-body">{body}</div>
          <div className="modal-footer">{footer}</div>
        </div>
      </div>
    </div>
  );
};
