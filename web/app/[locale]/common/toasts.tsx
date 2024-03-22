'use client';

import {
  forwardRef,
  type ReactNode,
  useEffect,
  useImperativeHandle,
  useRef,
  useState,
} from 'react';
import { nanoid } from 'nanoid';
import clsx from 'clsx';

export interface IToastProps {
  message: string | ReactNode;
  title?: string;
  subtitle?: string;
  autohide?: boolean;
  delay?: number;
  type?:
    | 'error'
    | 'danger'
    | 'primary'
    | 'success'
    | 'warning'
    | 'secondary'
    | 'info';
}

interface IToastPropsSuper extends IToastProps {
  id: string;
}

export interface IToastRef {
  show: (options: IToastProps) => void;
}

export default forwardRef(function Toasts(props, ref) {
  const [toasts, setToasts] = useState<IToastPropsSuper[]>([]);

  useImperativeHandle(ref, () => ({
    show,
  }));

  function show(options: IToastProps) {
    const _toast: IToastPropsSuper = { ...options, id: nanoid() };
    if (toasts.length > 11) {
      setToasts([_toast]);
    } else {
      setToasts([...toasts, _toast]);
    }
  }

  return (
    <div aria-live="polite" aria-atomic="true" className="position-relative">
      <div className="toast-container top-0 start-50 translate-middle-x p-3 overflow-x-hidden overflow-y-auto position-fixed vh-100">
        {toasts.map((item) => {
          return <Toast key={item.id} item={item} />;
        })}
      </div>
    </div>
  );
});

const Toast = ({ item }: { item: IToastPropsSuper }) => {
  const toastElementRef = useRef<HTMLDivElement>(null);
  const type = item.type ?? 'info';
  const title = item.title ?? 'Tips';
  const subtitle = item.title ?? '';
  const message = item.message ?? '';
  const autohide = item.autohide ?? true;
  const delay = item.delay ?? 5000;

  useEffect(() => {
    const current = toastElementRef.current;
    if (!current) {
      return;
    }

    let toast: any;
    import('bootstrap')
      .then((value) => {
        toast = value.Toast.getOrCreateInstance(current, {
          autohide,
          delay,
        });
        toast.show();
      })
      .catch((reason) => {
        console.error(reason);
      });

    return () => {
      if (current) {
        toast?.dispose();
      }
    };
  }, []);

  return (
    <div
      ref={toastElementRef}
      className="toast"
      role="alert"
      aria-live="assertive"
      aria-atomic="true"
    >
      <div className="toast-header">
        <div
          className={clsx('rounded me-2', {
            'bg-primary': type === 'info' || type === 'primary',
            'bg-danger': type === 'error' || type === 'danger',
            'bg-success': type === 'success',
            'bg-secondary': type === 'secondary',
            'bg-warning': type === 'warning',
          })}
          style={{ width: 20, height: 20 }}
        ></div>
        <strong className="me-auto">{title}</strong>
        <small className="text-body-secondary">{subtitle}</small>
        <button
          type="button"
          className="btn-close"
          data-bs-dismiss="toast"
          aria-label="Close"
        ></button>
      </div>
      <div className="toast-body">{message}</div>
    </div>
  );
};
