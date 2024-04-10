'use client';

import {
  forwardRef,
  type ReactNode,
  useContext,
  useEffect,
  useImperativeHandle,
  useState,
} from 'react';
import { nanoid } from 'nanoid';
import clsx from 'clsx';
import { GlobalContext } from '@/app/[locale]/contexts';

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
  ref: HTMLDivElement | null;
  displayed: boolean;
}

export interface IToastRef {
  show: (options: IToastProps) => void;
}

export default forwardRef(function Toasts(props, ref) {
  const { bs } = useContext(GlobalContext);
  const [items, setItems] = useState<IToastPropsSuper[]>([]);

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
        const instance = bootstrap.Toast.getOrCreateInstance(item.ref!, {
          autohide: item.autohide,
          delay: item.delay,
        });
        instance.show();
      });
  }, [bs, items]);

  function show(options: IToastProps) {
    setItems([
      {
        ...options,
        type: options.type ?? 'info',
        title: options.title ?? 'Message',
        autohide: options.autohide ?? true,
        delay: options.delay ?? 5000,
        id: nanoid(),
        ref: null,
        displayed: true,
      },
      ...items,
    ]);
  }

  return (
    <div className="toast-container top-0 start-50 translate-middle-x p-3 overflow-x-hidden overflow-y-auto position-fixed vh-100">
      {items.map((item) => {
        return (
          <div
            key={item.id}
            ref={(instance) => {
              item.ref = instance;
            }}
            className="toast"
            role="alert"
            aria-live="assertive"
            aria-atomic="true"
            tabIndex={-1}
          >
            <div className="toast-header">
              <div
                className={clsx('rounded me-2', {
                  'bg-primary': item.type === 'info' || item.type === 'primary',
                  'bg-danger': item.type === 'error' || item.type === 'danger',
                  'bg-success': item.type === 'success',
                  'bg-secondary': item.type === 'secondary',
                  'bg-warning': item.type === 'warning',
                })}
                style={{ width: 20, height: 20 }}
              ></div>
              <strong className="me-auto">{item.title}</strong>
              <small className="text-body-secondary">{item.subtitle}</small>
              <button
                type="button"
                className="btn-close"
                data-bs-dismiss="toast"
                aria-label="Close"
              ></button>
            </div>
            <div className="toast-body">{item.message}</div>
          </div>
        );
      })}
    </div>
  );
});
