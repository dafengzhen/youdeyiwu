import hljs from '@/app/common/highlight';
import clsx from 'clsx';

export default function Code({ value }: { value: HTMLElement }) {
  const code = hljs.highlightAuto(value.textContent ?? '').value;

  return (
    <code
      className={clsx(value.className, 'hljs')}
      dangerouslySetInnerHTML={{ __html: code }}
    />
  );
}
