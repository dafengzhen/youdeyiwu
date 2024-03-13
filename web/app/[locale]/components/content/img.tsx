import Image from 'next/image';
import Link from 'next/link';
import clsx from 'clsx';

export default function Img({
  value,
  className,
}: {
  value: HTMLImageElement;
  className?: string;
}) {
  return (
    <Link
      href={value.src}
      data-pswp-width={value.width}
      data-pswp-height={value.height}
      target="_blank"
    >
      <Image
        className={clsx(className, 'img-fluid')}
        src={value.src}
        alt={value.alt ?? 'image'}
        width={value.width}
        height={value.height}
      />
    </Link>
  );
}
