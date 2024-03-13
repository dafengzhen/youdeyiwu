import Link from 'next/link';
import { type SyntheticEvent, useState } from 'react';
import Image from 'next/image';
import NoImage from '@/public/no-image.svg';
import type { IPost } from '@/app/[locale]/interfaces/posts';

export default function Cover({ item }: { item: IPost }) {
  const cover = item.cover;
  const [isError, setIsError] = useState(false);

  function onErrorCover(e: SyntheticEvent<HTMLImageElement, Event>) {
    setIsError(true);
  }

  if (!cover) {
    return <></>;
  }

  return (
    <div className="col">
      <div className="ratio ratio-16x9" style={{ width: 260, height: 195 }}>
        <Link href={`/posts/${item.id}`}>
          <Image
            onError={onErrorCover}
            className="rounded object-fit-cover image-hover cursor-pointer"
            width={260}
            height={195}
            src={isError ? NoImage : cover}
            alt="cover"
            title={isError ? 'Image loading error occurred' : 'cover'}
          />
        </Link>
      </div>
    </div>
  );
}
