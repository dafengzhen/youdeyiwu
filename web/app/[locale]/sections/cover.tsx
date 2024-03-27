import type { ISection } from '@/app/[locale]/interfaces/sections';
import Link from 'next/link';
import { type SyntheticEvent, useState } from 'react';
import Image from 'next/image';

export default function Cover({ item }: { item: ISection }) {
  const cover = item.cover;
  const [isError, setIsError] = useState(false);

  function onErrorCover(e: SyntheticEvent<HTMLImageElement, Event>) {
    setIsError(true);
  }

  if (!cover) {
    return <></>;
  }

  return (
    <div className="ratio ratio-16x9 p-3" style={{ width: 260, height: 195 }}>
      <Link href={`/sections/${item.id}`} scroll={false}>
        <Image
          onError={onErrorCover}
          className="rounded-top object-fit-cover image-hover cursor-pointer"
          width={260}
          height={195}
          src={cover}
          alt="cover"
          title={isError ? 'Image loading error occurred' : 'cover'}
        />
      </Link>
    </div>
  );
}
