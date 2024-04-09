import Link from 'next/link';
import Image from 'next/image';
import type { IPost } from '@/app/[locale]/interfaces/posts';

export default function Cover({ item }: { item: IPost }) {
  const cover = item.cover;
  if (!cover) {
    return;
  }

  return (
    <div className="col">
      <div
        className="ratio ratio-16x9"
        style={{ width: 260, height: 195, maxWidth: 330 }}
      >
        <Link href={`/posts/${item.id}`}>
          <Image
            className="rounded object-fit-contain image-hover cursor-pointer"
            width={260}
            height={195}
            src={cover}
            alt="cover"
          />
        </Link>
      </div>
    </div>
  );
}
