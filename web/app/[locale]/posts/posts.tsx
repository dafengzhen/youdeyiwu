'use client';

import EmptyBox from '@/app/[locale]/home/empty-box';
import { IPage } from '@/app/[locale]/interfaces';
import { IPost } from '@/app/[locale]/interfaces/posts';
import Articles from '@/app/[locale]/home/posts';

export default function Posts({ data }: { data: IPage<IPost[]> }) {
  return (
    <div className="row mx-0">
      <div className="col">
        <div className="d-flex flex-column gap-4">
          <Articles data={data} />
          <EmptyBox />
        </div>
      </div>
    </div>
  );
}
