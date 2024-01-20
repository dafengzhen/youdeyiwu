import { IPost } from '@/app/interfaces/posts';
import Nodata from '@/app/common/nodata';
import Link from 'next/link';

export default function RelatedPosts({
  randomData = [],
}: {
  randomData?: IPost[];
}) {
  return (
    <>
      {randomData.length > 0 && (
        <div className="card border-0 shadow-sm shadow-hover">
          <div className="card-header bg-transparent border-bottom-0 fw-bold">
            Related Articles
          </div>
          <div className="card-body p-0">
            {randomData.map((item) => {
              return (
                <div
                  key={item.id}
                  className="card border-0 cursor-pointer card-hover"
                >
                  <Link
                    className="link-body-emphasis text-decoration-none"
                    href={`/posts/${item.id}`}
                    scroll={false}
                  >
                    <div className="card-body py-2">
                      <div className="line-clamp-2">{item.name}</div>
                    </div>
                  </Link>
                </div>
              );
            })}

            {randomData.length === 0 && <Nodata />}
          </div>
        </div>
      )}
    </>
  );
}
