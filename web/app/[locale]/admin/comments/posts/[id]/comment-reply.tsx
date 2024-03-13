'use client';

import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import { PostIdContext } from '@/app/[locale]/contexts/postid';
import Navbar from '@/app/[locale]/posts/[id]/navbar';
import EmptyBox from '@/app/[locale]/home/empty-box';
import type { IUser } from '@/app/[locale]/interfaces/users';
import Comments from '@/app/[locale]/admin/comments/posts/[id]/comments/comments';
import Box from '@/app/[locale]/admin/common/box';

export default function CommentReply({
  details,
  currentUser,
}: {
  details: IPostDetails;
  currentUser: IUser | null;
}) {
  return (
    <PostIdContext.Provider value={{ details, currentUser }}>
      <Box>
        <div className="d-flex flex-column gap-4">
          <Navbar details={details} />
          <Comments details={details} />
          <EmptyBox />
        </div>
      </Box>
    </PostIdContext.Provider>
  );
}
