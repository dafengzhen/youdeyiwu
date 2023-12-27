'use client';

import type { IPostDetails } from '@/app/interfaces/posts';
import { PostIdContext } from '@/app/contexts/postid';
import Navbar from '@/app/posts/[id]/navbar';
import EmptyBox from '@/app/home/empty-box';
import type { IUser } from '@/app/interfaces/users';
import Comments from '@/app/admin/comments/posts/[id]/comments/comments';
import Box from '@/app/admin/common/box';

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
