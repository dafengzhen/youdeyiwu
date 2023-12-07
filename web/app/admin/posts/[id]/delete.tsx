'use client';

import Box from '@/app/admin/common/box';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/contexts';
import Link from 'next/link';
import RefreshAction from '@/app/actions/refresh-action';
import { IPost } from '@/app/interfaces/posts';
import DeletePostAction from '@/app/actions/posts/delete-post-action';

export default function Delete({ post }: { post: IPost }) {
  const { toast } = useContext(GlobalContext);

  const deletePostActionMutation = useMutation({
    mutationFn: DeletePostAction,
  });
  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });

  async function onClickDelete() {
    try {
      const id = post.id;
      await deletePostActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/posts',
          tags: ['/admin/posts', `/admin/posts/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deletePostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box>
      <div className="alert alert-danger" role="alert">
        <h4 className="alert-heading">
          <span className="me-2 text-danger">Delete</span>
          <Link
            target="_blank"
            href={`/posts/${post.id}`}
            className="fw-bold link-danger link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
          >
            {post.name}&nbsp;(ID. {post.id})
          </Link>
        </h4>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the post will be deleted.
          </li>
          <li>
            Please proceed with caution when performing deletion, as what you
            may actually want to do is an update operation.
          </li>
        </ul>
        <hr />
        <p className="mb-0">
          After pressing the delete button, the processing will begin. Please
          wait patiently for the deletion to be completed.
        </p>
        <div className="mt-4">
          <button
            onClick={onClickDelete}
            disabled={deletePostActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deletePostActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
        </div>
      </div>
    </Box>
  );
}
