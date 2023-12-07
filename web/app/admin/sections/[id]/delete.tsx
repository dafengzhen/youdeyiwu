'use client';

import Box from '@/app/admin/common/box';
import type { ISection } from '@/app/interfaces/sections';
import { useMutation } from '@tanstack/react-query';
import { useContext } from 'react';
import { GlobalContext } from '@/app/contexts';
import Link from 'next/link';
import DeleteSectionAction from '@/app/actions/sections/delete-section-action';
import RefreshAction from '@/app/actions/refresh-action';

export default function Delete({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);

  const deleteSectionActionMutation = useMutation({
    mutationFn: DeleteSectionAction,
  });
  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });

  async function onClickDelete() {
    try {
      const id = section.id;
      await deleteSectionActionMutation.mutateAsync({ id });

      toast.current.show({
        type: 'success',
        message: 'Deleted Successfully, Refresh after 2 seconds',
      });

      setTimeout(() => {
        refreshActionMutation.mutateAsync({
          url: '/admin/sections',
          tags: ['/admin/sections', `/admin/sections/${id}`],
        });
      }, 2000);
    } catch (e: any) {
      deleteSectionActionMutation.reset();
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
            href={`/sections/${section.id}`}
            className="fw-bold link-danger link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
          >
            {section.name}&nbsp;(ID. {section.id})
          </Link>
        </h4>
        <ul className="list-unstyled fw-medium">
          <li>
            Irreversible deletion! All data related to the section will be
            deleted.
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
            disabled={deleteSectionActionMutation.isPending}
            type="button"
            className="btn btn-sm btn-danger"
          >
            {deleteSectionActionMutation.isPending ? 'Deleting' : 'Delete'}
          </button>
        </div>
      </div>
    </Box>
  );
}
