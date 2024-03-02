'use client';

import { useMutation } from '@tanstack/react-query';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import Box from '@/app/admin/common/box';
import { trimObjectStrings } from '@/app/common/client';
import ApprovedPostReviewQueuesAction, {
  type IApprovedPostReviewQueuesActionVariables,
} from '@/app/actions/posts/review-queues/approved-post-review-queues-action';

export default function Approved({ id }: { id: number }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    reason: string;
  }>({
    reason: '',
  });

  const approvedPostReviewQueuesActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IApprovedPostReviewQueuesActionVariables;
    }) => {
      const response = await ApprovedPostReviewQueuesAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onClickButton() {
    try {
      const variables = trimObjectStrings(form);
      await approvedPostReviewQueuesActionMutation.mutateAsync({
        id,
        variables,
      });

      toast.current.show({
        type: 'success',
        message: 'Thank you, your request to review the post has been approved',
      });
    } catch (e: any) {
      approvedPostReviewQueuesActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onSubmit(e: FormEvent<HTMLFormElement>) {
    e.stopPropagation();
    e.preventDefault();
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Reason for approval</label>
          <textarea
            rows={3}
            disabled={approvedPostReviewQueuesActionMutation.isSuccess}
            className="form-control"
            name="reason"
            value={form.reason}
            onChange={onChangeForm}
            placeholder="Please enter the reason"
            aria-describedby="reason"
          />
          <div className="form-text">
            Optional. If desired, you can provide a reason for approving the
            post to encourage the person to keep up the good work
          </div>
        </div>

        <div>
          <button
            onClick={onClickButton}
            disabled={
              approvedPostReviewQueuesActionMutation.isPending ||
              approvedPostReviewQueuesActionMutation.isSuccess
            }
            type="button"
            className="btn btn-success"
          >
            {approvedPostReviewQueuesActionMutation.isPending
              ? 'Processing'
              : 'Cancel Reception'}
          </button>
        </div>
      </form>
    </Box>
  );
}
