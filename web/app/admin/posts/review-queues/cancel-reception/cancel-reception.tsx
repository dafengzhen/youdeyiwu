'use client';

import { useMutation } from '@tanstack/react-query';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import Box from '@/app/admin/common/box';
import { trimObjectStrings } from '@/app/common/client';
import RefundPostReviewQueuesAction from '@/app/actions/posts/review-queues/refund-post-review-queues-action';

export default function CancelReception({ id }: { id: number }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    reason: string;
  }>({
    reason: '',
  });

  const refundPostReviewQueuesActionMutation = useMutation({
    mutationFn: RefundPostReviewQueuesAction,
  });

  async function onClickButton() {
    try {
      const variables = trimObjectStrings(form);
      await refundPostReviewQueuesActionMutation.mutateAsync({
        id,
        variables,
      });

      toast.current.show({
        type: 'success',
        message: 'Successfully canceled the request to review the post',
      });
    } catch (e: any) {
      refundPostReviewQueuesActionMutation.reset();
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
          <label className="form-label">
            The reason for cancellation of review
          </label>
          <textarea
            rows={3}
            disabled={refundPostReviewQueuesActionMutation.isSuccess}
            className="form-control"
            name="reason"
            value={form.reason}
            onChange={onChangeForm}
            placeholder="Please enter the reason"
            aria-describedby="reason"
          />
          <div className="form-text">
            The reason is optional and there can be several factors that lead to
            the cancellation of your review of the other party&apos;s post.
            However, it is still recommended that you explain it to the other
            party, as they have been waiting for your review and expecting a
            decision
          </div>
        </div>

        <div>
          <button
            onClick={onClickButton}
            disabled={
              refundPostReviewQueuesActionMutation.isPending ||
              refundPostReviewQueuesActionMutation.isSuccess
            }
            type="button"
            className="btn btn-success"
          >
            {refundPostReviewQueuesActionMutation.isPending
              ? 'Processing'
              : 'Cancel Reception'}
          </button>
        </div>
      </form>
    </Box>
  );
}
