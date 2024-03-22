'use client';

import { useMutation } from '@tanstack/react-query';
import ReceivePostReviewQueuesAction, {
  type IReceivePostReviewQueuesActionVariables,
} from '@/app/[locale]/actions/posts/review-queues/receive-post-review-queues-action';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Box from '@/app/[locale]/admin/common/box';
import {
  formatCurrentDateTime,
  trimObjectStrings,
} from '@/app/[locale]/common/client';
import { isBefore } from 'date-fns';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function Receive({ postId }: { postId: number }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    latestReviewResultTime: string;
  }>({
    latestReviewResultTime: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts/review-queues',
    'Post Review Queues#Receive',
  );

  const receivePostReviewQueuesActionMutation = useMutation({
    mutationFn: async (variables: IReceivePostReviewQueuesActionVariables) => {
      const response = await ReceivePostReviewQueuesAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onClickButton() {
    try {
      const vars = trimObjectStrings(form);
      const currentDate = formatCurrentDateTime(new Date(), 'yyyy-MM-dd');
      if (!vars.latestReviewResultTime) {
        vars.latestReviewResultTime = currentDate;
        setForm({ ...form, latestReviewResultTime: currentDate });
      }

      if (isBefore(vars.latestReviewResultTime, currentDate)) {
        toast.current.show({
          type: 'danger',
          message:
            'The estimated completion date for the review should not be earlier than the current date, but it is also not recommended to complete the review too late',
        });
        return;
      }

      await receivePostReviewQueuesActionMutation.mutateAsync({
        latestReviewResultTime: vars.latestReviewResultTime,
        postId,
      });

      toast.current.show({
        type: 'success',
        message:
          'Successful receipt of the moderation request for this post. Thank you for your effort in reviewing this post',
      });
    } catch (e: any) {
      receivePostReviewQueuesActionMutation.reset();
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
            Estimated time for completing the review
          </label>
          <input
            disabled={receivePostReviewQueuesActionMutation.isSuccess}
            type="date"
            className="form-control"
            name="latestReviewResultTime"
            value={form.latestReviewResultTime}
            onChange={onChangeForm}
            placeholder="Please enter the date"
            aria-describedby="latestReviewResultTime"
          />
          <div className="form-text">
            If the estimated time for completing the review is not provided, it
            will default to the current date
          </div>
          <div className="form-text">
            The estimated completion date for the review should not be earlier
            than the current date, but it is also not recommended to complete
            the review too late
          </div>
        </div>

        <div>
          <button
            onClick={onClickButton}
            disabled={
              isActionDisabled ||
              receivePostReviewQueuesActionMutation.isPending ||
              receivePostReviewQueuesActionMutation.isSuccess
            }
            type="button"
            className="btn btn-success"
          >
            {receivePostReviewQueuesActionMutation.isPending
              ? 'Processing'
              : 'Receive'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
