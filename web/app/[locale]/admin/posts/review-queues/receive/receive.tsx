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
import { useTranslations } from 'next-intl';

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
  const t = useTranslations();

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
          message: t('common.estimatedCompletionTimeFormText'),
        });
        return;
      }

      await receivePostReviewQueuesActionMutation.mutateAsync({
        latestReviewResultTime: vars.latestReviewResultTime,
        postId,
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
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
            {t('common.estimatedCompletionTime')}
          </label>
          <input
            disabled={receivePostReviewQueuesActionMutation.isSuccess}
            type="date"
            className="form-control"
            name="latestReviewResultTime"
            value={form.latestReviewResultTime}
            onChange={onChangeForm}
            aria-describedby="latestReviewResultTime"
          />
          <div className="form-text">
            {t('common.estimatedCompletionTimeFormText')}
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
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
