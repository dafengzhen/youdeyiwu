'use client';

import { useMutation } from '@tanstack/react-query';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Box from '@/app/[locale]/admin/common/box';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import RefundPostReviewQueuesAction, {
  type IRefundPostReviewQueuesActionVariables,
} from '@/app/[locale]/actions/posts/review-queues/refund-post-review-queues-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Return({ id }: { id: number }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    reason: string;
  }>({
    reason: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts/review-queues',
    'Post Review Queues#Return',
  );
  const t = useTranslations();

  const refundPostReviewQueuesActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IRefundPostReviewQueuesActionVariables;
    }) => {
      const response = await RefundPostReviewQueuesAction(variables);
      if (response.isError) {
        throw response;
      }
    },
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
        message: t('common.successfulUpdate'),
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
          <label className="form-label">{t('common.reason')}</label>
          <textarea
            rows={3}
            disabled={refundPostReviewQueuesActionMutation.isSuccess}
            className="form-control"
            name="reason"
            value={form.reason}
            onChange={onChangeForm}
            aria-describedby="reason"
          />
          <div className="form-text">{t('common.reasonFormText')}</div>
        </div>

        <div>
          <button
            onClick={onClickButton}
            disabled={
              isActionDisabled ||
              refundPostReviewQueuesActionMutation.isPending ||
              refundPostReviewQueuesActionMutation.isSuccess
            }
            type="button"
            className="btn btn-success"
          >
            {refundPostReviewQueuesActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
