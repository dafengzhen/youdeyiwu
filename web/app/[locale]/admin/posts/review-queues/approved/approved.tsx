'use client';

import { useMutation } from '@tanstack/react-query';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Box from '@/app/[locale]/admin/common/box';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import ApprovedPostReviewQueuesAction, {
  type IApprovedPostReviewQueuesActionVariables,
} from '@/app/[locale]/actions/posts/review-queues/approved-post-review-queues-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Approved({ id }: { id: number }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    reason: string;
  }>({
    reason: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts/review-queues',
    'Post Review Queues#Approved',
  );
  const t = useTranslations();

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
        message: t('common.successfulUpdate'),
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
          <label className="form-label">{t('common.reason')}</label>
          <textarea
            rows={3}
            disabled={approvedPostReviewQueuesActionMutation.isSuccess}
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
              approvedPostReviewQueuesActionMutation.isPending ||
              approvedPostReviewQueuesActionMutation.isSuccess
            }
            type="button"
            className="btn btn-success"
          >
            {approvedPostReviewQueuesActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
