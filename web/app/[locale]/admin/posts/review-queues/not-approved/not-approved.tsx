'use client';

import { useMutation } from '@tanstack/react-query';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Box from '@/app/[locale]/admin/common/box';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import NotApprovedPostReviewQueuesAction, {
  type INotApprovedPostReviewQueuesActionVariables,
} from '@/app/[locale]/actions/posts/review-queues/not-approved-post-review-queues-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function NotApproved({ id }: { id: number }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    reason: string;
  }>({
    reason: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts/review-queues',
    'Post Review Queues#NotApproved',
  );
  const t = useTranslations();

  const notApprovedPostReviewQueuesActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: INotApprovedPostReviewQueuesActionVariables;
    }) => {
      const response = await NotApprovedPostReviewQueuesAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onClickButton() {
    try {
      const variables = trimObjectStrings(form);
      await notApprovedPostReviewQueuesActionMutation.mutateAsync({
        id,
        variables,
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      notApprovedPostReviewQueuesActionMutation.reset();
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
            disabled={notApprovedPostReviewQueuesActionMutation.isSuccess}
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
              notApprovedPostReviewQueuesActionMutation.isPending ||
              notApprovedPostReviewQueuesActionMutation.isSuccess
            }
            type="button"
            className="btn btn-success"
          >
            {notApprovedPostReviewQueuesActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
