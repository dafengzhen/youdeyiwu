'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import { getUserAlias, trimObjectStrings } from '@/app/[locale]/common/client';
import type { IUser } from '@/app/[locale]/interfaces/users';
import DisableCommentReplyUserAction, {
  type IDisableCommentReplyUserActionVariables,
} from '@/app/[locale]/actions/users/disable-comment-reply-user-action';

export default function DisableCommentReply({ user }: { user: IUser }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    noPostingAllowed: boolean;
    disableComments: boolean;
    disableReplies: boolean;
    commentDisableReason: string;
    replyDisableReason: string;
    noPostingReason: string;
  }>({
    noPostingAllowed: user.noPostingAllowed ?? false,
    disableComments: user.disableComments ?? false,
    disableReplies: user.disableReplies ?? false,
    commentDisableReason: '',
    replyDisableReason: '',
    noPostingReason: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/users',
    'Users#Disable Comment Reply',
  );
  const t = useTranslations();

  const disableCommentReplyUserActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IDisableCommentReplyUserActionVariables;
    }) => {
      const response = await DisableCommentReplyUserAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings(form);
      const id = user.id;
      await disableCommentReplyUserActionMutation.mutateAsync({
        id,
        variables,
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      disableCommentReplyUserActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChange(e: ChangeEvent<HTMLTextAreaElement | HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;

    if (
      name === 'noPostingAllowed' ||
      name === 'disableComments' ||
      name === 'disableReplies'
    ) {
      setForm({ ...form, [name]: value === 'true' });
    } else {
      setForm({ ...form, [name]: value });
    }
  }

  function onClickLabel(
    name: 'noPostingAllowed' | 'disableComments' | 'disableReplies',
  ) {
    setForm({ ...form, [name]: !form[name] });
  }

  return (
    <Box title={`${getUserAlias(user)} (ID. ${user.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div className="card">
          <div className="card-body">
            <div className="form-check form-check-inline">
              <input
                className="form-check-input"
                type="checkbox"
                value={form.noPostingAllowed ? 'true' : 'false'}
                checked={form.noPostingAllowed}
                onChange={onChange}
              />
              <label
                onClick={() => onClickLabel('noPostingAllowed')}
                className="cursor-pointer form-check-label text-capitalize user-select-none"
              >
                {t('common.disableArticleCreation')}
              </label>
            </div>

            <div className="mt-4">
              <label className="form-label"> {t('common.disableReason')}</label>
              <textarea
                rows={2}
                className="form-control"
                name="noPostingReason"
                value={form.noPostingReason}
                onChange={onChange}
                aria-describedby="noPostingReason"
              />
              <div className="form-text">
                {t('common.disableReasonFormText')}
              </div>
            </div>
          </div>
        </div>

        <div className="card">
          <div className="card-body">
            <div className="form-check form-check-inline">
              <input
                className="form-check-input"
                type="checkbox"
                value={form.disableComments ? 'true' : 'false'}
                checked={form.disableComments}
                onChange={onChange}
              />
              <label
                onClick={() => onClickLabel('disableComments')}
                className="cursor-pointer form-check-label text-capitalize user-select-none"
              >
                {t('common.disableComment')}
              </label>
            </div>

            <div className="mt-4">
              <label className="form-label"> {t('common.disableReason')}</label>
              <textarea
                rows={2}
                className="form-control"
                name="commentDisableReason"
                value={form.commentDisableReason}
                onChange={onChange}
                aria-describedby="commentDisableReason"
              />
              <div className="form-text">
                {t('common.disableReasonFormText')}
              </div>
            </div>
          </div>
        </div>

        <div className="card">
          <div className="card-body">
            <div className="form-check form-check-inline">
              <input
                className="form-check-input"
                type="checkbox"
                value={form.disableReplies ? 'true' : 'false'}
                checked={form.disableReplies}
                onChange={onChange}
              />
              <label
                onClick={() => onClickLabel('disableReplies')}
                className="cursor-pointer form-check-label text-capitalize user-select-none"
              >
                {t('common.disableReply')}
              </label>
            </div>

            <div className="mt-4">
              <label className="form-label"> {t('common.disableReason')}</label>
              <textarea
                rows={2}
                className="form-control"
                name="replyDisableReason"
                value={form.replyDisableReason}
                onChange={onChange}
                aria-describedby="replyDisableReason"
              />
              <div className="form-text">
                {t('common.disableReasonFormText')}
              </div>
            </div>
          </div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled ||
              disableCommentReplyUserActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {disableCommentReplyUserActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
