'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import DisableCommentReplyUserPostAction, {
  type IDisableCommentReplyUserPostActionVariables,
} from '@/app/[locale]/actions/posts/disable-comment-reply-user-post-action';
import type { IPost, IPostUser } from '@/app/[locale]/interfaces/posts';

export default function DisableCommentReply({
  post,
  userRelationship,
}: {
  post: IPost;
  userRelationship?: IPostUser;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    userId: string;
    disableComments: boolean;
    disableReplies: boolean;
    commentDisableReason: string;
    replyDisableReason: string;
  }>({
    userId: userRelationship?.user?.id ? userRelationship.user.id + '' : '',
    disableComments: userRelationship?.disableComments ?? false,
    disableReplies: userRelationship?.disableReplies ?? false,
    commentDisableReason: '',
    replyDisableReason: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts',
    'Posts#Update User Relationship',
  );
  const t = useTranslations();

  const disableCommentReplyUserPostActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      userId: number;
      variables: IDisableCommentReplyUserPostActionVariables;
    }) => {
      const response = await DisableCommentReplyUserPostAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const data = trimObjectStrings(form);
      if (!data.userId) {
        toast.current.show({
          type: 'danger',
          message: t('common.userIdFormText'),
        });
        return;
      }

      const id = post.id;
      await disableCommentReplyUserPostActionMutation.mutateAsync({
        id,
        userId: data.userId,
        variables: {
          disableComments: data.disableComments,
          disableReplies: data.disableReplies,
          commentDisableReason: data.commentDisableReason,
          replyDisableReason: data.replyDisableReason,
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      disableCommentReplyUserPostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChange(e: ChangeEvent<HTMLTextAreaElement | HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'disableComments' || name === 'disableReplies') {
      setForm({ ...form, [name]: value === 'true' });
    } else {
      setForm({ ...form, [name]: value });
    }
  }

  function onClickLabel(name: 'disableComments' | 'disableReplies') {
    setForm({ ...form, [name]: !form[name] });
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.userId')}</label>
          <input
            disabled={!!userRelationship}
            required
            type="text"
            className="form-control"
            name="userId"
            value={form.userId}
            onChange={onChange}
            aria-describedby="userId"
            minLength={1}
          />
          <div className="form-text">{t('common.userIdFormText')}</div>
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
              disableCommentReplyUserPostActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {disableCommentReplyUserPostActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
