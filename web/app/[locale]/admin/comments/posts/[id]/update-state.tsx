'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { convertToCamelCase } from '@/app/[locale]/common/client';
import type { IPostReviewState } from '@/app/[locale]/interfaces/posts';
import type {
  IComment,
  ICommentReviewState,
} from '@/app/[locale]/interfaces/comments';
import type {
  IReply,
  IReplyReviewState,
} from '@/app/[locale]/interfaces/replies';
import UpdateStateCommentAction, {
  type IUpdateStateCommentActionVariables,
} from '@/app/[locale]/actions/comments/update-state-comment-action';
import UpdateStateReplyAction, {
  type IUpdateStateReplyActionVariables,
} from '@/app/[locale]/actions/replies/update-state-reply-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateStates({
  details,
  cid,
  rid,
}: {
  details: IComment | IReply;
  cid?: number;
  rid?: number;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    reason: string;
    reviewState: ICommentReviewState | IReplyReviewState;
  }>({
    reason: '',
    reviewState: details.reviewState ?? 'APPROVED',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/comments',
    'Comments#Update State',
  );
  const t = useTranslations();

  const updateStateCommentActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateStateCommentActionVariables;
    }) => {
      const response = await UpdateStateCommentAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });
  const updateStateReplyActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateStateReplyActionVariables;
    }) => {
      const response = await UpdateStateReplyAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const reviewState = form.reviewState;
      if (!reviewState) {
        toast.current.show({
          type: 'danger',
          message: t('common.reviewStateCannotBeEmpty'),
        });
        return;
      }

      const reason = form.reason.trim();
      const id = details.id;
      if (cid) {
        await updateStateCommentActionMutation.mutateAsync({
          id,
          variables: {
            reviewState,
            reason,
          },
        });
      } else if (rid) {
        await updateStateCommentActionMutation.mutateAsync({
          id,
          variables: {
            reviewState,
            reason,
          },
        });
      } else {
        toast.current.show({
          type: 'danger',
          message: t('common.failedUpdate'),
        });
        return;
      }

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      if (cid) {
        updateStateCommentActionMutation.reset();
      } else if (rid) {
        updateStateReplyActionMutation.reset();
      }

      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onClickReviewState(value: IPostReviewState) {
    setForm({
      ...form,
      reviewState: value,
    });
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <Box title={t('common.updateState')}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.reviewState')}</label>
          <div>
            {(
              ['APPROVED', 'REJECTED', 'PENDING_REVIEW'] as IPostReviewState[]
            ).map((item) => {
              return (
                <div key={item} className="form-check form-check-inline">
                  <input
                    required
                    className="form-check-input"
                    type="radio"
                    name="reviewState"
                    value={item}
                    checked={item === form.reviewState}
                    onChange={(event) =>
                      onClickReviewState(event.target.value as IPostReviewState)
                    }
                  />
                  <label
                    onClick={() => onClickReviewState(item)}
                    className="cursor-pointer form-check-label text-capitalize user-select-none"
                  >
                    {convertToCamelCase(item, ' ')}
                  </label>
                </div>
              );
            })}
          </div>
        </div>

        <div>
          <label className="form-label">{t('common.reason')}</label>
          <textarea
            rows={2}
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
            disabled={
              isActionDisabled ||
              updateStateCommentActionMutation.isPending ||
              updateStateReplyActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateStateCommentActionMutation.isPending ||
            updateStateReplyActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
