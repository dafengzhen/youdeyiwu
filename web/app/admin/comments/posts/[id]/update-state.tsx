'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { convertToCamelCase } from '@/app/common/client';
import { IPostReviewState } from '@/app/interfaces/posts';
import { IComment, ICommentReviewState } from '@/app/interfaces/comments';
import { IReply, IReplyReviewState } from '@/app/interfaces/replies';
import UpdateStateCommentAction from '@/app/actions/comments/update-state-comment-action';
import UpdateStateReplyAction from '@/app/actions/replies/update-state-reply-action';

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

  const updateStateCommentActionMutation = useMutation({
    mutationFn: UpdateStateCommentAction,
  });
  const updateStateReplyActionMutation = useMutation({
    mutationFn: UpdateStateReplyAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const reviewState = form.reviewState;
      if (!reviewState) {
        toast.current.show({
          type: 'danger',
          message: 'Review state cannot be empty',
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
          message: 'Unable to update status, data does not exist',
        });
        return;
      }

      toast.current.show({
        type: 'success',
        message: 'State updated successfully',
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
    <Box title="Update State">
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">
            <span className="fw-bold text-danger">*</span>
            Review State
          </label>
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
          <div className="form-text">
            Select an review state, with the default state set to
            &apos;Approved&lsquo;
          </div>
        </div>

        <div>
          <label className="form-label">Reason</label>
          <textarea
            rows={2}
            className="form-control"
            name="reason"
            value={form.reason}
            onChange={onChangeForm}
            placeholder="Please enter the reason"
            aria-describedby="reason"
          />
          <div className="form-text">
            Please enter the reasons for setting this state, if available
          </div>
          <div className="form-text">
            The reason will be communicated to the user via a message
          </div>
        </div>

        <div>
          <button
            disabled={
              updateStateCommentActionMutation.isPending ||
              updateStateReplyActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateStateCommentActionMutation.isPending ||
            updateStateReplyActionMutation.isPending
              ? 'Updating'
              : 'Update State'}
          </button>
        </div>
      </form>
    </Box>
  );
}
