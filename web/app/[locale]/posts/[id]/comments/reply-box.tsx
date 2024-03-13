import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import type { IUser } from '@/app/[locale]/interfaces/users';
import type { IComment } from '@/app/[locale]/interfaces/comments';
import type { IReply } from '@/app/[locale]/interfaces/replies';
import { type ChangeEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import CreateReplyAction, {
  type ICreateReplyActionVariables,
} from '@/app/[locale]/actions/replies/create-reply-action';
import { getUserAlias, trimObjectStrings } from '@/app/[locale]/common/client';
import { sanitizeHtmlContent } from '@/app/[locale]/common/editor';

export default function ReplyBox({
  details,
  onClickReply,
  replyingUser,
  isComment = true,
  item,
}: {
  details: IPostDetails;
  onClickReply: () => void;
  replyingUser: IUser | undefined;
  isComment?: boolean;
  item: IComment | IReply;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState({
    content: '',
  });

  const createReplyActionMutation = useMutation({
    mutationFn: async (variables: ICreateReplyActionVariables) => {
      const response = await CreateReplyAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  function onClickCancelReplyBox() {
    onClickReply();
  }

  function onChangeForm(e: ChangeEvent<HTMLTextAreaElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  async function onClickSendReply() {
    try {
      const { content } = trimObjectStrings({ ...form });
      if (!content) {
        toast.current.show({
          type: 'danger',
          message: 'The reply content cannot be empty',
        });
        return;
      }

      const _content = sanitizeHtmlContent(content);
      const id = item.id;

      const variables: ICreateReplyActionVariables = {
        postId: details.id,
        content: _content,
      };
      if (isComment) {
        variables.commentId = id;
      } else {
        variables.replyId = id;
      }

      await createReplyActionMutation.mutateAsync(variables);
      setForm({ ...form, content: '' });

      toast.current.show({
        type: 'success',
        message: 'Awesome, a reply has been posted',
      });

      onClickReply();
    } catch (e: any) {
      createReplyActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <div className="card border-0 shadow-sm">
      <div className="card-body d-flex flex-column gap-3 py-2">
        <div>
          {replyingUser && (
            <label className="form-label text-secondary">
              <span>Quoting the reply from user</span>
              <span>&nbsp;</span>
              <span className="fw-medium">@{getUserAlias(replyingUser)}</span>
            </label>
          )}

          <textarea
            disabled={createReplyActionMutation.isPending}
            autoFocus={true}
            onChange={onChangeForm}
            name="content"
            value={form.content}
            className="form-control mt-2"
            rows={4}
            placeholder="Please enter your reply content"
          ></textarea>
          <div className="form-text">
            Please make sure to adhere to the rules of the current website when
            replying. Have a great time!
          </div>
        </div>
        <div className="d-flex justify-content-end gap-2">
          <div className="d-flex gap-2">
            <button
              disabled={createReplyActionMutation.isPending}
              onClick={onClickCancelReplyBox}
              className="btn btn-link link-secondary text-decoration-none"
              type="button"
            >
              <span className="">Cancel</span>
            </button>

            <button
              disabled={createReplyActionMutation.isPending}
              onClick={onClickSendReply}
              className="btn btn-success rounded-pill"
              type="button"
            >
              <i className="bi bi-send-check me-2"></i>
              <span className="">
                {createReplyActionMutation.isPending
                  ? 'Replying'
                  : 'Send Reply'}
              </span>
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
