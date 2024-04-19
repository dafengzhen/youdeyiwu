import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import { type ChangeEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { PostIdContext } from '@/app/[locale]/contexts/postid';
import {
  getUserAlias,
  isHttpOrHttps,
  trimObjectStrings,
} from '@/app/[locale]/common/client';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import CreateCommentAction, {
  type ICreateCommentActionVariables,
} from '@/app/[locale]/actions/comments/create-comment-action';
import { sanitizeHtmlContent } from '@/app/[locale]/common/editor';
import Link from 'next/link';
import Image from 'next/image';
import { useTranslations } from 'next-intl';
import usePointsAlert from '@/app/[locale]/hooks/use-points-alert ';
import QueryRootConfigAction from '@/app/[locale]/actions/configs/root/query-root-config-action';

export default function CommentBox({ details }: { details: IPostDetails }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState({
    content: '',
  });
  const { openReplyBox, setOpenReplyBox, currentUser } =
    useContext(PostIdContext);
  let avatar;
  let alias = getUserAlias(currentUser);
  const t = useTranslations();
  let acronyms;
  let acronymsTitle;
  const pointsAlert = usePointsAlert();
  const queryClient = useQueryClient();

  if (currentUser) {
    avatar = isHttpOrHttps(currentUser.avatar) ? currentUser.avatar : undefined;

    if (
      details.section &&
      details.section.admins.find((item) => item.id === currentUser.id)
    ) {
      acronyms = 'Admin';

      if (
        currentUser.sections.find((item) => item.id === details.section!.id)
      ) {
        acronymsTitle = t('common.thisUserIsASectionAdministrator');
      } else {
        acronymsTitle = t('common.thisUserIsTheCurrentSectionAdministrator');
      }
    }

    if (currentUser.root) {
      acronyms = 'Forum Admin';
      acronymsTitle = t('common.thisUserIsAForumAdministrator');
    }

    if (currentUser.id === details.createdBy) {
      acronyms = 'Author';
      acronymsTitle = t('common.thisUserIsTheAuthorOfThisArticle');
    }
  }

  const queryRootConfigActionQuery = useQuery({
    queryKey: ['/configs/root', 'disableAnonymous'],
    queryFn: async () => {
      const response = await QueryRootConfigAction({
        disableAnonymous: true,
      });

      if (response.isError) {
        throw response;
      }

      return response.data;
    },
  });

  const createCommentActionMutation = useMutation({
    mutationFn: async (variables: ICreateCommentActionVariables) => {
      const response = await CreateCommentAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  function onClickCancelReplyBox() {
    setOpenReplyBox!(!openReplyBox);
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
          message: t('common.theReplyCannotBeEmpty'),
        });
        return;
      }

      const _content = sanitizeHtmlContent(content);
      const id = details.id;
      await createCommentActionMutation.mutateAsync({
        postId: id,
        content: _content,
      });
      setForm({ ...form, content: '' });
      setOpenReplyBox!(false);

      queryClient.refetchQueries({
        queryKey: [`/posts/${details.id}/comment-reply`, 'infinite'],
      });

      pointsAlert.refresh();

      toast.current.show({
        type: 'success',
        message: t('common.replySuccessfully'),
      });
    } catch (e: any) {
      createCommentActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <>
      {openReplyBox && (
        <div className="card rounded-4 border shadow">
          <div className="card-header d-flex gap-2 justify-content-between bg-transparent border-bottom-0">
            <div className="d-flex justify-content-around gap-3">
              <Link href={currentUser ? `/users/${currentUser.id}` : '/users'}>
                <Image
                  className="rounded-circle object-fit-contain image-hover"
                  src={avatar ?? '/assets/avatar.png'}
                  alt="avatar"
                  width={50}
                  height={50}
                />
              </Link>
              <div className="d-flex flex-column justify-content-around flex-grow-1">
                <div>
                  <Link
                    className="fw-medium link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                    href={currentUser ? `/users/${currentUser.id}` : '/users'}
                  >
                    {alias}
                  </Link>
                </div>
                <time className="fw-normal text-body-secondary small">
                  {t('common.now')}
                </time>
              </div>
            </div>

            {acronyms && (
              <div className="align-self-center">
                <div
                  title={acronymsTitle}
                  className="small border border-1 border-secondary px-2 rounded-pill"
                >
                  {acronyms}
                </div>
              </div>
            )}
          </div>
          <div className="card-body d-flex flex-column gap-3 py-2">
            <div>
              <textarea
                disabled={createCommentActionMutation.isPending}
                autoFocus={true}
                onChange={onChangeForm}
                name="content"
                value={form.content}
                className="form-control mt-2"
                rows={4}
              ></textarea>

              {queryRootConfigActionQuery.data?.disableAnonymousComments &&
              !currentUser ? (
                <div className="form-text text-danger">
                  {t('common.disableAnonymousCommentsMessage')}
                </div>
              ) : (
                <div className="form-text">{t('common.rulesForReplying')}</div>
              )}
            </div>
            <div className="d-flex justify-content-end gap-2">
              <div className="d-flex gap-2">
                <button
                  disabled={createCommentActionMutation.isPending}
                  onClick={onClickCancelReplyBox}
                  className="btn btn-link link-secondary text-decoration-none"
                  type="button"
                >
                  <span className="">{t('common.cancel')}</span>
                </button>

                {queryRootConfigActionQuery.data?.disableAnonymousComments &&
                !currentUser ? (
                  <button
                    disabled
                    className="btn btn-success rounded-pill"
                    type="button"
                  >
                    <i className="bi bi-send-check me-2"></i>
                    <span>{t('common.sendReply')}</span>
                  </button>
                ) : (
                  <button
                    disabled={createCommentActionMutation.isPending}
                    onClick={onClickSendReply}
                    className="btn btn-success rounded-pill"
                    type="button"
                  >
                    <i className="bi bi-send-check me-2"></i>
                    <span>
                      {createCommentActionMutation.isPending
                        ? t('common.replying')
                        : t('common.sendReply')}
                    </span>
                  </button>
                )}
              </div>
            </div>
          </div>
        </div>
      )}
    </>
  );
}
