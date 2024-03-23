import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import { useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { PostIdContext } from '@/app/[locale]/contexts/postid';
import clsx from 'clsx';
import { useMutation } from '@tanstack/react-query';
import LikePostAction from '@/app/[locale]/actions/posts/like-post-action';
import FavoritePostAction from '@/app/[locale]/actions/posts/favorite-post-action';
import { formatCount, wait } from '@/app/[locale]/common/client';
import RewardBox from '@/app/[locale]/posts/[id]/reward-box';
import usePointsAlert from '@/app/[locale]/hooks/use-points-alert ';
import { useTranslations } from 'next-intl';

export default function ActionButton({ details }: { details: IPostDetails }) {
  const { toast, modal } = useContext(GlobalContext);
  const { openReplyBox, setOpenReplyBox, currentUser } =
    useContext(PostIdContext);
  const [copying, setCopying] = useState(false);
  const [rewarding, setRewarding] = useState(false);
  const [openRewardBox, setOpenRewardBox] = useState(false);
  const [likeProcessing, setLikeProcessing] = useState(false);
  const [favouriteProcessing, setFavouriteProcessing] = useState(false);
  const [liked, setLiked] = useState(details.liked ?? false);
  const [likesCount, setLikesCount] = useState(details.likesCount);
  const [favorited, setFavorited] = useState(details.favorited ?? false);
  const [favoritesCount, setFavoritesCount] = useState(details.favoritesCount);
  const pointsAlert = usePointsAlert();
  const t = useTranslations();

  const likePostActionMutation = useMutation({
    mutationFn: async (variables: { id: number | string }) => {
      const response = await LikePostAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });
  const favoritePostActionMutation = useMutation({
    mutationFn: async (variables: { id: number | string }) => {
      const response = await FavoritePostAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onClickLike() {
    if (likeProcessing) {
      return;
    }
    setLikeProcessing(true);

    try {
      if (!currentUser) {
        await wait();
        toast.current.show({
          type: 'danger',
          message: t('common.likeBtn.anonymousUsers'),
        });
        return;
      }

      const id = details.id;
      await likePostActionMutation.mutateAsync({ id });

      let message;
      if (!liked) {
        message = t('common.likeBtn.gets');
        setLiked(true);
        setLikesCount(likesCount + 1);
      } else {
        setLiked(false);
        setLikesCount(likesCount - 1);
      }

      pointsAlert.refresh();

      toast.current.show({
        type: 'success',
        message,
      });
    } catch (e: any) {
      likePostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    } finally {
      setLikeProcessing(false);
    }
  }

  async function onClickFavourite() {
    if (favouriteProcessing) {
      return;
    }
    setFavouriteProcessing(true);

    try {
      if (!currentUser) {
        await wait();
        toast.current.show({
          type: 'danger',
          message: t('common.favouriteBtn.anonymousUsers'),
        });
        return;
      }

      const id = details.id;
      await favoritePostActionMutation.mutateAsync({ id });

      let message;
      if (!favorited) {
        message = t('common.favouriteBtn.gets');
        setFavorited(true);
        setFavoritesCount(favoritesCount + 1);
      } else {
        setFavorited(false);
        setFavoritesCount(favoritesCount - 1);
      }

      pointsAlert.refresh();

      toast.current.show({
        type: 'success',
        message,
      });
    } catch (e: any) {
      favoritePostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    } finally {
      setFavouriteProcessing(false);
    }
  }

  function onClickReward() {
    setOpenRewardBox(!openRewardBox);
  }

  function onClickShare() {
    if (copying) {
      return;
    }

    setCopying(true);
    const textToCopy = `[${details.name}](${location.origin}/posts/${details.id})`;
    navigator.clipboard
      .writeText(textToCopy)
      .then(async () => {
        await wait(500);
      })
      .then(() => {
        toast.current.show({
          type: 'success',
          message: 'Successfully copied to clipboard',
        });
      })
      .catch((e: any) => {
        console.error(e);
        toast.current.show({
          type: 'danger',
          message:
            'Unable to copy to clipboard: ' + e.message ?? 'unknown error',
        });
      })
      .finally(() => {
        setCopying(false);
      });
  }

  function onClickReply() {
    setOpenReplyBox!(!openReplyBox);
  }

  return (
    <div className="my-5">
      <div className="d-flex justify-content-center flex-column gap-3">
        <div className="d-flex justify-content-center gap-4">
          <button
            disabled={likeProcessing || likePostActionMutation.isPending}
            onClick={onClickLike}
            type="button"
            className="btn rounded-pill btn-outline-primary position-relative"
          >
            <span className="me-2">
              {likeProcessing || likePostActionMutation.isPending
                ? t('common.processing')
                : t('common.likeBtn.text')}
            </span>
            <i
              className={clsx(
                'bi',
                liked ? 'bi-hand-thumbs-up-fill' : 'bi-hand-thumbs-up',
              )}
            ></i>

            {likesCount > 0 && (
              <span className="position-absolute top-0 start-100 translate-middle badge rounded-pill bg-primary">
                <span>{formatCount(likesCount)}</span>
                <span className="visually-hidden">likes</span>
              </span>
            )}
          </button>
          <button
            disabled={
              favouriteProcessing || favoritePostActionMutation.isPending
            }
            onClick={onClickFavourite}
            type="button"
            className="btn rounded-pill btn-outline-primary position-relative"
          >
            <span className="me-2">
              {favouriteProcessing || favoritePostActionMutation.isPending
                ? t('common.processing')
                : t('common.favouriteBtn.text')}
            </span>
            <i
              className={clsx('bi', favorited ? 'bi-star-fill' : 'bi-star')}
            ></i>

            {favoritesCount > 0 && (
              <span className="position-absolute top-0 start-100 translate-middle badge rounded-pill bg-primary">
                <span>{formatCount(favoritesCount)}</span>
                <span className="visually-hidden">favorites</span>
              </span>
            )}
          </button>
          <button
            onClick={onClickReply}
            type="button"
            className={clsx(
              'btn rounded-pill',
              openReplyBox ? 'btn-outline-secondary' : 'btn-outline-primary',
            )}
          >
            {openReplyBox ? (
              t('common.replyBtn.cancelReply')
            ) : (
              <>
                <span className="me-2">{t('common.replyBtn.text')}</span>
                <i className="bi bi-send"></i>
              </>
            )}
          </button>
        </div>
      </div>

      {openRewardBox && (
        <RewardBox
          details={details}
          onClickReward={onClickReward}
          rewarding={rewarding}
          setRewarding={setRewarding}
        />
      )}
    </div>
  );
}
