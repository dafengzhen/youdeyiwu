import type { IPostDetails } from '@/app/interfaces/posts';
import { useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { PostIdContext } from '@/app/contexts/postid';
import clsx from 'clsx';
import { useMutation } from '@tanstack/react-query';
import LikePostAction from '@/app/actions/posts/like-post-action';
import FavoritePostAction from '@/app/actions/posts/favorite-post-action';
import { formatCount, wait } from '@/app/common/client';
import RewardBox from '@/app/posts/[id]/reward-box';

export default function ActionButton({ details }: { details: IPostDetails }) {
  const { toast, modal } = useContext(GlobalContext);
  const { openReplyBox, setOpenReplyBox, currentUser } =
    useContext(PostIdContext);
  const [copying, setCopying] = useState(false);
  const [rewarding, setRewarding] = useState(false);
  const [openRewardBox, setOpenRewardBox] = useState(false);
  const [likeProcessing, setLikeProcessing] = useState(false);
  const [favouriteProcessing, setFavouriteProcessing] = useState(false);

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
          message:
            'Sorry, anonymous users cannot participate in like counts. Thank you for your support!',
        });
        return;
      }

      const id = details.id;
      await likePostActionMutation.mutateAsync({ id });

      let message;
      if (details.liked) {
        message = 'Like successful, awesome!';
      } else {
        message =
          'The removal of the like was successful. Waiting for your support again!';
      }

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
          message:
            'Sorry, anonymous users cannot add to favorites. You may need to log in. Thank you for your bookmark!',
        });
        return;
      }

      const id = details.id;
      await favoritePostActionMutation.mutateAsync({ id });

      let message;
      if (details.bookmarked) {
        message = 'Added to favorites successfully, well done';
      } else {
        message =
          'The removal from favorites was successful. Looking forward to adding it again!';
      }

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
                ? 'Processing'
                : 'Like'}
            </span>
            <i className="bi bi-hand-thumbs-up"></i>

            {details.likesCount > 0 && (
              <span className="position-absolute top-0 start-100 translate-middle badge rounded-pill bg-primary">
                <span>{formatCount(details.likesCount)}</span>
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
            className="btn rounded-pill btn-outline-primary"
          >
            <span className="me-2">
              {favouriteProcessing || favoritePostActionMutation.isPending
                ? 'Processing'
                : 'Favourite'}
            </span>
            <i className="bi bi-star"></i>

            {details.favoritesCount > 0 && (
              <span className="position-absolute top-0 start-100 translate-middle badge rounded-pill bg-primary">
                <span>{formatCount(details.favoritesCount)}</span>
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
              'Cancel Reply'
            ) : (
              <>
                <span className="me-2">Reply</span>
                <i className="bi bi-send"></i>
              </>
            )}
          </button>
        </div>
        <div className="d-flex justify-content-center gap-4">
          <button
            disabled={rewarding}
            onClick={onClickReward}
            type="button"
            className={clsx(
              'btn rounded-pill',
              openRewardBox ? 'btn-outline-secondary' : 'btn-outline-primary',
            )}
          >
            <span className="me-2">
              {rewarding
                ? 'Rewarding'
                : openRewardBox
                  ? 'Cancel Reward'
                  : 'Reward'}
            </span>
            <i className="bi bi-currency-yen"></i>
          </button>
          <button
            disabled={copying}
            onClick={onClickShare}
            type="button"
            className={clsx(
              'btn rounded-pill',
              copying ? 'btn-outline-secondary' : 'btn-outline-primary',
            )}
          >
            <span className="me-2">{copying ? 'Copying' : 'Share'}</span>
            <i className="bi bi-share"></i>
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
