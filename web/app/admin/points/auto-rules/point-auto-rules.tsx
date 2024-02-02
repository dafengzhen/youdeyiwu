'use client';

import Box from '@/app/admin/common/box';
import Nodata from '@/app/common/nodata';
import { AutoRuleNameEnum, IPointAutoRule } from '@/app/interfaces/points';
import { useContext, useState } from 'react';
import clsx from 'clsx';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SaveAutoRulesPointsAction from '@/app/actions/points/auto-rules/save-auto-rules-points-action';

const tips = {
  LIKED_YOUR_POST: 'When someone likes your post',
  LIKED_YOUR_COMMENT: 'When someone likes your comment',
  LIKED_YOUR_REPLY: 'When someone likes your reply',
  COMMENTED_ON_YOUR_POST: 'When someone comments on your post',
  REPLIED_TO_YOUR_POST: 'When someone replies to your post',
  FOLLOWED_YOUR_POST: 'When someone follows your post',
  BOOKMARKED_YOUR_POST: 'When someone bookmarks your post',
  APPRECIATED_YOUR_POST: 'When someone appreciates your post',
  DISLIKED_YOUR_POST: 'When someone dislikes your post',
  DISLIKED_YOUR_COMMENT: 'When someone dislikes your comment',
  DISLIKED_YOUR_REPLY: 'When someone dislikes your reply',
  POST_NOT_APPROVED: 'When your post is not approved',
  POST_UNDER_REVIEW: 'When your post is under review',
  VISITED_YOUR_POST: 'When someone visits your post',
};

const rules = Object.keys(AutoRuleNameEnum).map((item) => {
  return {
    autoRuleName: item,
    requiredPoints: 0,
  };
}) as IPointAutoRule[];

export default function PointAutoRules({ data }: { data: IPointAutoRule[] }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPointAutoRule[]>(
    rules.map((item, index) => {
      const find = data.find(
        (_item) => _item.autoRuleName === item.autoRuleName,
      );
      return find
        ? { ...find, _tip: tips[item.autoRuleName] }
        : { ...item, id: index, _tip: tips[item.autoRuleName] };
    }),
  );
  const [isUpdate, setIsUpdate] = useState(false);
  const [saving, setSaving] = useState(false);

  const saveAutoRulesPointsActionMutation = useMutation({
    mutationFn: SaveAutoRulesPointsAction,
  });

  function onClickUpdate() {
    setIsUpdate(!isUpdate);
  }

  async function onClickSave() {
    try {
      if (saving) {
        return;
      }
      setSaving(true);

      const _content = content.map((item) => ({
        autoRuleName: item.autoRuleName,
        requiredPoints: item.requiredPoints,
      }));

      for (let item of _content) {
        await saveAutoRulesPointsActionMutation.mutateAsync(item);
      }

      setIsUpdate(false);
      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      saveAutoRulesPointsActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    } finally {
      setSaving(false);
    }
  }

  return (
    <Box
      header={
        <div className="d-flex align-items-center justify-content-between gap-4">
          <div></div>
          <div className="d-flex gap-2">
            <button
              disabled={saving}
              onClick={onClickUpdate}
              type="button"
              className={clsx(
                'btn btn-sm',
                isUpdate ? 'btn-secondary' : 'btn-primary',
              )}
            >
              {isUpdate ? 'Cancel Update' : 'Update'}
            </button>

            {isUpdate && (
              <button
                disabled={saving}
                onClick={onClickSave}
                type="button"
                className="btn btn-sm btn-success"
              >
                {saving ? 'Saving' : 'Save'}
              </button>
            )}
          </div>
        </div>
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <caption>
            <p className="mb-0">
              You will automatically receive points rewards or refunds,
              depending on the status of the target
            </p>
            <p>
              The default value is 0, and the value should be a positive number
            </p>
          </caption>
          <thead>
            <tr>
              <th scope="col">AutoRuleTip</th>
              <th scope="col">RequiredPoints</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item._tip}</th>
                  <td>
                    {isUpdate ? (
                      <input
                        required
                        disabled={saving}
                        type="number"
                        className="form-control"
                        name="requiredPoints"
                        value={item.requiredPoints}
                        onChange={(event) => {
                          const find = content.find(
                            (_item) => item.id === _item.id,
                          );
                          if (!find) {
                            return;
                          }

                          const value = parseInt(event.target.value);
                          if (isNaN(value)) {
                            return;
                          }

                          find.requiredPoints = value;
                          setContent([...content]);
                        }}
                        placeholder="The default value is 0, and the value should be a positive number"
                        aria-describedby="requiredPoints"
                      />
                    ) : (
                      <>{item.requiredPoints}</>
                    )}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {content.length === 0 && <Nodata />}
    </Box>
  );
}
