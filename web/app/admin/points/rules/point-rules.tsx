'use client';

import Box from '@/app/admin/common/box';
import Nodata from '@/app/common/nodata';
import { IPointRule, RuleNameEnum } from '@/app/interfaces/points';
import { useContext, useState } from 'react';
import clsx from 'clsx';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SaveRulesPointsAction from '@/app/actions/points/rules/save-rules-points-action';

const tips = {
  LIKE_POST: 'earning points for liking a post',
  LIKE_COMMENT: 'earning points for liking a comment',
  LIKE_REPLY: 'earning points for liking a reply',
  COMMENT_POST: 'earning points for commenting on a post',
  REPLY_POST: 'earning points for replying to a post',
  FOLLOW_POST: 'earning points for following a post',
  FAVORITE_POST: 'earning points for marking a post as a favorite',
  DISLIKE_POST: 'earning points for disliking a post',
  DISLIKE_COMMENT: 'earning points for disliking a comment',
  DISLIKE_REPLY: 'earning points for disliking a reply',
  POST_APPROVED: 'earning points for having a post approved',
  POST_NOT_APPROVED: 'earning points for having a post not approved',
  POST_PENDING_REVIEW: 'earning points for a post pending review',
  VISIT_POST: 'earning points for visiting a post',
  CREATE_POST: 'earning points for creating a new post',
};

const rules = Object.keys(RuleNameEnum).map((item) => {
  return {
    ruleName: item,
    initiatorRewardPoints: 0,
    receiverRewardPoints: 0,
  };
}) as IPointRule[];

export default function PointRules({ data }: { data: IPointRule[] }) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPointRule[]>(
    rules.map((item, index) => {
      const find = data.find((_item) => _item.ruleName === item.ruleName);
      return find
        ? { ...find, _tip: tips[item.ruleName] }
        : { ...item, id: index, _tip: tips[item.ruleName] };
    }),
  );
  const [isUpdate, setIsUpdate] = useState(false);
  const [saving, setSaving] = useState(false);

  const saveRulesPointsActionMutation = useMutation({
    mutationFn: SaveRulesPointsAction,
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
        ruleName: item.ruleName,
        initiatorRewardPoints: item.initiatorRewardPoints,
        receiverRewardPoints: item.receiverRewardPoints,
      }));

      for (let item of _content) {
        await saveRulesPointsActionMutation.mutateAsync(item);
      }

      setIsUpdate(false);
      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      saveRulesPointsActionMutation.reset();
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
              <th scope="col">Rule</th>
              <th scope="col">InitiatorRewardPoints</th>
              <th scope="col">ReceiverRewardPoints</th>
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
                        name="initiatorRewardPoints"
                        value={item.initiatorRewardPoints}
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

                          find.initiatorRewardPoints = value;
                          setContent([...content]);
                        }}
                        placeholder="The default value is 0, and the value should be a positive number"
                        aria-describedby="initiatorRewardPoints"
                      />
                    ) : (
                      <>{item.initiatorRewardPoints}</>
                    )}
                  </td>
                  <td>
                    {isUpdate ? (
                      <input
                        required
                        disabled={saving}
                        type="number"
                        className="form-control"
                        name="receiverRewardPoints"
                        value={item.receiverRewardPoints}
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

                          find.receiverRewardPoints = value;
                          setContent([...content]);
                        }}
                        placeholder="The default value is 0, and the value should be a positive number"
                        aria-describedby="receiverRewardPoints"
                      />
                    ) : (
                      <>{item.receiverRewardPoints}</>
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
