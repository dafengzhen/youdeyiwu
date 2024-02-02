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
  CREATE_POST: 'When you create a post',
  CREATE_COMMENT: 'When you create a comment',
  LIKE_POST: 'When you like a post',
  LIKE_COMMENT: 'When you like a comment',
  UPDATE_POST: 'When you update a post',
  FOLLOW_POST: 'When you follow a post',
  FAVORITE_POST: 'When you favorite a post',
  CREATE_REPLY: 'When you create a reply',
  ADD_POST_TAG: 'When you add a tag to a post',
  ADD_POST_CONTENT_LINK: 'When you add a content link to a post',
  ADD_POST_COVER_LINK: 'When you add a cover link to a post',
  ADD_POST_SECTION: 'When you add a section to a post',
};

const rules = Object.keys(RuleNameEnum).map((item) => {
  return {
    ruleName: item,
    requiredPoints: 0,
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
        requiredPoints: item.requiredPoints,
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
              The permission points required to perform these actions
            </p>
            <p>
              The default value is 0, and the value should be a positive number
            </p>
          </caption>
          <thead>
            <tr>
              <th scope="col">RuleTip</th>
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
