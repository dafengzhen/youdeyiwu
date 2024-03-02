'use client';

import Box from '@/app/admin/common/box';
import Nodata from '@/app/common/nodata';
import {
  type IPointPermissionRule,
  PermissionRuleNameEnum,
} from '@/app/interfaces/points';
import { useContext, useState } from 'react';
import clsx from 'clsx';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SavePermissionRulesPointsAction, {
  type ISavePermissionRulesPointsActionVariables,
} from '@/app/actions/points/permission-rules/save-permission-rules-points-action';

const tips = {
  CREATE_POST: 'permission to create a new post',
  CREATE_COMMENT: 'permission to create a comment',
  CREATE_REPLY: 'permission to create a reply',
  UPDATE_POST: 'permission to update a post',
  ADD_POST_TAG: 'permission to add tags to a post',
  ADD_POST_CONTENT_LINK: 'permission to add content links to a post',
  ADD_POST_COVER_LINK: 'permission to add cover links to a post',
  ADD_POST_SECTION: 'permission to add sections to a post',
};

const rules = Object.keys(PermissionRuleNameEnum).map((item) => {
  return {
    permissionRuleName: item,
    requiredPoints: 0,
    operationCost: 0,
  };
}) as IPointPermissionRule[];

export default function PointPermissionRules({
  data,
}: {
  data: IPointPermissionRule[];
}) {
  const { toast } = useContext(GlobalContext);
  const [content, setContent] = useState<IPointPermissionRule[]>(
    rules.map((item, index) => {
      const find = data.find(
        (_item) => _item.permissionRuleName === item.permissionRuleName,
      );
      return find
        ? { ...find, _tip: tips[item.permissionRuleName] }
        : { ...item, id: index, _tip: tips[item.permissionRuleName] };
    }),
  );
  const [isUpdate, setIsUpdate] = useState(false);
  const [saving, setSaving] = useState(false);

  const savePermissionRulesPointsActionMutation = useMutation({
    mutationFn: async (
      variables: ISavePermissionRulesPointsActionVariables,
    ) => {
      const response = await SavePermissionRulesPointsAction(variables);
      if (response.isError) {
        throw response;
      }
    },
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
        permissionRuleName: item.permissionRuleName,
        requiredPoints: item.requiredPoints,
        operationCost: item.operationCost,
      }));

      for (let item of _content) {
        await savePermissionRulesPointsActionMutation.mutateAsync(item);
      }

      setIsUpdate(false);
      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      savePermissionRulesPointsActionMutation.reset();
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
            The permission points required to execute this integration rule,
            with a default value of 0. The value should be a positive number
          </caption>
          <thead>
            <tr>
              <th scope="col">Rule</th>
              <th scope="col">RequiredPoints</th>
              <th scope="col">OperationCost</th>
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
                  <td>
                    {isUpdate ? (
                      <input
                        required
                        disabled={saving}
                        type="number"
                        className="form-control"
                        name="operationCost"
                        value={item.operationCost}
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

                          find.operationCost = value;
                          setContent([...content]);
                        }}
                        placeholder="The default value is 0, and the value should be a positive number"
                        aria-describedby="operationCost"
                      />
                    ) : (
                      <>{item.operationCost}</>
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
