'use client';

import Box from '@/app/[locale]/admin/common/box';
import Nodata from '@/app/[locale]/common/nodata';
import {
  type IPointPermissionRule,
  PermissionRuleNameEnum,
} from '@/app/[locale]/interfaces/points';
import { useContext, useState } from 'react';
import clsx from 'clsx';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SavePermissionRulesPointsAction, {
  type ISavePermissionRulesPointsActionVariables,
} from '@/app/[locale]/actions/points/permission-rules/save-permission-rules-points-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

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
  const t = useTranslations();
  const tips = {
    CREATE_POST: t('common.createAPost'),
    CREATE_COMMENT: t('common.createPostComment'),
    CREATE_REPLY: t('common.createPostReply'),
    UPDATE_POST: t('common.updatePost'),
    ADD_POST_TAG: t('common.updatePostTags'),
    ADD_POST_CONTENT_LINK: t('common.updatePostContentLink'),
    ADD_POST_COVER_LINK: t('common.updatePostCoverLink'),
    ADD_POST_SECTION: t('common.updatePostSection'),
  };
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
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/points/permission-rules',
    'Point Permissions#Update',
  );

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
        message: t('common.successfulUpdate'),
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
            <div>
              <button
                disabled={saving}
                onClick={onClickUpdate}
                type="button"
                className={clsx(
                  'btn btn-sm',
                  isUpdate ? 'btn-secondary' : 'btn-primary',
                )}
              >
                {isUpdate ? t('common.cancelUpdate') : t('common.update')}
              </button>
            </div>

            {isUpdate && (
              <div className="d-flex flex-column">
                <button
                  disabled={isActionDisabled || saving}
                  onClick={onClickSave}
                  type="button"
                  className="btn btn-sm btn-success"
                >
                  {saving ? t('common.saving') : t('common.save')}
                </button>
                <AccessDeniedAlert />
              </div>
            )}
          </div>
        </div>
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <caption>{t('common.permissionRuleText')}</caption>
          <thead>
            <tr>
              <th scope="col">{t('common.rule')}</th>
              <th scope="col">{t('common.pointsRequired')}</th>
              <th scope="col">{t('common.costOfUse')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <td>{item._tip}</td>
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
