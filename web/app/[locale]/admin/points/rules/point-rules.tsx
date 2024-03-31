'use client';

import Box from '@/app/[locale]/admin/common/box';
import Nodata from '@/app/[locale]/common/nodata';
import {
  type IPointRule,
  RuleNameEnum,
} from '@/app/[locale]/interfaces/points';
import { useContext, useState } from 'react';
import clsx from 'clsx';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import SaveRulesPointsAction, {
  type ISaveRulesPointsActionVariables,
} from '@/app/[locale]/actions/points/rules/save-rules-points-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

const rules = Object.keys(RuleNameEnum).map((item) => {
  return {
    ruleName: item,
    initiatorRewardPoints: 0,
    receiverRewardPoints: 0,
    enable: false,
  };
}) as IPointRule[];

export default function PointRules({ data }: { data: IPointRule[] }) {
  const t = useTranslations();
  const tips = {
    LIKE_POST: t('common.likingPosts'),
    LIKE_COMMENT: t('common.likePostComments'),
    LIKE_REPLY: t('common.likingRepliesToPosts'),
    COMMENT_POST: t('common.commentOnAPost'),
    REPLY_POST: t('common.replyToAPost'),
    // FOLLOW_POST: t('common.followAPost'),
    FAVORITE_POST: t('common.favouritePosts'),
    // DISLIKE_POST: t('common.dontLikeThisPost'),
    // DISLIKE_COMMENT: t('common.dontLikeThisPostComment'),
    // DISLIKE_REPLY: t('common.dontLikeThisPostReply'),
    POST_APPROVED: t('common.postsApprovedForModeration'),
    POST_NOT_APPROVED: t('common.postsNotApproved'),
    // POST_PENDING_REVIEW: t('common.postsInModeration'),
    VISIT_POST: t('common.browsePosts'),
    CREATE_POST: t('common.createAPost'),
  };
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
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/points/rules',
    'Point Rules#Update',
  );

  const saveRulesPointsActionMutation = useMutation({
    mutationFn: async (variables: ISaveRulesPointsActionVariables) => {
      const response = await SaveRulesPointsAction(variables);
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
        ruleName: item.ruleName,
        initiatorRewardPoints: item.initiatorRewardPoints,
        receiverRewardPoints: item.receiverRewardPoints,
        enable: item.enable,
      }));

      for (let item of _content) {
        await saveRulesPointsActionMutation.mutateAsync(item);
      }

      setIsUpdate(false);
      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
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
          <caption>
            <div>{t('common.ruleText')}</div>
            <div>{t('common.ruleText2')}</div>
          </caption>
          <thead>
            <tr>
              <th scope="col">{t('common.rule')}</th>
              <th scope="col">{t('common.senderBonusPoints')}</th>
              <th scope="col">{t('common.recipientBonusPoints')}</th>
              <th scope="col">{t('common.enable')}</th>
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
                        aria-describedby="senderBonusPoints"
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
                        aria-describedby="recipientBonusPoints"
                      />
                    ) : (
                      <>{item.receiverRewardPoints}</>
                    )}
                  </td>
                  <td>
                    {isUpdate ? (
                      <input
                        required
                        disabled={saving}
                        type="checkbox"
                        className="form-check-input"
                        name="enable"
                        value={item.enable + ''}
                        onChange={(event) => {
                          const find = content.find(
                            (_item) => item.id === _item.id,
                          );
                          if (!find) {
                            return;
                          }

                          find.enable = event.target.checked;
                          setContent([...content]);
                        }}
                        aria-describedby="enable"
                      />
                    ) : (
                      <input
                        disabled
                        type="checkbox"
                        className="form-check-input"
                        name="enable"
                        defaultChecked={item.enable}
                        aria-describedby="enable"
                      />
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
