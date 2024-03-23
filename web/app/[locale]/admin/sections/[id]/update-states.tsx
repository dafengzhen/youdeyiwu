'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type {
  ISection,
  ISectionState,
} from '@/app/[locale]/interfaces/sections';
import {
  convertToCamelCase,
  nonNum,
  removeDuplicatesByProperty,
} from '@/app/[locale]/common/client';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import UpdateStatesSectionAction, {
  type IUpdateStatesSectionActionVariables,
} from '@/app/[locale]/actions/sections/update-states-section-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

interface IState {
  id: number | string;
  value: string;
  checked: boolean;
}

export default function UpdateStates({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    states: IState[];
    accessKey: string;
  }>({
    states: [
      'SHOW',
      'HIDE',
      'LOCK',
      'ALLOW',
      'BLOCK',
      'VISIBLE_AFTER_LOGIN',
    ].map((item) => {
      return {
        id: item,
        value: item,
        checked: section.states.includes(item as ISectionState),
      };
    }),
    accessKey: section.accessKey ?? '',
  });
  const [allows, setAllows] = useState<string[]>(
    section.allows.map((item) => item.id + ''),
  );
  const [blocks, setBlocks] = useState<string[]>(
    section.blocks.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/sections',
    'Sections#Update States',
  );
  const t = useTranslations();

  const updateStatesSectionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateStatesSectionActionVariables;
    }) => {
      const response = await UpdateStatesSectionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _states: ISectionState[] = removeDuplicatesByProperty(
        form.states.filter((item) => item.checked),
        'value',
      ).map((item) => item.value);
      if (_states.includes('LOCK') && !form.accessKey.trim()) {
        toast.current.show({
          type: 'danger',
          message: t('common.accessKeyFormText'),
        });
        return;
      }

      const _allows = allows
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));
      if (_states.includes('ALLOW') && _allows.length === 0) {
        toast.current.show({
          type: 'danger',
          message: t('common.whitelistStateFormText'),
        });
        return;
      }

      const _blocks = blocks
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));
      if (_states.includes('BLOCK') && _blocks.length === 0) {
        toast.current.show({
          type: 'danger',
          message: t('common.blacklistStateFormText'),
        });
        return;
      }

      const id = section.id;
      await updateStatesSectionActionMutation.mutateAsync({
        id,
        variables: {
          states: _states,
          allows: _allows,
          blocks: _blocks,
          accessKey: form.accessKey,
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateStatesSectionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeState(item: IState) {
    const find = form.states.find((value) => value.id === item.id);
    if (find) {
      find.checked = !find.checked;
    }
    setForm({ ...form, states: [...form.states] });
  }

  return (
    <Box title={`${section.name} (ID. ${section.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.states')}</label>
          <div>
            {form.states.map((item) => {
              return (
                <div key={item.id} className="form-check form-check-inline">
                  <input
                    className="form-check-input"
                    type="checkbox"
                    value={item.value}
                    checked={item.checked}
                    onChange={() => onChangeState(item)}
                  />
                  <label
                    onClick={() => onChangeState(item)}
                    className="cursor-pointer form-check-label text-capitalize user-select-none"
                  >
                    {convertToCamelCase(item.value, ' ')}
                  </label>
                </div>
              );
            })}
          </div>
        </div>

        {form.states.find((item) => item.value === 'LOCK' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              {t('common.accessKey')}
            </label>
            <input
              required
              type="text"
              className="form-control"
              name="accessKey"
              value={form.accessKey}
              onChange={(event) =>
                setForm({ ...form, accessKey: event.target.value })
              }
              aria-describedby="accessKey"
            />
            <div className="form-text">{t('common.theKeyCannotBeEmpty')}</div>
          </div>
        )}

        {form.states.find((item) => item.value === 'ALLOW' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              {t('common.allow')}
            </label>
            <div className="card rounded-2">
              <div className="card-body">
                <SimpleDynamicInput items={allows} setItems={setAllows} />
              </div>
            </div>
            <div className="form-text">{t('common.allowFormText1')}</div>
            <div className="form-text">{t('common.allowFormText2')}</div>
          </div>
        )}

        {form.states.find((item) => item.value === 'BLOCK' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              {t('common.block')}
            </label>
            <div className="card rounded-2">
              <div className="card-body">
                <SimpleDynamicInput items={blocks} setItems={setBlocks} />
              </div>
            </div>
            <div className="form-text">{t('common.blockFormText1')}</div>
            <div className="form-text">{t('common.blockFormText2')}</div>
          </div>
        )}

        <div>
          <button
            disabled={
              isActionDisabled || updateStatesSectionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateStatesSectionActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
