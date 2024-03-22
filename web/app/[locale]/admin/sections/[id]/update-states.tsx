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
          message: 'In the locked state, the unlocking key cannot be empty',
        });
        return;
      }

      const _allows = allows
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));
      if (_states.includes('ALLOW') && _allows.length === 0) {
        toast.current.show({
          type: 'danger',
          message: 'Under the allows state, the whitelist cannot be empty',
        });
        return;
      }

      const _blocks = blocks
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));
      if (_states.includes('BLOCK') && _blocks.length === 0) {
        toast.current.show({
          type: 'danger',
          message: 'Under the blocks state, the blacklist cannot be empty',
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
        message: 'States updated successfully',
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
          <label className="form-label">States</label>
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
          <div className="form-text">
            The state can be selected multiple times
          </div>
        </div>

        {form.states.find((item) => item.value === 'LOCK' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              Access Key
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
              placeholder="Please enter the section access key"
              aria-describedby="accessKey"
            />
            <div className="form-text">
              The key required to access the section
            </div>
          </div>
        )}

        {form.states.find((item) => item.value === 'ALLOW' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              Allow
            </label>
            <div className="card rounded-2">
              <div className="card-body">
                <SimpleDynamicInput items={allows} setItems={setAllows} />
              </div>
            </div>
            <div className="form-text">
              Please enter the user ID to add to the whitelist
            </div>
            <div className="form-text">
              If a user is both on the whitelist and the blacklist, the user
              will be ineffective in the whitelist
            </div>
          </div>
        )}

        {form.states.find((item) => item.value === 'BLOCK' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              Block
            </label>
            <div className="card rounded-2">
              <div className="card-body">
                <SimpleDynamicInput items={blocks} setItems={setBlocks} />
              </div>
            </div>
            <div className="form-text">
              Please enter the user ID to add to the blacklist
            </div>
            <div className="form-text">
              If a user is both on the whitelist and the blacklist, the user
              will be considered effective in the blacklist
            </div>
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
              ? 'Updating'
              : 'Update Section States'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
