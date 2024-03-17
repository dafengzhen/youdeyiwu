'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { nonNum, trimObjectStrings } from '@/app/[locale]/common/client';
import type { IAction } from '@/app/[locale]/interfaces/menus';
import UpdateActionAction, {
  type IUpdateActionActionVariables,
} from '@/app/[locale]/actions/actions/update-action-action';
import { ACTION_PAGES_DATA } from '@/app/[locale]/constants';
import useMenuActionPermission from '@/app/[locale]/hooks/useMenuActionPermission';

const ACTION_PAGES = Object.keys(ACTION_PAGES_DATA);

export default function Update({ action }: { action: IAction }) {
  const actionNames = (action.name ?? '').split('#');
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    page: string;
    button: string;
    alias: string;
    sort: number;
    menu: string;
    submenu: string;
  }>({
    page: actionNames[0] ?? ACTION_PAGES[0] ?? '',
    button:
      actionNames[1] ?? (ACTION_PAGES_DATA as any)[ACTION_PAGES[0]][0] ?? '',
    alias: action.alias ?? '',
    sort: action.sort ?? 0,
    menu: (action.menu?.id ?? '') + '' ?? '',
    submenu: (action.submenu?.id ?? '') + '' ?? '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/actions',
    'Actions#Update',
  );

  const updateActionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateActionActionVariables;
    }) => {
      const response = await UpdateActionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as IUpdateActionActionVariables & {
        page?: any;
        button?: any;
      };

      if (!variables.page) {
        toast.current.show({
          type: 'danger',
          message: 'Please select a specific page',
        });
        return;
      } else if (!variables.button) {
        toast.current.show({
          type: 'danger',
          message: 'Then choose an action within that page',
        });
        return;
      }

      variables.name = `${variables.page}#${variables.button}`;

      delete variables.page;
      delete variables.button;

      const menu = variables.menu;
      if (menu && nonNum(menu + '')) {
        delete variables.menu;
      }

      const submenu = variables.submenu;
      if (submenu && nonNum(submenu + '')) {
        delete variables.submenu;
      }

      const id = action.id;
      await updateActionActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateActionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Page
          </label>
          <select
            required
            name="page"
            onChange={onChangeForm}
            className="form-select"
            value={form.page}
            aria-label="page"
          >
            {ACTION_PAGES.map((item) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">Please select a specific page</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Action
          </label>
          <select
            required
            name="button"
            onChange={onChangeForm}
            className="form-select"
            value={form.button}
            aria-label="button"
          >
            {(ACTION_PAGES_DATA as any)[form.page].map((item: string) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">
            Then choose an action within that page
          </div>
        </div>

        <div>
          <label className="form-label">Alias</label>
          <input
            type="text"
            className="form-control"
            name="alias"
            value={form.alias}
            onChange={onChangeForm}
            placeholder="Please enter the action alias"
            aria-describedby="link"
          />
          <div className="form-text">Give this action a different name</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Sort
          </label>
          <input
            min={0}
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            placeholder="Please enter the action sort"
            aria-describedby="sort"
          />
          <div className="form-text">The minimum value for sorting is 0</div>
        </div>

        <div>
          <label className="form-label">Menu</label>
          <input
            type="text"
            className="form-control"
            name="menu"
            value={form.menu}
            onChange={onChangeForm}
            placeholder="Please enter the menu ID"
            aria-describedby="menu"
          />
          <div className="form-text">
            Please enter the menu ID. If you don&apos;t have a menu yet, please
            create one first
          </div>
        </div>

        <div>
          <label className="form-label">Submenu</label>
          <input
            type="text"
            className="form-control"
            name="submenu"
            value={form.submenu}
            onChange={onChangeForm}
            placeholder="Please enter the submenu ID"
            aria-describedby="submenu"
          />
          <div className="form-text">
            Please enter the submenu ID. If you don&apos;t have a submenu yet,
            please create one first
          </div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled ||
              (ACTION_PAGES_DATA as any)[form.page].length === 0 ||
              updateActionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateActionActionMutation.isPending
              ? 'Updating'
              : 'Update Action'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
