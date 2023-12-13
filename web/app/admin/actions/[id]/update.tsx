'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { nonNum, trimObjectStrings } from '@/app/common/client';
import { IAction } from '@/app/interfaces/menus';
import UpdateActionAction, {
  IUpdateActionActionVariables,
} from '@/app/actions/actions/update-action-action';

export default function Update({ action }: { action: IAction }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    alias: string;
    sort: number;
    menu: string;
    submenu: string;
  }>({
    name: action.name ?? '',
    alias: action.alias ?? '',
    sort: action.sort ?? 0,
    menu: (action.menu?.id ?? '') + '' ?? '',
    submenu: (action.menu?.id ?? '') + '' ?? '',
  });

  const updateActionActionMutation = useMutation({
    mutationFn: UpdateActionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as IUpdateActionActionVariables;
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: 'The action name cannot be empty',
        });
        return;
      }
      if (!variables.alias) {
        variables.alias = variables.name;
      }

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
            Name
          </label>
          <input
            required
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            placeholder="Please enter the action name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">The action name cannot be empty</div>
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
          <div className="form-text">
            If the alias is empty, it defaults to the name
          </div>
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
            disabled={updateActionActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateActionActionMutation.isPending
              ? 'Updating'
              : 'Update Action'}
          </button>
        </div>
      </form>
    </Box>
  );
}
