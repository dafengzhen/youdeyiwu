'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { nonNum, trimObjectStrings } from '@/app/common/client';
import type { IMenu } from '@/app/interfaces/menus';
import UpdateMenuAction, {
  type IUpdateMenuActionVariables,
} from '@/app/actions/menus/update-menu-action';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';

export default function Update({ menu }: { menu: IMenu }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    link: string;
    sort: number;
  }>({
    name: menu.name ?? '',
    link: menu.link ?? '',
    sort: menu.sort ?? 0,
  });
  const [submenus, setSubmenus] = useState<string[]>(
    menu.submenus.map((item) => item.id + ''),
  );
  const [actions, setActions] = useState<string[]>(
    menu.actions.map((item) => item.id + ''),
  );

  const updateMenuActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateMenuActionVariables;
    }) => {
      const response = await UpdateMenuAction(variables);
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
      }) as IUpdateMenuActionVariables;
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: 'The menu name cannot be empty',
        });
        return;
      }
      if (!variables.link) {
        toast.current.show({
          type: 'danger',
          message: 'The menu link cannot be empty',
        });
        return;
      }

      variables.submenus = submenus
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));
      variables.actions = actions
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = menu.id;
      await updateMenuActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateMenuActionMutation.reset();
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
            placeholder="Please enter the menu name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">The menu name cannot be empty</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Link
          </label>
          <input
            required
            type="text"
            className="form-control"
            name="link"
            value={form.link}
            onChange={onChangeForm}
            placeholder="Please enter the menu link"
            aria-describedby="link"
            minLength={1}
          />
          <div className="form-text">The menu link cannot be empty</div>
          <div className="form-text">
            The link can be either a page path or a regular access link
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
            placeholder="Please enter the menu sort"
            aria-describedby="sort"
          />
          <div className="form-text">The minimum value for sorting is 0</div>
        </div>

        <div>
          <label className="form-label">Submenus</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={submenus}
                setItems={setSubmenus}
                showSourceInfo={menu.submenus}
              />
            </div>
          </div>
          <div className="form-text">
            Please enter the submenu ID. If you haven&apos;t created a tag group
            yet, please create one first
          </div>
          <div className="form-text">
            The note to remove the submenu means that the submenu will also be
            deleted
          </div>
        </div>

        <div>
          <label className="form-label">Actions</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={actions}
                setItems={setActions}
                showSourceInfo={menu.actions}
              />
            </div>
          </div>
          <div className="form-text">
            Please enter the action ID. If you haven&apos;t created a tag group
            yet, please create one first
          </div>
          <div className="form-text">
            The note to remove the action means that the action will also be
            deleted
          </div>
          <div className="form-text">
            The action corresponds to a menu or submenu, and if the action is
            already used by another menu, you should create a new action
          </div>
        </div>

        <div>
          <button
            disabled={updateMenuActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateMenuActionMutation.isPending ? 'Updating' : 'Update Menu'}
          </button>
        </div>
      </form>
    </Box>
  );
}
