'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { nonNum, trimObjectStrings } from '@/app/[locale]/common/client';
import type { ISubmenu } from '@/app/[locale]/interfaces/menus';
import UpdateSubmenuAction, {
  type IUpdateSubmenuActionVariables,
} from '@/app/[locale]/actions/submenus/update-submenu-action';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function Update({ submenu }: { submenu: ISubmenu }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    link: string;
    sort: number;
    menu: string;
  }>({
    name: submenu.name ?? '',
    link: submenu.link ?? '',
    sort: submenu.sort ?? 0,
    menu: (submenu.menu?.id ?? '') + '' ?? '',
  });
  const [actions, setActions] = useState<string[]>(
    submenu.actions.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/submenus',
    'Submenus#Update',
  );

  const updateSubmenuActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateSubmenuActionVariables;
    }) => {
      const response = await UpdateSubmenuAction(variables);
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
      }) as IUpdateSubmenuActionVariables;
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: 'The submenu name cannot be empty',
        });
        return;
      }
      if (!variables.link) {
        toast.current.show({
          type: 'danger',
          message: 'The submenu link cannot be empty',
        });
        return;
      }

      const menu = variables.menu;
      if (menu && nonNum(menu + '')) {
        delete variables.menu;
      }

      variables.actions = actions
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = submenu.id;
      await updateSubmenuActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateSubmenuActionMutation.reset();
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
          <label className="form-label">Actions</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={actions}
                setItems={setActions}
                showSourceInfo={submenu.actions}
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
            disabled={isActionDisabled || updateSubmenuActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateSubmenuActionMutation.isPending
              ? 'Updating'
              : 'Update Submenu'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
