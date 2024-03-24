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
import { useTranslations } from 'next-intl';

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
  const t = useTranslations();

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
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }
      if (!variables.link) {
        toast.current.show({
          type: 'danger',
          message: t('common.menuLinkCannotBeEmpty'),
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
        message: t('common.successfulUpdate'),
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
          <label className="form-label">{t('common.name')}</label>
          <input
            required
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">{t('common.nameCannotBeEmpty')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.link')}</label>
          <input
            required
            type="text"
            className="form-control"
            name="link"
            value={form.link}
            onChange={onChangeForm}
            aria-describedby="link"
            minLength={1}
          />
          <div className="form-text">{t('common.menuLinkFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.sort')}</label>
          <input
            min={0}
            type="number"
            className="form-control"
            name="sort"
            value={form.sort}
            onChange={onChangeForm}
            aria-describedby="sort"
          />
          <div className="form-text">{t('common.minimumValueIs0')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.menu')}</label>
          <input
            type="text"
            className="form-control"
            name="menu"
            value={form.menu}
            onChange={onChangeForm}
            aria-describedby="menu"
          />
          <div className="form-text">{t('common.menuFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.actions')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={actions}
                setItems={setActions}
                showSourceInfo={submenu.actions}
              />
            </div>
          </div>
          <div className="form-text">{t('common.actionsFormText')}</div>
        </div>

        <div>
          <button
            disabled={isActionDisabled || updateSubmenuActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateSubmenuActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
