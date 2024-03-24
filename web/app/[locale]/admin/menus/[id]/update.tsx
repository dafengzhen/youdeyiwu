'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { nonNum, trimObjectStrings } from '@/app/[locale]/common/client';
import type { IMenu } from '@/app/[locale]/interfaces/menus';
import UpdateMenuAction, {
  type IUpdateMenuActionVariables,
} from '@/app/[locale]/actions/menus/update-menu-action';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

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
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/menus',
    'Menus#Update',
  );
  const t = useTranslations();

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
        message: t('common.successfulUpdate'),
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
          <label className="form-label"> {t('common.sort')}</label>
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
          <label className="form-label">{t('common.submenus')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={submenus}
                setItems={setSubmenus}
                showSourceInfo={menu.submenus}
              />
            </div>
          </div>
          <div className="form-text">{t('common.submenusFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.actions')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={actions}
                setItems={setActions}
                showSourceInfo={menu.actions}
              />
            </div>
          </div>
          <div className="form-text">{t('common.actionsFormText')}</div>
        </div>

        <div>
          <button
            disabled={isActionDisabled || updateMenuActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateMenuActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
