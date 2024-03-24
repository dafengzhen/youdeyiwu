'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import UpdateSectionGroupAction, {
  type IUpdateSectionGroupActionVariables,
} from '@/app/[locale]/actions/section-groups/update-section-group-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Update({
  sectionGroup,
}: {
  sectionGroup: ISectionGroup;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    sort: number;
  }>({
    name: sectionGroup.name ?? '',
    sort: sectionGroup.sort ?? 0,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/section-groups',
    'Section Groups#Update',
  );
  const t = useTranslations();

  const updateSectionGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateSectionGroupActionVariables;
    }) => {
      const response = await UpdateSectionGroupAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({ ...form });
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }

      const id = sectionGroup.id;
      await updateSectionGroupActionMutation.mutateAsync({ id, variables });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateSectionGroupActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
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
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">{t('common.nameUniqueFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.sort')}</label>
          <input
            required
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
          <button
            disabled={
              isActionDisabled || updateSectionGroupActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateSectionGroupActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
