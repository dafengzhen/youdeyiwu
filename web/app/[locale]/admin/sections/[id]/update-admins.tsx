'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import UpdateAdminsSectionAction, {
  type IUpdateAdminsSectionActionVariables,
} from '@/app/[locale]/actions/sections/update-admins-section-action';
import { nonNum } from '@/app/[locale]/common/client';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function UpdateAdmins({ section }: { section: ISection }) {
  const { toast } = useContext(GlobalContext);
  const [admins, setAdmins] = useState<string[]>(
    section.admins.map((item) => item.id + ''),
  );
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/sections',
    'Sections#Update Admins',
  );
  const t = useTranslations();

  const updateAdminsSectionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateAdminsSectionActionVariables;
    }) => {
      const response = await UpdateAdminsSectionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _admins = admins
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = section.id;
      await updateAdminsSectionActionMutation.mutateAsync({
        id,
        variables: {
          admins: _admins,
        },
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateAdminsSectionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${section.name} (ID. ${section.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">{t('common.admins')}</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput items={admins} setItems={setAdmins} />
            </div>
          </div>
          <div className="form-text">{t('common.adminsFormText')}</div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updateAdminsSectionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateAdminsSectionActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
