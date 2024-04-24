'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { IPost } from '@/app/[locale]/interfaces/posts';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';
import UpdateStylesPostAction, {
  type IUpdateStylesPostActionVariables,
} from '@/app/[locale]/actions/posts/update-styles-post-action';
import { convertStyles } from '@/app/[locale]/common/tool';
import clsx from 'clsx';

export default function UpdateStyles({ post }: { post: IPost }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    styles: string;
    classNames: string;
  }>({
    styles: post.styles ?? '',
    classNames: post.classNames ?? '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/posts',
    'Posts#Update Styles',
  );
  const t = useTranslations();

  const updateStylesPostActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateStylesPostActionVariables;
    }) => {
      const response = await UpdateStylesPostAction(variables);
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
      const id = post.id;
      await updateStylesPostActionMutation.mutateAsync({
        id,
        variables,
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfulUpdate'),
      });
    } catch (e: any) {
      updateStylesPostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function tryConvertStyles() {
    try {
      return convertStyles(form.styles);
    } catch (e: any) {
      toast.current.show({
        type: 'danger',
        message: e?.message ?? t('common.stylesPreviewError'),
      });
      return {};
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
    <Box title={`${post.name} (ID. ${post.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label"> {t('common.styles')}</label>
          <textarea
            rows={1}
            className="form-control"
            name="styles"
            value={form.styles}
            onChange={onChangeForm}
            aria-describedby="styles"
          />
          <div className="form-text">{t('common.stylesFormText')}</div>
          <div className="form-text">Example: fontWeight:bold; color:blue</div>
        </div>

        {form.styles && (
          <div>
            <label className="form-label">{t('common.stylesPreview')}</label>
            <div style={tryConvertStyles()}>{post.name}</div>
          </div>
        )}

        <div>
          <label className="form-label"> {t('common.classNames')}</label>
          <textarea
            rows={1}
            className="form-control"
            name="classNames"
            value={form.classNames}
            onChange={onChangeForm}
            aria-describedby="classNames"
          />
          <div className="form-text">{t('common.classNamesFormText')}</div>
          <div className="form-text">Example: fw-bold text-primary</div>
        </div>

        {form.classNames && (
          <div>
            <label className="form-label">
              {t('common.classNamesPreview')}
            </label>
            <div className={clsx(form.classNames)}>{post.name}</div>
          </div>
        )}

        <div>
          <button
            disabled={
              isActionDisabled || updateStylesPostActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updateStylesPostActionMutation.isPending
              ? t('common.updating')
              : t('common.update')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
