'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import {
  isValidJSON,
  nonNum,
  trimObjectStrings,
} from '@/app/[locale]/common/client';
import CreateGlobalMessageAction, {
  type ICreateGlobalMessageActionVariables,
} from '@/app/[locale]/actions/messages/create-global-message-action';
import type { TMessageRange } from '@/app/[locale]/interfaces/messages';
import CreateMessageAction, {
  type ICreateMessageActionVariables,
} from '@/app/[locale]/actions/messages/create-message-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';
import { useTranslations } from 'next-intl';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    overview: string;
    link: string;
    links: string;
    content: string;
    sort: number;
    messageRange: TMessageRange;
    receiver: string;
  }>({
    name: '',
    overview: '',
    link: '',
    links: '',
    content: '',
    sort: 0,
    messageRange: 'ALL_USER',
    receiver: '',
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/messages',
    'Messages#Create',
  );
  const t = useTranslations();

  const createGlobalMessageActionMutation = useMutation({
    mutationFn: async (variables: ICreateGlobalMessageActionVariables) => {
      const response = await CreateGlobalMessageAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });
  const createMessageActionMutation = useMutation({
    mutationFn: async (variables: ICreateMessageActionVariables) => {
      const response = await CreateMessageAction(variables);
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
      });
      if (!variables.name) {
        toast.current.show({
          type: 'danger',
          message: t('common.nameCannotBeEmpty'),
        });
        return;
      }
      if (!variables.overview) {
        toast.current.show({
          type: 'danger',
          message: t('common.messageOverviewFormText'),
        });
        return;
      }

      if (variables.content) {
        if (isValidJSON(variables.content)) {
          variables.content = JSON.parse(variables.content);
        } else {
          toast.current.show({
            type: 'danger',
            message: t('common.invalidJsonFormat'),
          });
          return;
        }
      }

      if (variables.links) {
        if (isValidJSON(variables.links)) {
          variables.links = JSON.parse(variables.links);
        } else {
          toast.current.show({
            type: 'danger',
            message: t('common.invalidJsonFormat'),
          });
          return;
        }
      }

      if (variables.content === '') {
        delete variables.content;
      }

      if (variables.links === '') {
        delete variables.links;
      }

      const messageRange = form.messageRange;
      if (messageRange === 'ALL_USER') {
        delete variables.receiver;
        delete variables.messageRange;
      } else if (messageRange === 'USER') {
        if (!variables.receiver || nonNum(variables.receiver)) {
          toast.current.show({
            type: 'danger',
            message: t('common.receiverFormText'),
          });
          return;
        }

        delete variables.sort;
        delete variables.messageRange;
        variables.receiver = parseInt(variables.receiver);
      }

      if (messageRange === 'ALL_USER') {
        await createGlobalMessageActionMutation.mutateAsync(
          variables as ICreateGlobalMessageActionVariables,
        );
      } else if (messageRange === 'USER') {
        await createMessageActionMutation.mutateAsync(
          variables as ICreateMessageActionVariables,
        );
      }

      setForm({
        ...form,
        name: '',
        overview: '',
        content: '',
        sort: 0,
        messageRange: 'ALL_USER',
        receiver: '',
      });

      toast.current.show({
        type: 'success',
        message: t('common.successfullyCreated'),
      });
    } catch (e: any) {
      const messageRange = form.messageRange;
      if (messageRange === 'ALL_USER') {
        createGlobalMessageActionMutation.reset();
      } else if (messageRange === 'USER') {
        createMessageActionMutation.reset();
      }

      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>,
  ) {
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
            {t('common.name')}
          </label>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.overview')}
          </label>
          <textarea
            required
            rows={2}
            className="form-control"
            name="overview"
            value={form.overview}
            onChange={onChangeForm}
            aria-describedby="overview"
            minLength={1}
          />
          <div className="form-text">{t('common.messageOverviewFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.link')}</label>
          <input
            type="text"
            className="form-control"
            name="link"
            value={form.link}
            onChange={onChangeForm}
            aria-describedby="link"
          />
          <div className="form-text">{t('common.linkFormText')}</div>
        </div>

        <div>
          <label className="form-label">{t('common.links')}</label>
          <textarea
            rows={3}
            className="form-control"
            name="links"
            value={form.links}
            onChange={onChangeForm}
            aria-describedby="links"
          />
          <div className="form-text">{t('common.linksFormText')}</div>
          <div className="form-text">
            {t('common.example')}:&nbsp;
            {
              '{"detail1": "https://www.xxx.com/detail/1", "detail2": "/posts/detail/2"}'
            }
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            {t('common.range')}
          </label>
          <select
            required
            name="messageRange"
            onChange={onChangeForm}
            className="form-select"
            value={form.messageRange}
            aria-label="messageRange"
          >
            <option value="ALL_USER">ALL_USER</option>
            <option value="USER">USER</option>
          </select>
          <div className="form-text"> {t('common.rangeFormText')}</div>
        </div>

        {form.messageRange === 'ALL_USER' && (
          <div>
            <label className="form-label">
              <span className="text-danger fw-bold">*</span>
              {t('common.sort')}
            </label>
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
        )}

        {form.messageRange === 'USER' && (
          <div>
            <label className="form-label">
              <span className="text-danger fw-bold">*</span>
              {t('common.receiver')}
            </label>
            <input
              required
              type="text"
              className="form-control"
              name="receiver"
              value={form.receiver}
              onChange={onChangeForm}
              aria-describedby="receiver"
              minLength={1}
            />
            <div className="form-text">{t('common.receiverFormText')}</div>
          </div>
        )}

        <div>
          <label className="form-label">{t('common.content')}</label>
          <textarea
            rows={3}
            className="form-control"
            name="content"
            value={form.content}
            onChange={onChangeForm}
            aria-describedby="content"
          />
          <div className="form-text">{t('common.messageContentFormText')}</div>
          <div className="form-text">
            {t('common.example')}:&nbsp;
            {'{"businessId": "xxx", "reason": "test send message"}'}
          </div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled ||
              createGlobalMessageActionMutation.isPending ||
              createMessageActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createGlobalMessageActionMutation.isPending ||
            createMessageActionMutation.isPending
              ? t('common.creating')
              : t('common.create')}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
