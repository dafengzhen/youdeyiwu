'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useState } from 'react';
import type { IGlobalMessage } from '@/app/[locale]/interfaces/messages';
import { useTranslations } from 'next-intl';

export default function Details({ message }: { message: IGlobalMessage }) {
  const [form, setForm] = useState<{
    name: string;
    overview: string;
    link: string;
    links: string;
    content: string;
    sort: number;
    sender: string;
  }>({
    name: message.name ?? '',
    overview: message.overview ?? '',
    link: message.link ? message.link : '',
    links: message.links ? JSON.stringify(message.links) : '',
    content: message.content ? JSON.stringify(message.content) : '',
    sort: message.sort ?? 0,
    sender: message.sender ? message.sender.id + '' : '',
  });
  const t = useTranslations();

  return (
    <Box>
      <form className="vstack gap-4">
        <div>
          <label className="form-label">{t('common.name')}</label>
          <input
            readOnly
            type="text"
            className="form-control border-0 border-start"
            name="name"
            defaultValue={form.name}
            aria-describedby="name"
          />
        </div>

        <div>
          <label className="form-label">{t('common.overview')}</label>
          <textarea
            readOnly
            rows={2}
            className="form-control border-0 border-start"
            name="overview"
            defaultValue={form.overview}
            aria-describedby="overview"
          />
        </div>

        <div>
          <label className="form-label">{t('common.range')}</label>
          <select
            name="messageRange"
            className="form-select border-0 border-start"
            defaultValue="ALL_USER"
            aria-label="messageRange"
          >
            <option value="ALL_USER">ALL_USER</option>
          </select>
        </div>

        <div>
          <label className="form-label">{t('common.sort')}</label>
          <input
            readOnly
            type="number"
            className="form-control border-0 border-start"
            name="sort"
            defaultValue={form.sort}
            aria-describedby="sort"
          />
        </div>

        <div>
          <label className="form-label">{t('common.sender')}</label>
          <input
            readOnly
            type="text"
            className="form-control border-0 border-start"
            name="sender"
            defaultValue={form.sender}
            aria-describedby="sender"
          />
        </div>

        <div>
          <label className="form-label">{t('common.link')}</label>
          <textarea
            readOnly
            rows={1}
            className="form-control border-0 border-start"
            name="link"
            defaultValue={form.link}
            aria-describedby="link"
          />
        </div>

        <div>
          <label className="form-label">{t('common.links')}</label>
          <textarea
            readOnly
            rows={3}
            className="form-control border-0 border-start"
            name="links"
            defaultValue={form.links}
            aria-describedby="links"
          />
        </div>

        <div>
          <label className="form-label">{t('common.content')}</label>
          <textarea
            readOnly
            rows={3}
            className="form-control border-0 border-start"
            name="content"
            defaultValue={form.content}
            aria-describedby="content"
          />
        </div>
      </form>
    </Box>
  );
}
