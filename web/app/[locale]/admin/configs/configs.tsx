'use client';

import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import { useState } from 'react';
import { useTranslations } from 'next-intl';

export default function Configs() {
  const [items, setItems] = useState<string[]>(['jwt', 'point', 'post']);
  const t = useTranslations();

  return (
    <Box hideHeader={true}>
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col">{t('common.config')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {items.map((item) => {
              return (
                <tr key={item}>
                  <td scope="row">{item}</td>
                  <td>
                    <Link
                      className="link-body-emphasis text-decoration-none user-select-none"
                      href={`/admin/configs/${item}`}
                    >
                      {t('common.update')}
                    </Link>
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    </Box>
  );
}
