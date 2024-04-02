'use client';

import Box from '@/app/[locale]/admin/common/box';
import { useTranslations } from 'next-intl';
import Link from 'next/link';

export default function PostConfig() {
  const t = useTranslations();

  return (
    <Box>
      <div className="card">
        <div className="card-header bg-transparent border-0">
          {t('common.createGuide')}
        </div>
        <div className="card-body">
          <Link
            className="btn btn-success"
            href="/admin/configs/post/create-guide"
          >
            {t('common.update')}
          </Link>
        </div>
      </div>
    </Box>
  );
}
