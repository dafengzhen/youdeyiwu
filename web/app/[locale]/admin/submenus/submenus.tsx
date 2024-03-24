'use client';

import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import { type MouseEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import Nodata from '@/app/[locale]/common/nodata';
import type { ISubmenu } from '@/app/[locale]/interfaces/menus';
import { useTranslations } from 'next-intl';

export default function Submenus({ data }: { data: ISubmenu[] }) {
  const { toast } = useContext(GlobalContext);
  const router = useRouter();
  const [content, setContent] = useState<ISubmenu[]>(data);
  const t = useTranslations();

  function onClickLink(url: string, e: MouseEvent<HTMLAnchorElement>) {
    e.stopPropagation();
    e.preventDefault();
    router.push(url);
  }

  return (
    <Box
      header={
        <div className="d-flex align-items-center justify-content-between gap-4">
          <div></div>
          <div>
            <Link
              href="/admin/submenus?type=add"
              type="button"
              className="btn btn-sm btn-primary"
            >
              {t('common.create')}
            </Link>
          </div>
        </div>
      }
    >
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col">ID</th>
              <th scope="col">{t('common.sort')}</th>
              <th scope="col">{t('common.name')}</th>
              <th scope="col">{t('common.link')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>{item.sort}</td>
                  <td>{item.name}</td>
                  <td>{item.link}</td>
                  <td>
                    <div
                      className="cursor-pointer user-select-none"
                      data-bs-toggle="dropdown"
                    >
                      {t('common.more')}
                      <ul className="dropdown-menu">
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(`/admin/submenus/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/submenus/${item.id}`}
                          >
                            {t('common.update')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/submenus/${item.id}?type=roles`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/submenus/${item.id}?type=roles`}
                          >
                            {t('common.updateRoles')}
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/submenus/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/submenus/${item.id}?type=del`}
                          >
                            {t('common.delete')}
                          </Link>
                        </li>
                      </ul>
                    </div>
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {content.length === 0 && <Nodata />}
    </Box>
  );
}
