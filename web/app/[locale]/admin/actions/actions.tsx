'use client';

import Box from '@/app/[locale]/admin/common/box';
import Link from 'next/link';
import { type MouseEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import Nodata from '@/app/[locale]/common/nodata';
import type { IAction } from '@/app/[locale]/interfaces/menus';
import { useTranslations } from 'next-intl';

export default function Actions({ data }: { data: IAction[] }) {
  const { toast } = useContext(GlobalContext);
  const router = useRouter();
  const [content, setContent] = useState<IAction[]>(data);
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
              href="/admin/actions?type=add"
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
              <th scope="col">{t('common.page')}</th>
              <th scope="col">{t('common.action')}</th>
              <th scope="col">{t('common.alias')}</th>
              <th scope="col">{t('common.operate')}</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              const actionNames = item.name.split('#');
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>{item.sort}</td>
                  <td>{actionNames[0]}</td>
                  <td>{actionNames[1]}</td>
                  <td>{item.alias}</td>
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
                              onClickLink(`/admin/actions/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/actions/${item.id}`}
                          >
                            {t('common.update')}
                          </Link>
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/actions/${item.id}?type=role`,
                                event,
                              )
                            }
                            className="dropdown-item"
                            href={`/admin/actions/${item.id}?type=role`}
                          >
                            {t('common.updateRole')}
                          </Link>
                        </li>
                        <li>
                          <hr className="dropdown-divider" />
                        </li>
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(
                                `/admin/actions/${item.id}?type=del`,
                                event,
                              )
                            }
                            className="dropdown-item text-danger"
                            href={`/admin/actions/${item.id}?type=del`}
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
