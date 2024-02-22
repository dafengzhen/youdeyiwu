'use client';

import Box from '@/app/admin/common/box';
import Link from 'next/link';
import { MouseEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useRouter } from 'next/navigation';
import Nodata from '@/app/common/nodata';
import { IAction } from '@/app/interfaces/menus';

export default function Actions({ data }: { data: IAction[] }) {
  const { toast } = useContext(GlobalContext);
  const router = useRouter();
  const [content, setContent] = useState<IAction[]>(data);

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
              Create Action
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
              <th scope="col">Page</th>
              <th scope="col">Action</th>
              <th scope="col">Alias</th>
              <th scope="col">Sort</th>
              <th scope="col">Operate</th>
            </tr>
          </thead>
          <tbody>
            {content.map((item) => {
              const actionNames = item.name.split('#');
              return (
                <tr key={item.id}>
                  <th scope="row">{item.id}</th>
                  <td>{actionNames[0]}</td>
                  <td>{actionNames[1]}</td>
                  <td>{item.alias}</td>
                  <td>{item.sort}</td>
                  <td>
                    <div
                      className="cursor-pointer user-select-none"
                      data-bs-toggle="dropdown"
                    >
                      More
                      <ul className="dropdown-menu">
                        <li>
                          <Link
                            onClick={(event) =>
                              onClickLink(`/admin/actions/${item.id}`, event)
                            }
                            className="dropdown-item"
                            href={`/admin/actions/${item.id}`}
                          >
                            Update
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
                            Update Role
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
                            Delete
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
