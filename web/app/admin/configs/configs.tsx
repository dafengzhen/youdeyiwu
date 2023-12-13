'use client';

import Box from '@/app/admin/common/box';
import Link from 'next/link';
import { useState } from 'react';

export default function Configs() {
  const [items, setItems] = useState<string[]>(['jwt']);

  return (
    <Box>
      <div className="table-responsive">
        <table className="table align-middle table-striped">
          <thead>
            <tr>
              <th scope="col">ID</th>
              <th scope="col">Operate</th>
            </tr>
          </thead>
          <tbody>
            {items.map((item) => {
              return (
                <tr key={item}>
                  <th scope="row">{item}</th>
                  <td>
                    <Link
                      className="link-dark text-decoration-none user-select-none"
                      href={`/admin/configs/${item}`}
                    >
                      Update
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
