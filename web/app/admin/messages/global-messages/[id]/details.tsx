'use client';

import Box from '@/app/admin/common/box';
import { useState } from 'react';
import { IGlobalMessage } from '@/app/interfaces/messages';

export default function Details({ message }: { message: IGlobalMessage }) {
  const [form, setForm] = useState<{
    name: string;
    overview: string;
    content: string;
    sort: number;
    sender: string;
  }>({
    name: message.name ?? '',
    overview: message.overview ?? '',
    content: message.content ? JSON.stringify(message.content) : '',
    sort: message.sort ?? 0,
    sender: message.sender ? message.sender.id + '' : '',
  });

  return (
    <Box>
      <form className="vstack gap-4">
        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Name
          </label>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Overview
          </label>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Range
          </label>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Sort
          </label>
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
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Sender
          </label>
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
          <label className="form-label">Content</label>
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
