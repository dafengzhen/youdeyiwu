'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { isValidJSON, nonNum, trimObjectStrings } from '@/app/common/client';
import CreateGlobalMessageAction, {
  ICreateGlobalMessageActionVariables,
} from '@/app/actions/messages/create-global-message-action';
import { TMessageRange } from '@/app/interfaces/messages';
import Link from 'next/link';
import CreateMessageAction, {
  ICreateMessageActionVariables,
} from '@/app/actions/messages/create-message-action';

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

  const createGlobalMessageActionMutation = useMutation({
    mutationFn: CreateGlobalMessageAction,
  });
  const createMessageActionMutation = useMutation({
    mutationFn: CreateMessageAction,
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
          message: 'The message name cannot be empty',
        });
        return;
      }
      if (!variables.overview) {
        toast.current.show({
          type: 'danger',
          message: 'The message overview cannot be empty',
        });
        return;
      }

      if (variables.content) {
        if (isValidJSON(variables.content)) {
          variables.content = JSON.parse(variables.content);
        } else {
          toast.current.show({
            type: 'danger',
            message:
              'Please enter the attributes of the message in JSON format',
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
            message:
              'Please enter the attributes of the message in JSON format',
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
            message: 'Please enter the message receiver ID',
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
        message: 'Successfully created',
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
            Name
          </label>
          <input
            required
            type="text"
            className="form-control"
            name="name"
            value={form.name}
            onChange={onChangeForm}
            placeholder="Please enter the message name"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">The message name cannot be empty</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Overview
          </label>
          <textarea
            required
            rows={2}
            className="form-control"
            name="overview"
            value={form.overview}
            onChange={onChangeForm}
            placeholder="Please enter the message overview"
            aria-describedby="overview"
            minLength={1}
          />
          <div className="form-text">The message overview cannot be empty</div>
        </div>

        <div>
          <label className="form-label">Link</label>
          <input
            type="text"
            className="form-control"
            name="link"
            value={form.link}
            onChange={onChangeForm}
            placeholder="Please enter the message link"
            aria-describedby="link"
          />
          <div className="form-text">Can be an absolute or relative path</div>
        </div>

        <div>
          <label className="form-label">Links</label>
          <textarea
            rows={3}
            className="form-control"
            name="links"
            value={form.links}
            onChange={onChangeForm}
            placeholder="Please enter the message links"
            aria-describedby="links"
          />
          <div className="form-text">
            Please enter the attributes of the message in
            <Link
              target="_blank"
              rel="noreferrer"
              className="link-dark"
              href="https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON"
            >
              &nbsp;JSON&nbsp;
            </Link>
            format. The object key is the link name, and the value is the
            specific link
          </div>
          <div className="form-text">
            Example:&nbsp;
            {
              '{"detail1": "https://www.xxx.com/detail/1", "detail2": "/posts/detail/2"}'
            }
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Range
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
          <div className="form-text">Please select a specific range</div>
        </div>

        {form.messageRange === 'ALL_USER' && (
          <div>
            <label className="form-label">
              <span className="text-danger fw-bold">*</span>
              Sort
            </label>
            <input
              required
              min={0}
              type="number"
              className="form-control"
              name="sort"
              value={form.sort}
              onChange={onChangeForm}
              placeholder="Please enter the message sort"
              aria-describedby="sort"
            />
            <div className="form-text">
              Please enter the sorting value for the message, with a minimum
              value of 0
            </div>
          </div>
        )}

        {form.messageRange === 'USER' && (
          <div>
            <label className="form-label">
              <span className="text-danger fw-bold">*</span>
              Receiver
            </label>
            <input
              required
              type="text"
              className="form-control"
              name="receiver"
              value={form.receiver}
              onChange={onChangeForm}
              placeholder="Please enter the message receiver ID"
              aria-describedby="receiver"
              minLength={1}
            />
            <div className="form-text">
              Please enter the recipient&apos;s ID, the user ID cannot be empty
            </div>
          </div>
        )}

        <div>
          <label className="form-label">Content</label>
          <textarea
            rows={3}
            className="form-control"
            name="content"
            value={form.content}
            onChange={onChangeForm}
            placeholder="Please enter the message content"
            aria-describedby="content"
          />
          <div className="form-text">
            Please enter the attributes of the message in
            <Link
              rel="noreferrer"
              target="_blank"
              className="link-dark"
              href="https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON"
            >
              &nbsp;JSON&nbsp;
            </Link>
            format. You can store some custom property values, which may be
            useful for further development
          </div>
          <div className="form-text">
            Example: {'{"businessId": "xxx", "reason": "test send message"}'}
          </div>
        </div>

        <div>
          <button
            disabled={
              createGlobalMessageActionMutation.isPending ||
              createMessageActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createGlobalMessageActionMutation.isPending ||
            createMessageActionMutation.isPending
              ? 'Creating'
              : 'Create Message'}
          </button>
          <div className="form-text"></div>
        </div>
      </form>
    </Box>
  );
}
