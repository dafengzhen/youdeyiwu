'use client';

import Box from '@/app/[locale]/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import CreateActionAction, {
  type ICreateActionActionVariables,
} from '@/app/[locale]/actions/actions/create-action-action';
import { ACTION_PAGES_DATA } from '@/app/[locale]/constants';

const ACTION_PAGES = Object.keys(ACTION_PAGES_DATA);

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    page: string;
    button: string;
    alias: string;
    sort: number;
  }>({
    page: ACTION_PAGES[0] ?? '',
    button: ACTION_PAGES[0]
      ? (ACTION_PAGES_DATA as any)[ACTION_PAGES[0]][0] ?? ''
      : '',
    alias: '',
    sort: 0,
  });

  const createActionActionMutation = useMutation({
    mutationFn: async (variables: ICreateActionActionVariables) => {
      const response = await CreateActionAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({ ...form });
      if (!variables.page) {
        toast.current.show({
          type: 'danger',
          message: 'Please select a specific page',
        });
        return;
      } else if (!variables.button) {
        toast.current.show({
          type: 'danger',
          message: 'Then choose an action within that page',
        });
        return;
      }

      variables.name = `${variables.page}#${variables.button}`;

      delete variables.page;
      delete variables.button;

      await createActionActionMutation.mutateAsync(variables);
      setForm({ ...form, page: '', button: '', alias: '', sort: 0 });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createActionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) {
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
            Page
          </label>
          <select
            required
            name="page"
            onChange={onChangeForm}
            className="form-select"
            value={form.page}
            aria-label="page"
          >
            {ACTION_PAGES.map((item) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">Please select a specific page</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Action
          </label>
          <select
            required
            name="button"
            onChange={onChangeForm}
            className="form-select"
            value={form.button}
            aria-label="button"
          >
            {(ACTION_PAGES_DATA as any)[form.page].map((item: string) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">
            Then choose an action within that page
          </div>
          {(ACTION_PAGES_DATA as any)[form.page].length === 0 && (
            <div className="form-text text-decoration-underline">
              The current selection has no options available, cannot proceed
              with the creation
            </div>
          )}
        </div>

        <div>
          <label className="form-label">Alias</label>
          <input
            type="text"
            className="form-control"
            name="alias"
            value={form.alias}
            onChange={onChangeForm}
            placeholder="Please enter the action alias"
            aria-describedby="link"
          />
          <div className="form-text">Give this action a different name</div>
        </div>

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
            placeholder="Please enter the menu sort"
            aria-describedby="sort"
          />
          <div className="form-text">
            Please enter the sorting value for the action, with a minimum value
            of 0
          </div>
        </div>

        <div>
          <button
            disabled={
              (ACTION_PAGES_DATA as any)[form.page].length === 0 ||
              createActionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createActionActionMutation.isPending
              ? 'Creating'
              : 'Create Action'}
          </button>
        </div>
      </form>
    </Box>
  );
}
