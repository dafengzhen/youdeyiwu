'use client';

import Box from '@/app/admin/common/box';
import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { nonNum, trimObjectStrings } from '@/app/common/client';
import {
  TPermissionMethod,
  TPermissionType,
} from '@/app/interfaces/permissions';
import CreatePermissionAction, {
  ICreatePermissionActionVariables,
} from '@/app/actions/permissions/create-permission-action';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';

export default function Create() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    alias?: string;
    overview?: string;
    method: TPermissionMethod;
    type: TPermissionType;
    sort: number;
    caseInsensitive: boolean;
    matchers?: number[];
  }>({
    name: '',
    alias: '',
    overview: '',
    method: 'GET',
    type: 'ANT',
    sort: 0,
    caseInsensitive: false,
    matchers: [],
  });
  const [matchers, setMatchers] = useState<string[]>([]);

  const createPermissionActionMutation = useMutation({
    mutationFn: CreatePermissionAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({
        ...form,
      }) as ICreatePermissionActionVariables;
      if (variables.name.length < 1) {
        toast.current.show({
          type: 'danger',
          message: 'Permission name cannot be empty',
        });
        return;
      }

      variables.matchers = matchers
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      await createPermissionActionMutation.mutateAsync(variables);
      setForm({
        ...form,
        name: '',
        alias: '',
        overview: '',
        method: 'GET',
        type: 'ANT',
        sort: 0,
        caseInsensitive: false,
        matchers: [],
      });

      toast.current.show({
        type: 'success',
        message: 'Successfully created',
      });
    } catch (e: any) {
      createPermissionActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(e: ChangeEvent<HTMLInputElement | HTMLSelectElement>) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'caseInsensitive') {
      setForm({ ...form, caseInsensitive: value === 'true' });
    } else {
      setForm({ ...form, [name]: value });
    }
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
            placeholder="Please enter the permission URL"
            aria-describedby="name"
            minLength={1}
          />
          <div className="form-text">Please enter the permission URL</div>
          <div className="form-text">
            The permission URL should start with a &apos;/&apos;
          </div>
        </div>

        <div>
          <label className="form-label">Alias</label>
          <input
            type="text"
            className="form-control"
            name="alias"
            value={form.alias}
            onChange={onChangeForm}
            placeholder="Please enter the permission alias"
            aria-describedby="alias"
          />
          <div className="form-text">
            Consider giving the permission URL an alias
          </div>
        </div>

        <div>
          <label className="form-label">Overview</label>
          <input
            type="text"
            className="form-control"
            name="overview"
            value={form.overview}
            onChange={onChangeForm}
            placeholder="Please enter the permission overview"
            aria-describedby="overview"
          />
          <div className="form-text">
            Provide a brief overview of what this permission is for
          </div>
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
            placeholder="Please enter the permission sort"
            aria-describedby="sort"
          />
          <div className="form-text">The minimum value for sorting is 0</div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Case In Sensitive
          </label>
          <select
            required
            name="caseInsensitive"
            onChange={onChangeForm}
            className="form-select"
            value={form.caseInsensitive + ''}
            aria-label="caseInsensitive"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
          <div className="form-text">
            The default permission URL is case-sensitive
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Method
          </label>
          <select
            required
            name="method"
            onChange={onChangeForm}
            className="form-select"
            value={form.method}
            aria-label="method"
          >
            {[
              'GET',
              'POST',
              'PUT',
              'PATCH',
              'DELETE',
              'HEAD',
              'OPTIONS',
              'TRACE',
            ].map((item) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">
            Please select the permission request method
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger fw-bold">*</span>
            Type
          </label>
          <select
            required
            name="type"
            onChange={onChangeForm}
            className="form-select"
            value={form.type}
            aria-label="type"
          >
            {['ANT', 'REGEX'].map((item) => {
              return (
                <option key={item} value={item}>
                  {item}
                </option>
              );
            })}
          </select>
          <div className="form-text">Please select the permission URL type</div>
        </div>

        <div>
          <label className="form-label">Matchers</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput items={matchers} setItems={setMatchers} />
            </div>
          </div>
          <div className="form-text">
            The permission ID to be added must be an existing permission
          </div>
        </div>

        <div>
          <button
            disabled={
              !form.name.trim() || createPermissionActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {createPermissionActionMutation.isPending
              ? 'Creating'
              : 'Create Permission'}
          </button>
        </div>
      </form>
    </Box>
  );
}
