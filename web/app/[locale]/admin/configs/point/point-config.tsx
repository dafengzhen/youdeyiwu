'use client';

import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import Box from '@/app/[locale]/admin/common/box';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import type { IPointConfig } from '@/app/[locale]/interfaces/configs';
import UpdatePointConfigAction, {
  type IUpdatePointActionVariables,
} from '@/app/[locale]/actions/configs/point/update-point-config-action';
import useMenuActionPermission from '@/app/[locale]/hooks/use-menu-action-permission';

export default function PointConfig({ config }: { config: IPointConfig }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    enable: boolean;
    initPoints: number;
  }>({
    enable: config.enable ?? false,
    initPoints: config.initPoints ?? 100,
  });
  const { isActionDisabled, AccessDeniedAlert } = useMenuActionPermission(
    '/admin/configs',
    'PointConfigs#Update',
  );

  const updatePointConfigActionMutation = useMutation({
    mutationFn: async (variables: IUpdatePointActionVariables) => {
      const response = await UpdatePointConfigAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings(form);
      await updatePointConfigActionMutation.mutateAsync(variables);

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updatePointConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'enable') {
      setForm({ ...form, enable: value === 'true' });
    } else {
      setForm({ ...form, [name]: value });
    }
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">
            <span className="text-danger">*</span>
            Enable
          </label>
          <select
            required
            name="enable"
            onChange={onChangeForm}
            className="form-select"
            value={form.enable + ''}
            aria-label="enable"
          >
            <option value="true">true</option>
            <option value="false">false</option>
          </select>
          <div className="form-text">
            Please select whether to enable a points system
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="text-danger">*</span>
            InitPoints
          </label>
          <input
            required
            type="number"
            className="form-control"
            name="initPoints"
            value={form.initPoints}
            onChange={onChangeForm}
            placeholder="Please enter the initial points for the user"
            aria-describedby="initPoints"
          />
          <div className="form-text">
            The default initial points are set to 100, but you can customize it
          </div>
        </div>

        <div>
          <button
            disabled={
              isActionDisabled || updatePointConfigActionMutation.isPending
            }
            type="submit"
            className="btn btn-success"
          >
            {updatePointConfigActionMutation.isPending
              ? 'Updating'
              : 'Update Config'}
          </button>
          <AccessDeniedAlert />
        </div>
      </form>
    </Box>
  );
}
