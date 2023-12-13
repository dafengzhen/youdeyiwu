'use client';

import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import Box from '@/app/admin/common/box';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { trimObjectStrings } from '@/app/common/client';
import GenerateRandomSecretJwtConfigAction from '@/app/actions/configs/jwt/generate-random-secret-jwt-config-action';
import UpdateJwtConfigAction from '@/app/actions/configs/jwt/update-jwt-config-action';
import { IJwtConfig } from '@/app/interfaces/configs';

export default function JwtConfig({ config }: { config: IJwtConfig }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    secret: string;
  }>({
    secret: config.secret ?? '',
  });

  const generateRandomSecretJwtConfigActionMutation = useMutation({
    mutationFn: GenerateRandomSecretJwtConfigAction,
  });
  const updateJwtConfigActionMutation = useMutation({
    mutationFn: UpdateJwtConfigAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const variables = trimObjectStrings({ ...form });
      if (variables.secret && variables.secret === config.secret) {
        delete variables.secret;
      }

      await updateJwtConfigActionMutation.mutateAsync(variables);

      toast.current.show({
        type: 'success',
        message: 'Successfully updated',
      });
    } catch (e: any) {
      updateJwtConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  async function onClickGenerateRandomSecret() {
    try {
      const secret =
        await generateRandomSecretJwtConfigActionMutation.mutateAsync();
      setForm({ ...form, secret });

      toast.current.show({
        type: 'success',
        message:
          'The new key has been generated. Save and apply for it to take effect',
      });
    } catch (e: any) {
      generateRandomSecretJwtConfigActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">
            <span className="text-danger">*</span>
            Secret
          </label>
          <div className="input-group">
            <input
              required
              readOnly
              type="text"
              className="form-control"
              name="secret"
              value={form.secret}
              onChange={onChangeForm}
              placeholder="The JWT secret cannot be empty"
              aria-describedby="secret"
            />
            <button
              disabled={generateRandomSecretJwtConfigActionMutation.isPending}
              onClick={onClickGenerateRandomSecret}
              className="btn btn-outline-secondary"
              type="button"
            >
              {generateRandomSecretJwtConfigActionMutation.isPending
                ? 'Generating'
                : 'Generate random secret'}
            </button>
          </div>
          <div className="form-text">
            If you want to generate a new secret, please click the button to
            generate. Manual input is not supported at the moment
          </div>
          <div className="form-text">
            Attention, changing the current JWT secret means that previously
            issued tokens will become invalid, and users will need to log in
            again
          </div>
          <div className="form-text">
            Do not disclose it to avoid security issues
          </div>
        </div>

        <div>
          <button
            disabled={updateJwtConfigActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateJwtConfigActionMutation.isPending
              ? 'Updating'
              : 'Update Config'}
          </button>
        </div>
      </form>
    </Box>
  );
}
