import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import { useMutation, useQuery } from '@tanstack/react-query';
import RegisterAction, {
  type IRegisterActionVariables,
} from '@/app/[locale]/actions/users/register-action';
import { useTranslations } from 'next-intl';
import QueryDisableRegistrationRootConfigAction from '@/app/[locale]/actions/configs/root/query-disable-registration-root-config-action';

export default function Username() {
  const [form, setForm] = useState({
    username: '',
    password: '',
  });
  const { toast } = useContext(GlobalContext);
  const [isRegister, setIsRegister] = useState(false);
  const router = useRouter();
  const t = useTranslations();

  const queryDisableRegistrationRootConfigActionQuery = useQuery({
    queryKey: ['/configs/root/disable-registration'],
    queryFn: async (context) => {
      const response = await QueryDisableRegistrationRootConfigAction();
      if (response.isError) {
        throw response;
      }

      return response.data;
    },
  });

  const registerActionMutation = useMutation({
    mutationFn: async (variables: IRegisterActionVariables) => {
      const response = await RegisterAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  function onChangeForm(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;
    setForm({ ...form, [name]: value });
  }

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.preventDefault();
      e.stopPropagation();

      const variables = trimObjectStrings({
        ...form,
      }) as IRegisterActionVariables;
      if (!variables.username) {
        toast.current.show({
          type: 'danger',
          message: t('common.usernameCannotBeEmpty'),
        });
        return;
      }

      if (variables.username.length < 3 || variables.username.length > 16) {
        toast.current.show({
          type: 'danger',
          message: t('common.usernameFromText'),
        });
        return;
      }

      if (!variables.password) {
        toast.current.show({
          type: 'danger',
          message: t('common.passwordCannotBeEmpty'),
        });
        return;
      }

      if (variables.password.length < 6 || variables.password.length > 18) {
        toast.current.show({
          type: 'danger',
          message: t('common.passwordFromText'),
        });
        return;
      }

      await registerActionMutation.mutateAsync(variables);
      setIsRegister(true);

      toast.current.show({
        type: 'success',
        message: t('common.registrationSuccessful'),
      });

      setTimeout(() => {
        router.push('/');
      }, 1000);
    } catch (e: any) {
      registerActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <form className="d-flex flex-column gap-4" onSubmit={onSubmit}>
      <div>
        <label className="form-label">
          <span className="text-danger fw-bold">*</span>
          {t('common.username')}
        </label>
        <input
          required
          disabled={isRegister || registerActionMutation.isPending}
          value={form.username}
          onChange={onChangeForm}
          name="username"
          type="text"
          className="form-control"
          placeholder={t('common.usernamePlaceholder')}
          aria-describedby="username"
        />
        <div className="form-text"> {t('common.usernameFromText')}</div>
      </div>

      <div>
        <label className="form-label">
          <span className="text-danger fw-bold">*</span>
          {t('common.password')}
        </label>
        <input
          required
          disabled={isRegister || registerActionMutation.isPending}
          value={form.password}
          onChange={onChangeForm}
          name="password"
          type="password"
          placeholder={t('common.passwordPlaceholder')}
          className="form-control"
          autoComplete="password"
        />
        <div className="form-text">{t('common.passwordFromText')}</div>
      </div>

      {queryDisableRegistrationRootConfigActionQuery.data ? (
        <div>
          <button
            disabled={true}
            type="submit"
            className="btn btn-outline-danger mt-4 w-100"
          >
            <i className="bi bi-slash-circle me-2"></i>
            {t('common.quickRegister')}
          </button>
          <div className="form-text text-danger text-center mt-2">
            {t('common.disableRegistrationMessage')}
          </div>
        </div>
      ) : (
        <button
          disabled={isRegister || registerActionMutation.isPending}
          type="submit"
          className="btn btn-outline-primary my-4"
        >
          {registerActionMutation.isPending
            ? t('common.registering')
            : t('common.quickRegister')}
        </button>
      )}
    </form>
  );
}
