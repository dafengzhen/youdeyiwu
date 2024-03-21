import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import LoginAction, {
  type ILoginActionVariables,
} from '@/app/[locale]/actions/users/login-action';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import { useRouter } from 'next/navigation';
import { useTranslations } from 'next-intl';

export default function Username() {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState({
    username: '',
    password: '',
  });
  const [isLogin, setIsLogin] = useState(false);
  const router = useRouter();
  const t = useTranslations();

  const loginActionMutation = useMutation({
    mutationFn: async (variables: ILoginActionVariables) => {
      const response = await LoginAction(variables);
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

      const variables = trimObjectStrings({ ...form }) as ILoginActionVariables;
      if (!variables.username) {
        toast.current.show({
          type: 'danger',
          message: 'Username can not be empty',
        });
        return;
      }

      if (variables.username.length < 3 || variables.username.length > 16) {
        toast.current.show({
          type: 'danger',
          message: 'Username length should be between 3 and 16 characters',
        });
        return;
      }

      if (!variables.password) {
        toast.current.show({
          type: 'danger',
          message: 'Password can not be empty',
        });
        return;
      }

      if (variables.password.length < 6 || variables.password.length > 18) {
        toast.current.show({
          type: 'danger',
          message: 'Password length should be between 6 and 18 characters',
        });
        return;
      }

      await loginActionMutation.mutateAsync(variables);
      setIsLogin(true);

      toast.current.show({
        type: 'success',
        message: 'Login successful, refreshing in 2 seconds',
      });

      setTimeout(() => {
        router.push('/');
      }, 2000);
    } catch (e: any) {
      loginActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <form className="d-flex flex-column gap-4" onSubmit={onSubmit}>
      <div>
        <label className="form-label"> {t('common.username')}</label>
        <input
          required
          disabled={isLogin || loginActionMutation.isPending}
          value={form.username}
          onChange={onChangeForm}
          name="username"
          type="text"
          className="form-control"
          placeholder={t('common.usernamePlaceholder')}
          aria-describedby="username"
        />
      </div>

      <div>
        <label className="form-label">{t('common.password')}</label>
        <input
          required
          disabled={isLogin || loginActionMutation.isPending}
          value={form.password}
          onChange={onChangeForm}
          name="password"
          type="password"
          placeholder={t('common.passwordPlaceholder')}
          className="form-control"
          autoComplete="password"
        />
      </div>

      <button
        disabled={isLogin || loginActionMutation.isPending}
        type="submit"
        className="btn rounded-2 btn-outline-primary my-4"
      >
        {loginActionMutation.isPending
          ? t('common.loggingIn')
          : t('common.signInNow')}
      </button>
    </form>
  );
}
