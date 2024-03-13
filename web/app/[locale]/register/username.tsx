import { type ChangeEvent, type FormEvent, useContext, useState } from 'react';
import { trimObjectStrings } from '@/app/[locale]/common/client';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useRouter } from 'next/navigation';
import { useMutation } from '@tanstack/react-query';
import RegisterAction, {
  type IRegisterActionVariables,
} from '@/app/[locale]/actions/users/register-action';

export default function Username() {
  const [form, setForm] = useState({
    username: '',
    password: '',
  });
  const { toast } = useContext(GlobalContext);
  const [isRegister, setIsRegister] = useState(false);
  const router = useRouter();

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

      await registerActionMutation.mutateAsync(variables);
      setIsRegister(true);

      toast.current.show({
        type: 'success',
        message: 'Register successful, refreshing in 2 seconds',
      });

      setTimeout(() => {
        router.push('/');
      }, 2000);
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
          Username
        </label>
        <input
          required
          disabled={isRegister || registerActionMutation.isPending}
          value={form.username}
          onChange={onChangeForm}
          name="username"
          type="text"
          className="form-control"
          placeholder="Please enter your username"
          aria-describedby="username"
        />
        <div className="form-text">
          Username length should be between 3 and 16 characters
        </div>
      </div>

      <div>
        <label className="form-label">
          <span className="text-danger fw-bold">*</span>
          Password
        </label>
        <input
          required
          disabled={isRegister || registerActionMutation.isPending}
          value={form.password}
          onChange={onChangeForm}
          name="password"
          type="password"
          placeholder="Please enter your password"
          className="form-control"
          autoComplete="password"
        />
        <div className="form-text">
          Password length should be between 6 and 18 characters
        </div>
      </div>

      <button
        disabled={isRegister || registerActionMutation.isPending}
        type="submit"
        className="btn btn-outline-primary my-4"
      >
        {registerActionMutation.isPending ? 'Registering' : 'Quick Register'}
      </button>
    </form>
  );
}
