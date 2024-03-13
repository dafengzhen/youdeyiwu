import type { IPostDetails } from '@/app/[locale]/interfaces/posts';
import {
  getUserAlias,
  trimObjectStrings,
  wait,
} from '@/app/[locale]/common/client';
import {
  type ChangeEvent,
  type Dispatch,
  type FormEvent,
  type SetStateAction,
  useContext,
  useState,
} from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import Link from 'next/link';
import Image from 'next/image';
import { PostIdContext } from '@/app/[locale]/contexts/postid';
import clsx from 'clsx';

export default function RewardBox({
  details,
  onClickReward,
  rewarding,
  setRewarding,
}: {
  details: IPostDetails;
  onClickReward: () => void;
  rewarding: boolean;
  setRewarding: Dispatch<SetStateAction<boolean>>;
}) {
  const user = details.user;
  const alias = getUserAlias(user);

  const { toast, modal } = useContext(GlobalContext);
  const { currentUser } = useContext(PostIdContext);
  const [form, setForm] = useState<{
    amount: number;
    rewardGiver: string;
    message: string;
    paymentMethod: 'zfb' | 'wx';
    paymentMethodZfb: boolean;
    paymentMethodWx: boolean;
  }>({
    amount: 1,
    rewardGiver: '',
    message: '',
    paymentMethod: 'zfb',
    paymentMethodZfb: true,
    paymentMethodWx: false,
  });

  let uid = user?.id;
  let avatar = user?.avatar;
  if (user) {
    uid = user.id;
    avatar = user.avatar;
  }

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    e.stopPropagation();
    e.preventDefault();
  }

  async function onClickSubmit() {
    if (rewarding) {
      return;
    }
    setRewarding(true);

    try {
      if (!currentUser) {
        await wait();
        toast.current.show({
          type: 'danger',
          message:
            'Sorry, anonymous users are unable to reward the author, but we still appreciate your support and encouragement. If possible, we encourage you to register or log in to an account in order to proceed',
        });
        return;
      }

      const variables = trimObjectStrings({ ...form });
      if (variables.amount <= 0) {
        toast.current.show({
          type: 'danger',
          message:
            'Sorry, the reward amount cannot be 0. We kindly ask for your understanding and support',
        });
        return;
      }

      toast.current.show({
        type: 'success',
        message:
          'Reward successful! Regardless of the amount, thank you for your support and encouragement to the author',
      });

      onClickCancelSubmit();
    } catch (e: any) {
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    } finally {
      setRewarding(false);
    }
  }

  function onClickCancelSubmit() {
    onClickReward();
  }

  function onClickPaymentOption(value: 'zfb' | 'wx') {
    setForm({
      ...form,
      paymentMethod: value,
      paymentMethodZfb: value === 'zfb',
      paymentMethodWx: value === 'wx',
    });
  }

  function onChangeForm(
    e: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
  ) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'paymentMethod') {
      const _value = value !== 'zfb' && value !== 'wx' ? 'zfb' : value;
      setForm({
        ...form,
        paymentMethod: _value,
        paymentMethodZfb: _value === 'zfb',
        paymentMethodWx: _value === 'wx',
      });
    } else {
      setForm({ ...form, [name]: value });
    }
  }

  return (
    <div className="card rounded-4 border shadow mt-5">
      <div className="card-header bg-transparent border-bottom-0 fw-bold mt-2">
        Reward the author
      </div>
      <div className="card-body d-flex flex-column gap-3 py-2">
        <div className="d-flex justify-content-around gap-3">
          <Link href={user ? `/users/${uid}` : '/users'}>
            <Image
              className="rounded-circle object-fit-contain image-hover"
              src={avatar ?? '/avatar.png'}
              alt="avatar"
              width={50}
              height={50}
            />
          </Link>
          <div className="d-flex flex-column justify-content-around flex-grow-1">
            <div>
              <Link
                className="fw-medium link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                href={uid ? `/users/${uid}` : '/users'}
              >
                {alias}
              </Link>
            </div>
            <time className="fw-normal small">
              If you find my articles helpful, please feel free to reward/tip
              me. Your support will encourage me to continue creating!
            </time>
          </div>
        </div>

        <form className="vstack gap-4 mt-4" onSubmit={onSubmit}>
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              Amount (Chinese Yuan: RMB)
            </label>
            <div className="input-group">
              <span className="input-group-text">￥</span>
              <input
                min={0}
                type="number"
                className="form-control"
                name="amount"
                value={form.amount}
                onChange={onChangeForm}
                placeholder="Please enter the amount"
                aria-describedby="amount"
              />
            </div>
            <div className="form-text">
              The default amount is 1 RMB (Chinese currency)
            </div>
          </div>

          <div>
            <label className="form-label">Reward giver</label>
            <input
              type="text"
              className="form-control"
              name="rewardGiver"
              value={form.rewardGiver}
              onChange={onChangeForm}
              placeholder="Please enter the reward giver"
              aria-describedby="rewardGiver"
            />
            <div className="form-text">
              The reward giver (optional, can be a name, alias, or other contact
              information)
            </div>
          </div>

          <div>
            <label className="form-label">Message</label>
            <textarea
              rows={3}
              className="form-control"
              name="message"
              value={form.message}
              onChange={onChangeForm}
              placeholder="Please enter the message"
              aria-describedby="message"
            />
            <div className="form-text">
              Leave a message for the recipient of your reward or anything
              you&apos;d like to say to them
            </div>
          </div>

          <div>
            <label className="form-label">
              Payment option (Alphabetically sorted)
            </label>
            <div>
              <div className="form-check">
                <input
                  className="form-check-input"
                  type="radio"
                  name="paymentMethod"
                  value="zfb"
                  checked={form.paymentMethodZfb}
                  onChange={onChangeForm}
                />
                <label
                  className="form-check-label cursor-pointer user-select-none"
                  onClick={() => onClickPaymentOption('zfb')}
                >
                  <i
                    className={clsx('bi bi-alipay me-2', {
                      'text-primary': form.paymentMethod === 'zfb',
                    })}
                  ></i>
                  Ali pay
                </label>
              </div>
              <div className="form-check">
                <input
                  className="form-check-input"
                  type="radio"
                  name="paymentMethod"
                  value="wx"
                  checked={form.paymentMethodWx}
                  onChange={onChangeForm}
                />
                <label
                  className="form-check-label cursor-pointer user-select-none"
                  onClick={() => onClickPaymentOption('wx')}
                >
                  <i
                    className={clsx('bi bi-wechat me-2', {
                      'text-primary': form.paymentMethod === 'wx',
                    })}
                  ></i>
                  WeChat Pay
                </label>
              </div>
            </div>
          </div>

          <div className="text-end">
            <button
              disabled={rewarding}
              type="button"
              onClick={onClickCancelSubmit}
              className="btn rounded-pill btn-link link-secondary link-underline-opacity-0 link-underline-opacity-100-hover link-offset-2"
            >
              Cancel
            </button>
            <button
              disabled={rewarding}
              type="button"
              onClick={onClickSubmit}
              className="btn rounded-pill btn-link link-success link-offset-2"
            >
              {rewarding ? 'Rewarding' : 'Confirm'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
