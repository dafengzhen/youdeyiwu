'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import { ISectionState } from '@/app/interfaces/sections';
import {
  convertToCamelCase,
  nonNum,
  removeDuplicatesByProperty,
} from '@/app/common/client';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import {
  IPost,
  IPostReviewState,
  IPostSortState,
  IPostState,
} from '@/app/interfaces/posts';
import UpdateStatesPostAction from '@/app/actions/posts/update-states-post-action';

interface IState {
  id: number | string;
  value: string;
  checked: boolean;
}

export default function UpdateStates({ post }: { post: IPost }) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    states: IState[];
    accessKey: string;
    reviewState: IPostReviewState;
    sortState: IPostSortState;
  }>({
    states: [
      'SHOW',
      'HIDE',
      'LOCK',
      'ALLOW',
      'BLOCK',
      'VISIBLE_AFTER_LOGIN',
    ].map((item) => {
      return {
        id: item,
        value: item,
        checked: post.states.includes(item as IPostState),
      };
    }),
    accessKey: post.accessKey ?? '',
    reviewState: post.reviewState ?? 'APPROVED',
    sortState: post.sortState ?? 'DEFAULT',
  });
  const [allows, setAllows] = useState<string[]>(
    post.allows.map((item) => item.id + ''),
  );
  const [blocks, setBlocks] = useState<string[]>(
    post.blocks.map((item) => item.id + ''),
  );

  const updateStatesPostActionMutation = useMutation({
    mutationFn: UpdateStatesPostAction,
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _states: ISectionState[] = removeDuplicatesByProperty(
        form.states.filter((item) => item.checked),
        'value',
      ).map((item) => item.value);
      if (_states.includes('LOCK') && !form.accessKey.trim()) {
        toast.current.show({
          type: 'danger',
          message: 'In the locked state, the unlocking key cannot be empty',
        });
        return;
      }

      const _allows = allows
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));
      if (_states.includes('ALLOW') && _allows.length === 0) {
        toast.current.show({
          type: 'danger',
          message: 'Under the allows state, the whitelist cannot be empty',
        });
        return;
      }

      const _blocks = blocks
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));
      if (_states.includes('BLOCK') && _blocks.length === 0) {
        toast.current.show({
          type: 'danger',
          message: 'Under the blocks state, the blacklist cannot be empty',
        });
        return;
      }

      const reviewState = form.reviewState;
      if (!reviewState) {
        toast.current.show({
          type: 'danger',
          message: 'Review state cannot be empty',
        });
        return;
      }

      const sortState = form.sortState;
      if (!sortState) {
        toast.current.show({
          type: 'danger',
          message: 'Sort state cannot be empty',
        });
        return;
      }

      const id = post.id;
      await updateStatesPostActionMutation.mutateAsync({
        id,
        variables: {
          states: _states,
          allows: _allows,
          blocks: _blocks,
          accessKey: form.accessKey,
          reviewState,
          sortState,
        },
      });

      toast.current.show({
        type: 'success',
        message: 'States updated successfully',
      });
    } catch (e: any) {
      updateStatesPostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChangeState(item: IState) {
    const find = form.states.find((value) => value.id === item.id);
    if (find) {
      find.checked = !find.checked;
    }
    setForm({ ...form, states: [...form.states] });
  }

  function onClickReviewState(value: IPostReviewState) {
    setForm({
      ...form,
      reviewState: value,
    });
  }

  function onClickSortState(value: IPostSortState) {
    setForm({
      ...form,
      sortState: value,
    });
  }

  return (
    <Box title={`${post.name} (ID. ${post.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">States</label>
          <div>
            {form.states.map((item) => {
              return (
                <div key={item.id} className="form-check form-check-inline">
                  <input
                    className="form-check-input"
                    type="checkbox"
                    value={item.value}
                    checked={item.checked}
                    onChange={() => onChangeState(item)}
                  />
                  <label
                    onClick={() => onChangeState(item)}
                    className="cursor-pointer form-check-label text-capitalize user-select-none"
                  >
                    {convertToCamelCase(item.value, ' ')}
                  </label>
                </div>
              );
            })}
          </div>
          <div className="form-text">
            The state can be selected multiple times
          </div>
        </div>

        {form.states.find((item) => item.value === 'LOCK' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              Access Key
            </label>
            <input
              required
              type="text"
              className="form-control"
              name="accessKey"
              value={form.accessKey}
              onChange={(event) =>
                setForm({ ...form, accessKey: event.target.value })
              }
              placeholder="Please enter the post access key"
              aria-describedby="accessKey"
            />
            <div className="form-text">The key required to access the post</div>
          </div>
        )}

        {form.states.find((item) => item.value === 'ALLOW' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              Allow
            </label>
            <div className="card rounded-2">
              <div className="card-body">
                <SimpleDynamicInput items={allows} setItems={setAllows} />
              </div>
            </div>
            <div className="form-text">
              Please enter the user ID to add to the whitelist
            </div>
            <div className="form-text">
              If a user is both on the whitelist and the blacklist, the user
              will be ineffective in the whitelist
            </div>
          </div>
        )}

        {form.states.find((item) => item.value === 'BLOCK' && item.checked) && (
          <div>
            <label className="form-label">
              <span className="fw-bold text-danger">*</span>
              Block
            </label>
            <div className="card rounded-2">
              <div className="card-body">
                <SimpleDynamicInput items={blocks} setItems={setBlocks} />
              </div>
            </div>
            <div className="form-text">
              Please enter the user ID to add to the blacklist
            </div>
            <div className="form-text">
              If a user is both on the whitelist and the blacklist, the user
              will be considered effective in the blacklist
            </div>
          </div>
        )}

        <div>
          <label className="form-label">
            <span className="fw-bold text-danger">*</span>
            Review State
          </label>
          <div>
            {(
              ['APPROVED', 'REJECTED', 'PENDING_REVIEW'] as IPostReviewState[]
            ).map((item) => {
              return (
                <div key={item} className="form-check form-check-inline">
                  <input
                    required
                    className="form-check-input"
                    type="radio"
                    name="reviewState"
                    value={item}
                    checked={item === form.reviewState}
                    onChange={(event) =>
                      onClickReviewState(event.target.value as IPostReviewState)
                    }
                  />
                  <label
                    onClick={() => onClickReviewState(item)}
                    className="cursor-pointer form-check-label text-capitalize user-select-none"
                  >
                    {convertToCamelCase(item, ' ')}
                  </label>
                </div>
              );
            })}
          </div>
          <div className="form-text">
            Select an review state, with the default state set to
            &apos;Approved&lsquo;
          </div>
        </div>

        <div>
          <label className="form-label">
            <span className="fw-bold text-danger">*</span>
            Sort State
          </label>
          <div>
            {(
              [
                'DEFAULT',
                'POPULAR',
                'CURRENT_TOP',
                'GLOBAL_TOP',
              ] as IPostSortState[]
            ).map((item) => {
              return (
                <div key={item} className="form-check form-check-inline">
                  <input
                    required
                    className="form-check-input"
                    type="radio"
                    name="sortState"
                    value={item}
                    checked={item === form.sortState}
                    onChange={(event) =>
                      onClickSortState(event.target.value as IPostSortState)
                    }
                  />
                  <label
                    onClick={() => onClickSortState(item)}
                    className="cursor-pointer form-check-label text-capitalize user-select-none"
                  >
                    {convertToCamelCase(item, ' ')}
                  </label>
                </div>
              );
            })}
          </div>
          <div className="form-text">
            Select an sort state, with the default state set to
            &apos;Default&lsquo;
          </div>
        </div>

        <div>
          <button
            disabled={updateStatesPostActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateStatesPostActionMutation.isPending
              ? 'Updating'
              : 'Update Post States'}
          </button>
        </div>
      </form>
    </Box>
  );
}
