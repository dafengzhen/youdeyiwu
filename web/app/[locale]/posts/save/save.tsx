'use client';

import type { IPost } from '@/app/[locale]/interfaces/posts';
import { useRouter } from 'next/navigation';
import Script from 'next/script';
import {
  type ChangeEvent,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useMutation } from '@tanstack/react-query';
import PublishPostAction, {
  type IPublishPostActionVariables,
} from '@/app/[locale]/actions/posts/publish-post-action';
import clsx from 'clsx';
import SimpleDynamicInput from '@/app/[locale]/common/simple-dynamic-input';
import { isHttpOrHttps, trimObjectStrings } from '@/app/[locale]/common/client';
import {
  getContent,
  onErrorEditor,
  onLoadEditor,
  setContent,
} from '@/app/[locale]/common/editor';
import type { ISection } from '@/app/[locale]/interfaces/sections';
import RefreshAction from '@/app/[locale]/actions/refresh-action';
import UploadCover from '@/app/[locale]/posts/save/upload-cover';
import { isNum } from '@/app/[locale]/common/tool';
import { useTranslations } from 'next-intl';
import usePointsAlert from '@/app/[locale]/hooks/use-points-alert ';

const POST_EDITOR_COLLAPSE = 'post-editor-collapse';
const POST_EDITOR_SPLIT = 'post-editor-split';
const POST_EDITOR_CENTER = 'post-editor-center';

export default function Save({
  post,
  sections,
}: {
  post?: IPost;
  sections: Pick<ISection, 'id' | 'name'>[];
}) {
  const isEdit = !!post;
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    name: string;
    cover: string;
    overview: string;
    contentLink: string;
    content: string;
    sectionId: string;
  }>(
    isEdit
      ? {
          name: post.name ?? '',
          cover: post.cover ?? '',
          overview: post.overview ?? '',
          contentLink: post.contentLink ?? '',
          content: post.content ?? '',
          sectionId: (post?.section?.id ?? 'none') + '',
        }
      : {
          name: '',
          cover: '',
          overview: '',
          contentLink: '',
          content: '',
          sectionId: 'none',
        },
  );
  const router = useRouter();
  const editorElementRef = useRef<HTMLDivElement>(null);
  const editorRef = useRef<any>(null);
  const [editorInitializing, setEditorInitializing] = useState(true);
  const [expand, setExpand] = useState(true);
  const [center, setCenter] = useState(false);
  const [split, setSplit] = useState(false);
  const [tags, setTags] = useState<string[]>(
    post?.tags.map((item) => item.name) ?? [],
  );
  const [sectionOptions, setSectionOptions] = useState<
    { id: number; name: string }[]
  >(sections.map((item) => ({ id: item.id, name: item.name })));
  const [first, setFirst] = useState(false);
  const t = useTranslations();
  const pointsAlert = usePointsAlert();

  const publishPostActionMutation = useMutation({
    mutationFn: async (variables: {
      id?: number;
      variables?: IPublishPostActionVariables;
    }) => {
      const response = await PublishPostAction(variables);
      if (response.isError) {
        throw response;
      }

      return response.data;
    },
  });
  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });

  useEffect(() => {
    const item = localStorage.getItem(POST_EDITOR_COLLAPSE);
    if (item === 'true') {
      setExpand(true);
    } else if (item === 'false') {
      setExpand(false);
    } else {
      setExpand(true);
    }
  }, []);
  useEffect(() => {
    const item = localStorage.getItem(POST_EDITOR_SPLIT);
    if (item === 'true') {
      setSplit(true);
    } else if (item === 'false') {
      setSplit(false);
    } else {
      setSplit(false);
    }
  }, []);
  useEffect(() => {
    const item = localStorage.getItem(POST_EDITOR_CENTER);
    if (item === 'true') {
      setCenter(true);
    } else if (item === 'false') {
      setCenter(false);
    } else {
      setCenter(false);
    }
  }, []);
  useEffect(() => {
    const current = editorRef.current;
    const content = isEdit ? post.content ?? '' : '';
    if (current && content) {
      setContent(content, editorRef);
    }
  }, [isEdit, editorRef.current]);

  function onClickReturn() {
    router.back();
  }

  function onClickCollapseForm() {
    const value = !expand;
    setExpand(value);
    localStorage.setItem(POST_EDITOR_COLLAPSE, value + '');
  }

  function onClickSplitLayout() {
    const value = !split;
    setSplit(value);
    localStorage.setItem(POST_EDITOR_SPLIT, value + '');
  }

  function onClickCenterBox() {
    const value = !center;
    setCenter(value);
    localStorage.setItem(POST_EDITOR_CENTER, value + '');
  }

  async function onSubmit() {
    try {
      if (isEdit && !post.createdBy) {
        toast.current.show({
          type: 'danger',
          message: 'The anonymous article cannot be edited',
        });
        return;
      }

      const variables = trimObjectStrings({ ...form });
      variables.sectionId = isNum(variables.sectionId)
        ? parseInt(variables.sectionId)
        : undefined;
      variables.removeSection = variables.sectionId === 'none';

      const name = variables.name;
      if (!name) {
        toast.current.show({
          type: 'danger',
          message: 'The post name cannot be empty',
        });
        return;
      }

      if (variables.cover && !isHttpOrHttps(variables.cover)) {
        toast.current.show({
          type: 'danger',
          message: 'The cover link only supports the HTTP or HTTPS protocol',
        });
        return;
      }

      if (variables.contentLink && !isHttpOrHttps(variables.contentLink)) {
        toast.current.show({
          type: 'danger',
          message: 'The content link only supports the HTTP or HTTPS protocol',
        });
        return;
      }

      variables.content = getContent(editorRef);
      variables.tags = tags
        .map((item) => item.trim())
        .filter((item) => item !== '');

      const id = post?.id;
      const response = await publishPostActionMutation.mutateAsync({
        id,
        variables,
      });

      let message = 'Successfully published';
      if (isEdit) {
        message = 'Successfully updated';
        setForm({ ...form, content: variables.content });
      } else {
        message = 'Successfully published, Refresh after 1 seconds';
        setFirst(true);
      }

      pointsAlert.refresh();

      toast.current.show({
        type: 'success',
        message,
      });

      if (!isEdit) {
        if (response?.startsWith('/')) {
          setTimeout(() => {
            refreshActionMutation.mutateAsync({
              url: response,
              tags: ['/'],
            });
          }, 1500);
        } else {
          toast.current.show({
            type: 'danger',
            message: 'Refresh failed, please manually click the back button',
          });
        }
      }
    } catch (e: any) {
      publishPostActionMutation.reset();
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
    setForm({ ...form, [name]: value });
  }

  return (
    <>
      <div className="row mx-0">
        <div className="col">
          <div className={clsx({ container: center })}>
            <div className="card rounded-2">
              <div className="card-header">
                <div className="d-flex align-items-center flex-wrap justify-content-between gap-4">
                  <div className="d-flex align-items-center flex-wrap gap-2 user-select-none">
                    <div
                      className={clsx(
                        'cursor-pointer',
                        expand ? 'text-secondary' : 'text-primary',
                      )}
                      aria-expanded="true"
                      aria-controls="collapse form"
                      onClick={onClickCollapseForm}
                    >
                      {expand ? (
                        <>
                          {t('common.collapseForm')}
                          <i className="bi bi-arrows-collapse ms-1"></i>
                        </>
                      ) : (
                        <>
                          {t('common.ExpandForm')}
                          <i className="bi bi-arrows-expand ms-1"></i>
                        </>
                      )}
                    </div>
                    <div className="vr text-secondary"></div>
                    <div
                      className={clsx(
                        'cursor-pointer',
                        split ? 'text-primary' : 'text-secondary',
                      )}
                      onClick={onClickSplitLayout}
                    >
                      {split ? (
                        <>
                          {t('common.splitLayout')}
                          <i className="bi bi-arrows-expand-vertical ms-1"></i>
                        </>
                      ) : (
                        <>
                          {t('common.verticalLayout')}
                          <i className="bi bi-arrows-collapse-vertical ms-1"></i>
                        </>
                      )}
                    </div>
                    <div className="vr text-secondary"></div>
                    <div
                      className={clsx(
                        'cursor-pointer',
                        center ? 'text-primary' : 'text-secondary',
                      )}
                      onClick={onClickCenterBox}
                    >
                      {center ? (
                        <>
                          {t('common.centeredLayout')}
                          <i className="bi bi-arrows-vertical ms-1"></i>
                        </>
                      ) : (
                        <>
                          {t('common.wideLayout')}
                          <i className="bi bi-arrows ms-1"></i>
                        </>
                      )}
                    </div>
                  </div>
                  <div className="d-flex align-items-center flex-wrap gap-2">
                    <button
                      disabled={first || publishPostActionMutation.isPending}
                      onClick={onSubmit}
                      type="button"
                      className="btn btn-success"
                    >
                      {isEdit ? (
                        <>
                          {publishPostActionMutation.isPending
                            ? t('common.updating')
                            : t('common.update')}
                        </>
                      ) : (
                        <>
                          {publishPostActionMutation.isPending
                            ? t('common.publishing')
                            : t('common.publish')}
                        </>
                      )}
                    </button>
                    <button
                      disabled={publishPostActionMutation.isPending}
                      onClick={onClickReturn}
                      type="button"
                      className="btn btn-secondary"
                    >
                      {t('common.return')}
                    </button>
                  </div>
                </div>
              </div>
              <div className="card-body">
                <form className={clsx(split ? 'row' : 'vstack gap-4')}>
                  <div className={clsx(split ? 'col-6' : '')}>
                    <div
                      className={clsx('vstack gap-4', {
                        'd-none': !expand,
                      })}
                    >
                      <div>
                        <label className="form-label">
                          <span className="fw-bold text-danger">*</span>
                          {t('common.name')}
                        </label>
                        <textarea
                          required
                          rows={1}
                          className="form-control"
                          name="name"
                          value={form.name}
                          onChange={onChangeForm}
                          aria-describedby="name"
                          minLength={1}
                        />
                        <div className="form-text">
                          {t('common.postNameFormText')}
                        </div>
                      </div>

                      <div>
                        <label className="form-label">
                          {t('common.contentLink')}
                        </label>
                        <input
                          type="text"
                          className="form-control"
                          name="contentLink"
                          value={form.contentLink}
                          onChange={onChangeForm}
                          aria-describedby="contentLink"
                        />
                        <div className="form-text">
                          {t('common.contentLinkFormText')}
                        </div>
                      </div>

                      <div>
                        <label className="form-label">
                          {t('common.coverLink')}
                        </label>
                        <input
                          type="text"
                          className="form-control"
                          name="cover"
                          value={form.cover}
                          onChange={onChangeForm}
                          aria-describedby="cover"
                        />
                        <div className="form-text">
                          {t('common.coverLinkFormText')}
                        </div>
                      </div>

                      {post?.id && (
                        <UploadCover
                          id={isEdit && post.createdBy ? post.id : undefined}
                          callback={() => {
                            setForm({
                              ...form,
                              cover: `${location.origin}/api/posts/${post.id}/cover`,
                            });
                          }}
                        />
                      )}

                      <div>
                        <label className="form-label">
                          {t('common.selectContent')}
                        </label>
                        <select
                          className="form-select"
                          aria-label="Select Content"
                          value={form.sectionId}
                          onChange={onChangeForm}
                          name="sectionId"
                        >
                          <option value="none" defaultValue="none">
                            {t('common.none')}
                          </option>
                          {sectionOptions.map((item) => {
                            return (
                              <option key={item.id} value={item.id}>
                                {item.name}
                              </option>
                            );
                          })}
                        </select>
                        <div className="form-text">
                          {t('common.postSelectContentFormText')}
                        </div>
                      </div>

                      <div>
                        <label className="form-label">
                          {t('common.addTags')}
                        </label>
                        <div className="card rounded-2">
                          <div className="card-body">
                            <SimpleDynamicInput
                              items={tags}
                              setItems={setTags}
                            />
                          </div>
                        </div>
                        <div className="form-text">
                          {t('common.postAddTagsFormText')}
                        </div>
                      </div>

                      <div>
                        <label className="form-label">
                          {t('common.overview')}
                          {form.overview.length > 0 && (
                            <span>&nbsp;({form.overview.length})</span>
                          )}
                        </label>
                        <textarea
                          rows={3}
                          className="form-control"
                          name="overview"
                          value={form.overview}
                          onChange={onChangeForm}
                          aria-describedby="overview"
                        />
                        <div className="form-text">
                          {t('common.overviewFormText')}
                        </div>
                      </div>
                    </div>
                  </div>
                  <div className={clsx(split ? 'col-6' : '')}>
                    {!expand && (
                      <div className="mb-4">
                        <label className="form-label">
                          <span className="fw-bold text-danger">*</span>
                          {t('common.name')}
                        </label>
                        <textarea
                          required
                          rows={1}
                          className="form-control"
                          name="name"
                          value={form.name}
                          onChange={onChangeForm}
                          aria-describedby="name"
                          minLength={1}
                        />
                        <div className="form-text">
                          {t('common.postNameFormText')}
                        </div>
                      </div>
                    )}

                    <div>
                      <label className="form-label">
                        {t('common.content')}
                      </label>
                      <div className="form-text mb-2">
                        {t('common.contentFormText')}
                      </div>
                      {editorInitializing && (
                        <div className="form-text mb-2">
                          {t('common.editorLoading')}
                        </div>
                      )}
                      <div ref={editorElementRef}></div>
                    </div>
                  </div>
                </form>
              </div>
            </div>
          </div>
        </div>
      </div>
      <Script
        onReady={() =>
          onLoadEditor(editorElementRef, editorRef, () =>
            setEditorInitializing(false),
          )
        }
        onError={(e) => onErrorEditor(e, toast)}
        src="/editor/ckeditor.js"
      />
    </>
  );
}
