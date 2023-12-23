'use client';

import { IPost } from '@/app/interfaces/posts';
import { useRouter } from 'next/navigation';
import Script from 'next/script';
import { ChangeEvent, useContext, useEffect, useRef, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import PublishPostAction from '@/app/actions/posts/publish-post-action';
import clsx from 'clsx';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { isHttpOrHttps, trimObjectStrings } from '@/app/common/client';
import {
  getContent,
  onErrorEditor,
  onLoadEditor,
  setContent,
} from '@/app/common/editor';
import { ISection } from '@/app/interfaces/sections';
import RefreshAction from '@/app/actions/refresh-action';
import UploadCover from '@/app/posts/save/upload-cover';

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

  const refreshActionMutation = useMutation({
    mutationFn: RefreshAction,
  });
  const publishPostActionMutation = useMutation({
    mutationFn: PublishPostAction,
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
      delete variables.uploadCoverFile;

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
        message = 'Successfully published, Refresh after 2 seconds';
        setFirst(true);
      }

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
          }, 2000);
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
                          Collapse form
                          <i className="bi bi-arrows-collapse ms-1"></i>
                        </>
                      ) : (
                        <>
                          Expand form
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
                          Split layout
                          <i className="bi bi-arrows-expand-vertical ms-1"></i>
                        </>
                      ) : (
                        <>
                          Vertical layout
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
                          Centered layout
                          <i className="bi bi-arrows-vertical ms-1"></i>
                        </>
                      ) : (
                        <>
                          Wide layout
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
                            ? 'Updating'
                            : 'Update'}
                        </>
                      ) : (
                        <>
                          {publishPostActionMutation.isPending
                            ? 'Publishing'
                            : 'Publish'}
                        </>
                      )}
                    </button>
                    <button
                      disabled={publishPostActionMutation.isPending}
                      onClick={onClickReturn}
                      type="button"
                      className="btn btn-secondary"
                    >
                      Return
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
                          Name
                        </label>
                        <textarea
                          required
                          rows={1}
                          className="form-control"
                          name="name"
                          value={form.name}
                          onChange={onChangeForm}
                          placeholder="Please enter the post name"
                          aria-describedby="name"
                          minLength={1}
                        />
                        <div className="form-text">
                          Recommendations for Post Names: Concise and Structured
                        </div>
                      </div>

                      <div>
                        <label className="form-label">Select Content</label>
                        <select
                          className="form-select"
                          aria-label="Select Content"
                          placeholder="Please select a content topic"
                          value={form.sectionId}
                          onChange={onChangeForm}
                          name="sectionId"
                        >
                          <option value="none" defaultValue="none">
                            None
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
                          Choose a content topic, which refers to a broad
                          category. However, you can also choose not to make a
                          selection or use tags for more specific categorization
                        </div>
                      </div>

                      <div>
                        <label className="form-label">Content Link</label>
                        <input
                          type="text"
                          className="form-control"
                          name="contentLink"
                          value={form.contentLink}
                          onChange={onChangeForm}
                          placeholder="Please enter the post content link"
                          aria-describedby="contentLink"
                        />
                        <div className="form-text">
                          The links only support the HTTP or HTTPS protocols
                        </div>
                        <div className="form-text">
                          When there is a content link, the content will not be
                          displayed, and clicking will directly redirect
                        </div>
                      </div>

                      <div>
                        <label className="form-label">
                          Upload cover or enter cover link
                        </label>
                        <input
                          type="text"
                          className="form-control"
                          name="cover"
                          value={form.cover}
                          onChange={onChangeForm}
                          placeholder="Please enter the post cover"
                          aria-describedby="cover"
                        />
                        <div className="form-text">
                          The aspect ratio of the section cover is 16 x 9, for
                          example, 260 x 195
                        </div>
                        <div className="form-text">
                          Only cover URLs using the HTTP or HTTPS protocol are
                          supported
                        </div>
                        <div className="form-text">
                          The cover will not be displayed in all scenarios, but
                          only in specific scenarios
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
                        <label className="form-label">Tags</label>
                        <div className="card rounded-2">
                          <div className="card-body">
                            <SimpleDynamicInput
                              items={tags}
                              setItems={setTags}
                            />
                          </div>
                        </div>
                        <div className="form-text">
                          Appropriate tags can help organize and categorize
                          posts
                        </div>
                      </div>

                      <div>
                        <label className="form-label">
                          Overview
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
                          placeholder="Please enter the post overview"
                          aria-describedby="overview"
                        />
                        <div className="form-text">
                          The recommended length for an overview is around 160
                          words
                        </div>
                      </div>
                    </div>
                  </div>
                  <div className={clsx(split ? 'col-6' : '')}>
                    <label className="form-label">Content</label>
                    <div className="form-text mb-2">
                      You can publish an article with only a title or with
                      content. If you need a related topic, please select the
                      content
                    </div>
                    {editorInitializing && (
                      <div className="form-text mb-2">
                        Loading the editor...
                      </div>
                    )}
                    <div ref={editorElementRef}></div>
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
