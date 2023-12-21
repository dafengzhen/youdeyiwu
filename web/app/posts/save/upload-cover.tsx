import { type ChangeEvent, useContext, useEffect, useState } from 'react';
import { useMutation } from '@tanstack/react-query';
import Image from 'next/image';
import UploadCoverPostAction from '@/app/actions/posts/upload-cover-post-action';
import { GlobalContext } from '@/app/contexts';

export default function UploadCover({
  id,
  uploadCoverUrl,
  setUploadCoverUrl,
}: {
  id?: number;
  uploadCoverUrl?: string;
  setUploadCoverUrl: (uploadCoverUrl: string) => void;
}) {
  const isEdit = !!id;
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    uploadCover: string;
    uploadCoverFile?: File;
    uploadCoverObjectUrl: string;
  }>({
    uploadCover: '',
    uploadCoverObjectUrl: '',
  });

  const uploadCoverPostActionMutation = useMutation({
    mutationFn: UploadCoverPostAction,
  });

  useEffect(() => {
    return () => {
      if (form.uploadCoverObjectUrl) {
        URL.revokeObjectURL(form.uploadCoverObjectUrl);
      }
    };
  }, [form.uploadCoverObjectUrl]);

  async function onClickUploadCover() {
    try {
      if (!id) {
        toast.current.show({
          type: 'danger',
          message:
            'Failed to upload cover image file, article id does not exist',
        });
        return;
      }

      const file = form.uploadCoverFile;
      if (!file) {
        toast.current.show({
          type: 'danger',
          message: 'Please upload files locally first',
        });
        return;
      }

      await uploadCoverPostActionMutation.mutateAsync({
        id,
        variables: { file },
      });

      toast.current.show({
        type: 'success',
        message: 'Successfully uploaded',
      });
    } catch (e: any) {
      uploadCoverPostActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  function onChange(e: ChangeEvent<HTMLInputElement>) {
    const name = e.target.name;
    const value = e.target.value;

    if (name === 'uploadCover') {
      if (form.uploadCoverObjectUrl) {
        URL.revokeObjectURL(form.uploadCoverObjectUrl);
      }

      const files = e.target.files;
      if (!files) {
        toast.current.show({
          type: 'danger',
          message: 'Please upload files',
        });
        return;
      }

      const file = files[0];
      if (!file) {
        toast.current.show({
          type: 'danger',
          message: 'The file does not exist, please try uploading again',
        });
        return;
      }

      const objectUrl = URL.createObjectURL(file);
      setForm({
        ...form,
        uploadCover: value,
        uploadCoverFile: file,
        uploadCoverObjectUrl: objectUrl,
      });
    } else {
      setForm({
        ...form,
        [name]: value,
      });
    }
  }

  function onClickClose() {
    if (form.uploadCoverObjectUrl) {
      URL.revokeObjectURL(form.uploadCoverObjectUrl);
    }

    setForm({
      ...form,
      uploadCover: '',
      uploadCoverFile: undefined,
      uploadCoverObjectUrl: '',
    });
  }

  return (
    <div>
      <div className="input-group">
        <input
          type="file"
          accept="image/png"
          className="form-control"
          name="uploadCover"
          value={form.uploadCover}
          onChange={onChange}
        />
        <button
          disabled={uploadCoverPostActionMutation.isPending}
          onClick={onClickUploadCover}
          className="btn btn-secondary"
          type="button"
        >
          <i className="bi bi-upload me-2"></i>
          {uploadCoverPostActionMutation.isPending ? 'Uploading' : 'Upload'}
        </button>
      </div>

      <div className="form-text">
        Only PNG format cover image files are supported, with a size of up to
        1MB
      </div>
      <div className="form-text">
        Alternatively, you can choose to upload a cover image from your local
        device
      </div>

      {form.uploadCoverObjectUrl && (
        <div
          className="mt-2 position-relative"
          style={{ width: 260, height: 195 }}
        >
          <div className="ratio ratio-16x9" style={{ width: 260, height: 195 }}>
            <Image
              width={260}
              height={195}
              src={form.uploadCoverObjectUrl}
              alt="cover"
              className="rounded object-fit-cover"
            />
          </div>

          <i
            onClick={onClickClose}
            className="bi bi-x-lg text-danger position-absolute top-0 start-100 translate-middle fs-4 cursor-pointer"
          ></i>
        </div>
      )}
    </div>
  );
}
