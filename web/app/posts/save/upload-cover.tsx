import {
  type ChangeEvent,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import { useMutation } from '@tanstack/react-query';
import Image from 'next/image';
import UploadCoverPostAction, {
  type IUploadCoverPostActionVariables,
} from '@/app/actions/posts/upload-cover-post-action';
import { GlobalContext } from '@/app/contexts';

export default function UploadCover({
  id,
  callback,
}: {
  id?: number;
  callback: () => void;
}) {
  const { toast } = useContext(GlobalContext);
  const [form, setForm] = useState<{
    uploadCover: string;
    uploadCoverObjectUrl: string;
  }>({
    uploadCover: '',
    uploadCoverObjectUrl: '',
  });
  const uploadCoverFile = useRef<File | null>(null);

  const uploadCoverPostActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUploadCoverPostActionVariables;
    }) => {
      const response = await UploadCoverPostAction(variables);
      if (response.isError) {
        throw response;
      }
    },
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
          message: 'The anonymous article cannot be uploaded',
        });
        return;
      }

      const file = uploadCoverFile.current;
      if (!file) {
        toast.current.show({
          type: 'danger',
          message: 'Please upload files locally first',
        });
        return;
      }

      const formData = new FormData();
      formData.append('file', file);
      await uploadCoverPostActionMutation.mutateAsync({
        id,
        variables: { formData },
      });

      callback();

      toast.current.show({
        type: 'success',
        message: 'Successfully uploaded',
      });

      setTimeout(() => {
        toast.current.show({
          type: 'success',
          message:
            'Upload successful. Changes will take effect after clicking the "Update" button',
        });
      }, 1500);
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
        return;
      }

      const file = files[0];
      if (!file) {
        return;
      }

      const objectUrl = URL.createObjectURL(file);
      setForm({
        ...form,
        uploadCover: value,
        uploadCoverObjectUrl: objectUrl,
      });
      uploadCoverFile.current = file;
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
      uploadCoverObjectUrl: '',
    });

    uploadCoverFile.current = null;
  }

  return (
    <div>
      <div className="input-group">
        <input
          type="file"
          accept="image/jpg,image/png"
          className="form-control"
          name="uploadCover"
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
        Only JPG Or PNG format cover image files are supported, with a size of
        up to 1MB
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
