import {
  type ChangeEvent,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import { useMutation } from '@tanstack/react-query';
import Image from 'next/image';
import { GlobalContext } from '@/app/[locale]/contexts';
import { useTranslations } from 'next-intl';
import UploadImageFileAction from '@/app/[locale]/actions/files/upload-image-file-action';
import type { IFileUrls } from '@/app/[locale]/interfaces/file';

export default function UploadAvatar({
  callback,
}: {
  callback: (value: IFileUrls) => void;
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
  const t = useTranslations();

  const uploadImageFileActionMutation = useMutation({
    mutationFn: async (variables: { formData: FormData }) => {
      const response = await UploadImageFileAction(variables);
      if (response.isError) {
        throw response;
      }

      return response.data;
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
      const file = uploadCoverFile.current;
      if (!file) {
        toast.current.show({
          type: 'danger',
          message: t('common.theFileCannotBeEmpty'),
        });
        return;
      }

      const formData = new FormData();
      formData.append('upload', file);
      const response = await uploadImageFileActionMutation.mutateAsync({
        formData,
      });

      callback(response);

      toast.current.show({
        type: 'success',
        message: t('common.uploadedSuccessfully'),
      });
    } catch (e: any) {
      uploadImageFileActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.error ?? e.message,
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
          disabled={uploadImageFileActionMutation.isPending}
          onClick={onClickUploadCover}
          className="btn btn-secondary"
          type="button"
        >
          <i className="bi bi-upload me-2"></i>
          {uploadImageFileActionMutation.isPending
            ? t('common.uploading')
            : t('common.upload')}
        </button>
      </div>

      <div className="form-text">{t('common.avatarFormText')}</div>

      {form.uploadCoverObjectUrl && (
        <div
          className="mt-4 mb-2 position-relative img-thumbnail rounded-circle"
          style={{ width: 200, height: 200 }}
        >
          <Image
            width={200}
            height={200}
            src={form.uploadCoverObjectUrl}
            alt="avatar"
            className="rounded-circle object-fit-contain"
          />

          <i
            onClick={onClickClose}
            className="bi bi-x-lg text-danger position-absolute top-0 start-100 translate-middle fs-4 cursor-pointer"
          ></i>
        </div>
      )}
    </div>
  );
}
