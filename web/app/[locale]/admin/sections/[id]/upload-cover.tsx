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
import UploadCoverSectionAction, {
  type IUploadCoverSectionActionVariables,
} from '@/app/[locale]/actions/sections/upload-cover-section-action';
import { useTranslations } from 'next-intl';

export default function UploadCover({
  id,
  callback,
}: {
  id: number;
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
  const t = useTranslations();

  const uploadCoverSectionActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUploadCoverSectionActionVariables;
    }) => {
      const response = await UploadCoverSectionAction(variables);
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
      const file = uploadCoverFile.current;
      if (!file) {
        toast.current.show({
          type: 'danger',
          message: t('common.theFileCannotBeEmpty'),
        });
        return;
      }

      const formData = new FormData();
      formData.append('file', file);
      await uploadCoverSectionActionMutation.mutateAsync({
        id,
        variables: { formData },
      });

      callback();

      toast.current.show({
        type: 'success',
        message: t('common.uploadedSuccessfully'),
      });
    } catch (e: any) {
      uploadCoverSectionActionMutation.reset();
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
          disabled={uploadCoverSectionActionMutation.isPending}
          onClick={onClickUploadCover}
          className="btn btn-secondary"
          type="button"
        >
          <i className="bi bi-upload me-2"></i>
          {uploadCoverSectionActionMutation.isPending
            ? t('common.uploading')
            : t('common.upload')}
        </button>
      </div>

      <div className="form-text">{t('common.coverFormText4')}</div>

      {form.uploadCoverObjectUrl && (
        <div
          className="mt-4 mb-2 position-relative"
          style={{ width: 260, height: 195 }}
        >
          <div className="ratio ratio-16x9">
            <Image
              width={260}
              height={195}
              src={form.uploadCoverObjectUrl}
              alt="cover"
              className="rounded object-fit-contain"
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
