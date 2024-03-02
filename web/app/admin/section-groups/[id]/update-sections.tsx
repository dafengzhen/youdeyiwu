'use client';

import Box from '@/app/admin/common/box';
import { type FormEvent, useContext, useState } from 'react';
import { GlobalContext } from '@/app/contexts';
import { useMutation } from '@tanstack/react-query';
import SimpleDynamicInput from '@/app/common/simple-dynamic-input';
import { nonNum } from '@/app/common/client';
import type { ISectionGroup } from '@/app/interfaces/section-groups';
import UpdateSectionsSectionGroupAction, {
  type IUpdateSectionsSectionGroupActionVariables,
} from '@/app/actions/sections/update-sections-section-group-action';

export default function UpdateSections({
  sectionGroup,
}: {
  sectionGroup: ISectionGroup;
}) {
  const { toast } = useContext(GlobalContext);
  const [sections, setSections] = useState<string[]>(
    sectionGroup.sections.map((item) => item.id + ''),
  );

  const updateSectionsSectionGroupActionMutation = useMutation({
    mutationFn: async (variables: {
      id: number;
      variables: IUpdateSectionsSectionGroupActionVariables;
    }) => {
      const response = await UpdateSectionsSectionGroupAction(variables);
      if (response.isError) {
        throw response;
      }
    },
  });

  async function onSubmit(e: FormEvent<HTMLFormElement>) {
    try {
      e.stopPropagation();
      e.preventDefault();

      const _sections = sections
        .filter((item) => item !== '' && !nonNum(item))
        .map((item) => parseInt(item));

      const id = sectionGroup.id;
      await updateSectionsSectionGroupActionMutation.mutateAsync({
        id,
        variables: {
          sections: _sections,
        },
      });

      toast.current.show({
        type: 'success',
        message: 'Sections updated successfully',
      });
    } catch (e: any) {
      updateSectionsSectionGroupActionMutation.reset();
      toast.current.show({
        type: 'danger',
        message: e.message,
      });
    }
  }

  return (
    <Box title={`${sectionGroup.name} (ID. ${sectionGroup.id})`}>
      <form className="vstack gap-4" onSubmit={onSubmit}>
        <div>
          <label className="form-label">Sections</label>
          <div className="card rounded-2">
            <div className="card-body">
              <SimpleDynamicInput
                items={sections}
                setItems={setSections}
                showSourceInfo={sectionGroup.sections}
              />
            </div>
          </div>
          <div className="form-text">
            Please enter the section ID. If you haven&apos;t created a section
            yet, please create a section first
          </div>
        </div>

        <div>
          <button
            disabled={updateSectionsSectionGroupActionMutation.isPending}
            type="submit"
            className="btn btn-success"
          >
            {updateSectionsSectionGroupActionMutation.isPending
              ? 'Updating'
              : 'Update Sections'}
          </button>
        </div>
      </form>
    </Box>
  );
}
