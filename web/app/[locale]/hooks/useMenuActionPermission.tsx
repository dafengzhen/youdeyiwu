import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import MenusUserAction from '@/app/[locale]/actions/users/menus-user-action';
import { useTranslations } from 'next-intl';

function useMenuActionPermission(link: string, name: string) {
  const [isActionDisabled, setIsActionDisabled] = useState(false);

  const menuQuery = useQuery({
    queryKey: ['/users', '/menus'],
    queryFn: async () => {
      const response = await MenusUserAction();
      if (response.isError) {
        throw response;
      }

      return response.data;
    },
  });

  useEffect(() => {
    if (menuQuery.data) {
      const menus = menuQuery.data;
      let find = menus.find((item) => item.link === link);
      if (!find) {
        const findSubmenu = menus
          .filter((item) => item.submenus.length > 0)
          .find((item) =>
            item.submenus.find((submenuItem) => submenuItem.link === link),
          );

        if (!findSubmenu) {
          setIsActionDisabled(true);
          return;
        }

        find = findSubmenu;
      }

      const findAction = find.actions.find((item) => item.name === name);
      setIsActionDisabled(!findAction);
    } else {
      setIsActionDisabled(true);
    }
  }, [menuQuery.data, link, name]);

  return {
    isActionDisabled,
    AccessDeniedAlert: isActionDisabled
      ? AccessDeniedAlert
      : AccessGrantedAlert,
  };
}

function AccessDeniedAlert() {
  const t = useTranslations();

  return (
    <div className="form-text text-danger">
      {t('common.sorryYouDontHaveAccessToThisButtonYet')}
    </div>
  );
}

function AccessGrantedAlert() {
  return <></>;
}

export default useMenuActionPermission;
