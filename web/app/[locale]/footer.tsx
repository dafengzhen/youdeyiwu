import Link from 'next/link';
import { getTranslations } from 'next-intl/server';

export default async function Footer({ locale }: { locale: string }) {
  const t = await getTranslations({ locale });

  return (
    <footer className="p-4 mt-4">
      <aside>
        <p className="text-center">
          {t.rich('common.footerFormText', {
            name: (chunks) => (
              <Link
                href="https://www.youdeyiwu.com"
                target="_blank"
                rel="noreferrer"
                className="link-offset-2 link-dark link-underline-opacity-25 link-underline-opacity-100-hover"
              >
                {chunks}
              </Link>
            ),
            link: (chunks) => (
              <Link
                href="https://github.com/dafengzhen/youdeyiwu"
                target="_blank"
                rel="noreferrer"
                className="link-offset-2 link-dark link-underline-opacity-25 link-underline-opacity-100-hover"
              >
                {chunks}
              </Link>
            ),
          })}
        </p>
      </aside>
    </footer>
  );
}
