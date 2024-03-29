import Link from 'next/link';
import { getTranslations } from 'next-intl/server';

export default async function Footer({ locale }: { locale: string }) {
  const t = await getTranslations({ locale });
  const customFooter = process.env.CUSTOM_FOOTER;

  return (
    <footer className="p-4 mt-4">
      <aside>
        <hr className="mb-4 text-secondary opacity-25 w-25 mx-auto" />
        {customFooter ? (
          <div dangerouslySetInnerHTML={{ __html: customFooter }}></div>
        ) : (
          <p className="text-center text-body-secondary">
            {t.rich('common.footerFormText', {
              name: (chunks) => (
                <Link
                  href="https://www.youdeyiwu.com"
                  target="_blank"
                  rel="noreferrer"
                  className="link-offset-2 text-body-secondary link-underline-opacity-25 link-underline-opacity-100-hover"
                >
                  {chunks}
                </Link>
              ),
              link: (chunks) => (
                <Link
                  href="https://github.com/dafengzhen/youdeyiwu"
                  target="_blank"
                  rel="noreferrer"
                  className="link-offset-2 text-body-secondary link-underline-opacity-25 link-underline-opacity-100-hover"
                >
                  {chunks}
                </Link>
              ),
            })}
          </p>
        )}
      </aside>
    </footer>
  );
}
