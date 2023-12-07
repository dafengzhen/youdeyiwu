import Link from 'next/link';

export default function Footer() {
  return (
    <footer className="p-4 mt-4 position-sticky">
      <aside>
        <p className="text-center">
          Built by&nbsp;
          <Link
            href="https://www.youdeyiwu.com"
            target="_blank"
            rel="noreferrer"
            className="link-offset-2 link-dark link-underline-opacity-25 link-underline-opacity-100-hover"
          >
            youdeyiwu
          </Link>
          . The source code is available on&nbsp;
          <Link
            href="https://github.com/dafengzhen/youdeyiwu"
            target="_blank"
            rel="noreferrer"
            className="link-offset-2 link-dark link-underline-opacity-25 link-underline-opacity-100-hover"
          >
            GitHub
          </Link>
          .
        </p>
      </aside>
    </footer>
  );
}
