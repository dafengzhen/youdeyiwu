import Code from '@/app/[locale]/components/content/code';
import clsx from 'clsx';

export default function CodeBlockPreview({
  value,
  language,
  classs,
}: {
  value: string;
  language: string;
  classs?: string;
}) {
  return <Code value={value} classs={clsx(classs, `language-${language}`)} />;
}
