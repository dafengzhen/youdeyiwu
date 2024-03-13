import clsx from 'clsx';
import styles from '@/app/[locale]/home/home.module.scss';

export default function EmptyBox() {
  return <div className={clsx(styles.emptyBox)}></div>;
}
