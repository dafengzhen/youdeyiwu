import { createRoot } from 'react-dom/client';
import PhotoSwipeLightbox from 'photoswipe/lightbox';
import Img from '@/app/[locale]/components/content/img';
import Code from '@/app/[locale]/components/content/code';

export default function Content({
  className,
  html,
}: {
  className?: string;
  html: string | undefined | null;
}) {
  // useRef<Map<string, any>>(new Map());

  function handleContentCallback(element: HTMLDivElement | null) {
    let lightbox: PhotoSwipeLightbox | undefined;
    if (element) {
      handleHighlightCode(element);
      lightbox = handlePhotoSwipeLightbox(element);
    }

    if (!element) {
      lightbox?.destroy();
    }
  }

  function handleHighlightCode(element: HTMLDivElement) {
    element.querySelectorAll('pre code').forEach((value) => {
      if (value) {
        const preElement = document.createElement('pre');
        const root = createRoot(preElement);
        root.render(<Code value={value as HTMLElement} />);
        value.parentElement?.replaceWith(preElement);
      }
    });
  }

  function handlePhotoSwipeLightbox(element: HTMLDivElement) {
    element.querySelectorAll('img').forEach((value) => {
      if (value) {
        const figureElement = document.createElement('figure');
        figureElement.classList.add('figure');
        const root = createRoot(figureElement);
        root.render(<Img className="figure-img" value={value} />);
        value.parentElement?.replaceChild(figureElement, value);
      }
    });

    const lightbox = new PhotoSwipeLightbox({
      gallery: element,
      children: 'a',
      pswpModule: () => import('photoswipe'),
    });
    lightbox.init();
    return lightbox;
  }

  return (
    <div
      ref={handleContentCallback}
      className={className}
      dangerouslySetInnerHTML={{ __html: html ?? '' }}
    ></div>
  );
}
