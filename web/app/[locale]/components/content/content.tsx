import { createRoot } from 'react-dom/client';
import PhotoSwipeLightbox from 'photoswipe/lightbox';
import Img from '@/app/[locale]/components/content/img';

export default function Content({
  className,
  html,
}: {
  className?: string;
  html: string | undefined | null;
}) {
  function handleContentCallback(element: HTMLDivElement | null) {
    let lightbox: PhotoSwipeLightbox | undefined;
    if (element) {
      lightbox = handlePhotoSwipeLightbox(element);
    }

    if (!element) {
      lightbox?.destroy();
    }
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
      children: 'figure > a',
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
