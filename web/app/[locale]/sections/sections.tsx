'use client';

import Link from 'next/link';
import { ISectionGroup } from '@/app/[locale]/interfaces/section-groups';
import { ISection } from '@/app/[locale]/interfaces/sections';
import Nodata from '@/app/[locale]/common/nodata';
import { getUserAlias } from '@/app/[locale]/common/client';
import Image from 'next/image';

export default function Sections({
  sectionGroups,
  sections,
}: {
  sectionGroups: ISectionGroup[];
  sections: ISection[];
}) {
  return (
    <div className="row mx-0">
      <div className="col">
        <div className="d-flex flex-column gap-4">
          {sectionGroups
            .filter((item) => item.sections.length)
            .map((item) => {
              return (
                <div key={item.id} className="card border-0">
                  <div className="card-header bg-transparent border-bottom-0">
                    <div className="d-flex align-items-center justify-content-between">
                      <div className="h4 text-secondary">{item.name}</div>
                      <div>
                        <i className="bi bi-arrow-right fs-4 text-secondary text-opacity-50"></i>
                      </div>
                    </div>
                  </div>
                  <div className="card-body">
                    <div className="row row-cols-auto">
                      <Items sections={item.sections} />
                    </div>
                  </div>
                </div>
              );
            })}

          {!!sections.length && (
            <div className="card border-0">
              {sectionGroups.filter((item) => item.sections.length).length >
                0 && (
                <div className="card-header bg-transparent border-bottom-0">
                  <div className="d-flex align-items-center justify-content-between">
                    <div className="h4">Others</div>
                    <div>
                      <i className="bi bi-arrow-right fs-4 text-secondary text-opacity-25"></i>
                    </div>
                  </div>
                </div>
              )}
              <div className="card-body">
                <div className="row row-cols-auto">
                  <Items sections={sections} />
                </div>
              </div>
            </div>
          )}

          {sectionGroups.length === 0 && sections.length === 0 && <Nodata />}
        </div>
      </div>
    </div>
  );
}

const Items = ({ sections }: { sections: ISection[] }) => {
  return (
    <>
      {sections.map((section) => {
        return (
          <div key={section.id} className="col" style={{ maxWidth: 330 }}>
            <div className="card yw-card shadow-sm shadow-hover">
              <div className="card-body d-flex flex-column p-0">
                {section.cover ? (
                  <div className="p-3">
                    <Link href={`/sections/${section.id}`} scroll={false}>
                      <Image
                        className="rounded-top object-fit-contain image-hover cursor-pointer"
                        width={260}
                        height={195}
                        src={section.cover}
                        alt="cover"
                      />
                    </Link>
                  </div>
                ) : (
                  section.overview && (
                    <Link
                      className="line-clamp-3 card-text text-reset text-decoration-none p-3"
                      href={`/sections/${section.id}`}
                      scroll={false}
                    >
                      {section.overview}
                    </Link>
                  )
                )}

                {!!section.admins.length && (
                  <div className="text-secondary p-3">
                    <div className="">Admins</div>
                    <div className="mt-2 d-flex flex-wrap align-items-center column-gap-2">
                      {section.admins.map((admin) => {
                        return (
                          <Link
                            key={admin.id}
                            href={`/users/${admin.id}`}
                            className="link-body-emphasis link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                          >
                            {getUserAlias(admin)}
                          </Link>
                        );
                      })}
                    </div>
                  </div>
                )}

                <div className="px-3 py-2">
                  <Link
                    href={`/sections/${section.id}`}
                    className="link-primary link-offset-2 link-underline-opacity-0 link-underline-opacity-100-hover"
                    scroll={false}
                  >
                    {section.name}
                    <i
                      className="bi bi-arrow-up-short align-middle d-inline-block fs-4"
                      style={{ transform: 'rotate(45deg)' }}
                    ></i>
                  </Link>
                </div>
              </div>
            </div>
          </div>
        );
      })}
    </>
  );
};
