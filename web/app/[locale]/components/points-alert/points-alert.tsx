/* eslint-disable react-hooks/exhaustive-deps */

import { forwardRef, useEffect, useImperativeHandle, useState } from 'react';
import { gsap } from 'gsap';
import { nanoid } from 'nanoid';
import { useQuery } from '@tanstack/react-query';
import QueryAllMessageAction from '@/app/[locale]/actions/messages/query-all-message-action';
import { nonNum } from '@/app/[locale]/common/client';

export interface IPointsAlert {
  id: string;
  value: number;
  actualValue: number;
  show: boolean;
  displayed: boolean;
  ref: HTMLDivElement | null;
  sign: HTMLElement | null;
  flag: '+' | '-';
}

export interface IPointsAlertRef {
  add: (item: Pick<IPointsAlert, 'value' | 'actualValue' | 'flag'>) => void;
}

const pointMessagesDisplayedKey = '_youdeyiwu_point_messages_displayed';

export default forwardRef(function PointsAlert(props, ref) {
  const [items, setItems] = useState<IPointsAlert[]>([]);
  const [updateTrigger, setUpdateTrigger] = useState(items.length);

  useImperativeHandle(ref, () => ({
    add,
  }));

  const messageQuery = useQuery({
    queryKey: ['/messages'],
    queryFn: async () => {
      const response = await QueryAllMessageAction();
      if (response.isError) {
        throw response;
      }
      return response.data;
    },
  });

  useEffect(() => {
    function extractValuesFromBrackets(message: string) {
      const increased = message.match(/Increased by \[ (\d+) ]/);
      const decreased = message.match(/decreased by \[ (\d+) ]/);
      const remaining = message.match(/remaining points are \[ (\d+) ]/);
      const source = message.match(/source of the points is \[ (.+) ]/);

      return {
        increasedBy: increased ? increased[1] : null,
        decreasedBy: decreased ? decreased[1] : null,
        remainingPoints: remaining ? remaining[1] : null,
        sourceOfPoints: source ? source[1] : null,
      };
    }

    if (messageQuery.data) {
      const data = messageQuery.data;
      const filter = data.content.filter(
        (item) => item.name === 'Changes in your points',
      );
      const pointMessagesDisplayedItem = localStorage.getItem(
        pointMessagesDisplayedKey,
      );
      let pointMessagesDisplayed: any[] = [];

      if (pointMessagesDisplayedItem) {
        try {
          const parse = JSON.parse(pointMessagesDisplayedItem);
          if (Array.isArray(parse)) {
            pointMessagesDisplayed = parse;
          }
        } catch (e) {}
      } else {
        localStorage.setItem(
          pointMessagesDisplayedKey,
          JSON.stringify(
            filter.map((item) => {
              return {
                id: item.id,
              };
            }),
          ),
        );
      }

      filter
        .filter(
          (item) =>
            !pointMessagesDisplayed.find((_item) => _item.id === item.id),
        )
        .forEach((item) => {
          const output = extractValuesFromBrackets(item.overview);
          if (
            typeof output.remainingPoints === 'string' &&
            !nonNum(output.remainingPoints)
          ) {
            const value = parseInt(output.remainingPoints);
            const flag = output.increasedBy === '0' ? '-' : '+';
            const actualValue =
              flag === '+'
                ? parseInt(output.increasedBy ?? '0')
                : parseInt(output.decreasedBy ?? '0');

            add({
              value,
              actualValue,
              flag,
            });
          }
        });
    }
  }, [messageQuery.data]);

  useEffect(() => {
    items
      .filter((item) => !item.displayed)
      .forEach((item) => {
        if (!item.ref || !item.sign) {
          return;
        }

        setItems((prevItems) => {
          const updatedItems = [...prevItems];
          const find = updatedItems.find((_item) => _item.id === item.id);
          if (find) {
            find.displayed = true;
          }
          return updatedItems;
        });

        const element = item.ref;
        const signElement = item.sign;
        const t1 = gsap.timeline();
        const obj = { value: 0 };

        t1.fromTo(
          element,
          { autoAlpha: 0, height: '0rem' },
          {
            x: -0,
            height: '5rem',
            autoAlpha: 1,
            duration: 1,
            ease: 'power1.in',
            onComplete: () => {
              setItems((prevItems) => {
                const updatedItems = [...prevItems];
                const find = updatedItems.find((_item) => _item.id === item.id);
                if (find) {
                  find.show = true;
                }
                return updatedItems;
              });
            },
          },
        )
          .to(obj, {
            value: item.value,
            duration: 2,
            ease: 'circ.out',
            onUpdate: () => {
              setItems((prevItems) => {
                const updatedItems = [...prevItems];
                const find = updatedItems.find((_item) => _item.id === item.id);
                if (find) {
                  find.value = Math.round(obj.value);
                }
                return updatedItems;
              });
            },
            onComplete: () => {
              element.classList.add('point-clippath-hover');
            },
          })
          .fromTo(
            element,
            { autoAlpha: 1 },
            {
              autoAlpha: 0,
              duration: 2,
              delay: 2,
              onComplete: () => {
                t1.kill();
                setItems((prevItems) => {
                  return [...prevItems].filter((_item) => _item.id !== item.id);
                });
              },
            },
          );

        t1.add(() => {
          gsap.to(signElement, {
            rotation: 360,
            ease: 'circ.out',
            duration: 2,
          });
        }, 1);
      });
  }, [updateTrigger]);

  function add(item: Pick<IPointsAlert, 'value' | 'actualValue' | 'flag'>) {
    setItems((prevItems) => {
      return [
        {
          ...item,
          id: nanoid(),
          show: false,
          displayed: false,
          ref: null,
          sign: null,
        },
        ...prevItems,
      ];
    });
    setUpdateTrigger((prevState) => prevState + 1);
  }

  return (
    <div className="toast-container top-0 end-0 p-3 overflow-hidden position-fixed">
      {items.map((item, index) => {
        return (
          <div
            key={item.id}
            ref={(instance) => {
              item.ref = instance;
            }}
            className="toast show align-items-center border-0 point-clippath text-success"
            role="points"
          >
            <div className="toast-body">
              <div className="d-flex justify-content-between align-items-center flex-wrap gap-2 lead">
                <div>
                  <div className="d-flex align-items-center gap-1">
                    <i className="bi bi-check2-circle"></i>
                    {item.flag === '+' && (
                      <i
                        ref={(instance) => {
                          item.sign = instance;
                        }}
                        className="bi bi-plus"
                      ></i>
                    )}
                    {item.flag === '-' && (
                      <i
                        ref={(instance) => {
                          item.sign = instance;
                        }}
                        className="bi bi-dash text-warning-emphasis"
                      ></i>
                    )}
                    {/*<i className="fs-6 mb-1">{item.actualValue}</i>*/}
                  </div>
                  <div className="d-flex align-content-center gap-2">
                    {item.show && <div className="fw-medium">{item.value}</div>}
                  </div>
                </div>
                {item.flag === '+' && <div>Points have increased</div>}
                {item.flag === '-' && <div>Points have decreased</div>}
              </div>
            </div>
          </div>
        );
      })}
    </div>
  );
});
