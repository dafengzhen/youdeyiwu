'use client';

import type { TUsersCountByDate } from '@/app/[locale]/interfaces/users';
import { useEffect, useRef } from 'react';
import Chart from '@/app/[locale]/common/chart';
import type { ChartData, ChartOptions } from 'chart.js';
import { useTranslations } from 'next-intl';

export default function Admin({
  usersCountByDate,
}: {
  usersCountByDate: TUsersCountByDate;
}) {
  const canvas = useRef<HTMLCanvasElement>(null);
  const chartRef = useRef<Chart<'line', any, any>>();
  const t = useTranslations();

  useEffect(() => {
    const current = canvas.current;
    if (current) {
      chartRef.current = new Chart(current, {
        type: 'line',
        data: {
          labels: usersCountByDate.map((item) => item.date),
          datasets: [
            {
              label: 'Count',
              data: usersCountByDate.map((item) => item.count),
              pointRadius: 6,
              pointHoverRadius: 10,
              stepped: true,
            },
          ],
        } as ChartData<'line', any>,
        options: {
          responsive: true,
          interaction: {
            intersect: false,
            axis: 'x',
          },
          plugins: {
            title: {
              display: false,
              text: 'Statistics of User Registrations in the Past 15 Days',
            },
          },
          scales: {
            x: {
              display: true,
              title: {
                display: false,
              },
            },
            y: {
              display: true,
              title: {
                display: false,
              },
            },
          },
        } as ChartOptions<'line'>,
      });
    }

    return () => {
      if (current) {
        chartRef.current?.destroy();
      }
    };
  }, [canvas.current]);

  return (
    <div className="row mx-0">
      <div className="col">
        <div className="row">
          <div className="col-2"></div>
          <div className="col">
            <div className="card rounded-2">
              <div className="card-header bg-transparent border-bottom-0">
                <div className="fw-bold">
                  {t('common.userRegistrationStatisticsForTheLast15Days')}
                </div>
              </div>
              <div className="card-body">
                <canvas ref={canvas}></canvas>
              </div>
            </div>
          </div>
          <div className="col-2"></div>
        </div>
      </div>
    </div>
  );
}
