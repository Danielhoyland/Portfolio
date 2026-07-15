import React from 'react';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Title,
  Tooltip,
  Legend,
  defaults
} from 'chart.js';
import { Line } from 'react-chartjs-2';
//import faker from 'faker';
import type { ChartData, ChartOptions } from 'chart.js';
import zoomPlugin from 'chartjs-plugin-zoom';

defaults.responsive = true

ChartJS.register(
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  Tooltip,
  Title,
  Legend,
  zoomPlugin
);

//why is this required
function getX() {
  var p: 'x' | 'y' |'xy'
  p = 'x'
  return p
}

export const options = {
  responsive: true,
  plugins: {
    legend: {
      position: 'top' as const,
    },
    title: {
      display: true,
      text: 'Overview',
    },
    zoom: {
      pan: {
        enabled: true,
        mode: getX()
      },
      zoom: {
        wheel: {
          enabled: true
        },
        pinch: {
          enabled: true
        },
        mode: getX(),
      }
    },
    tooltip: {
      callbacks: {
          label: function(context: any) {
              let label = context.dataset.label || '';

              if (label) {
                  label += ': ';
              }
              if (context.parsed.y !== null) {
                  label += context.parsed.y + " Watts";
              }
              return label;
          }
      }
    },
  },
  scales: {
    y: {
        min:0,
        suggestedMax: 100,
        ticks: {
            beginAtZero: true,
            // Include a dollar sign in the ticks
            callback: function(value: any) {
                return value + " W";
            }
        }
    }
  }
};

interface LineProps {
    options?: ChartOptions<'line'>;
    data: ChartData<'line'>;
}

const LineChart: React.FC<LineProps> = ({ data }) => {
    return (
        <Line options={options} data={data}/>
    )
}

export default LineChart;