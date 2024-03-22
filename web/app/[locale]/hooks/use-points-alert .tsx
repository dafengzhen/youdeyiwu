import { useContext } from 'react';
import { GlobalContext } from '@/app/[locale]/contexts';

function usePointsAlert() {
  const { pointsAlert } = useContext(GlobalContext);
  return pointsAlert.current;
}

export default usePointsAlert;
