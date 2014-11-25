{-Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и сохраняя каждое слагаемое
в журнал посредством монады Writer. В тексте программы допускается только один вызов функции tell.
-}

import Control.Monad.Writer
eps = 0.000001


taylor x prev result n = tell [prev] >> 
		if (abs(result - new) < eps) then return prev
		else taylor x (prev + new) new (n + 2)
	where
		new = -1 * result * x * x / ((n + 1) * (n + 2))
		
rad_to_deg x = (pi*x)/180
sin_new x =fst $ sin_temp $ rad_to_deg x
cos_new x =fst $ cos_temp $ rad_to_deg x

sin_temp x = runWriter $ taylor x x x 1
cos_temp x = runWriter $ taylor x 1 1 0

 

