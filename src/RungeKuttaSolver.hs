module RungeKuttaSolver
(
rungeKutta
) where 

rungeKutta :: Float ->(Float -> Float) -> Float -> Float
rungeKutta timeStep function prevValue = prevValue + timeStep*(k1+2*k2+2*k3+k4)/6
    where
        k1 = function prevValue
        k2 = function (prevValue + timeStep*k1/2)
        k3 = function (prevValue + timeStep*k2/2)
        k4 = function ( prevValue +timeStep*k3)