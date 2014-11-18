{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}
type Alphabet = [Char]
type State = Int
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect (a,s,i,t,acc)= (not (null a)) && (not (null s)) && (not (null acc)) && (elem i s)

-- в дальнейшем может пригодиться функция whileM,
-- которая собирает результаты в список, пока истинно
-- заданное условие
whileM :: m Bool -> m a -> m [a]
whileM = undefined

-- Напишите функцию, определяющую, допускает ли НКА заданное слово 
accept :: NFA -> String -> Bool
accept = undefined

-- Постройте ещё как минимум три примера НКА
--abab...
nfa1 :: NFA
nfa1 = (['a','b'], [1, 2, 3], 1, trans, [3])
 where
    trans 1 'a' = [2]
    trans 1 _ = []
    trans 2 'b' = [3]
    trans 2 _ = []
    trans 3 'a' = [1]
    trans 3 _ = []
    
---a*b*c    
nfa2 :: NFA
nfa2 = (['a','b','c'], [1, 2, 3], 1, trans, [3])
 where
    trans 1 'a' = [1]
    trans 1 'b' = [2]
    trans 1 _ = []
    trans 2 'b' = [2]
    trans 2 'c' = [3]
    trans 2 _ = []
    trans 3 'c' = []
    trans 3 _ = [1]
--ba(c)*ab
nfa3 :: NFA
nfa3= (['a','b','c'], [1, 2, 3,4,5], 1, trans, [5])
 where
    trans 1 'b' = [2]
    trans 1 _ = []
    trans 2 'a' = [3]
    trans 2 _ = []
    trans 3 'c' = [3]
    trans 3 'a' = [4]
    trans 3 'b' = []
    trans 4 'b' = [5]
    trans 4 _ = []
    trans 5 'b' = []
    trans 5 _ = [1]

{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify = undefined
