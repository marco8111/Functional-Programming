module TP1 where

-- TIPOS DE DADOS #################################################################################

-- Informação nutricional de um dado alimento
-- (nome do alimento,valor calórico, quantidade de lípidos, quantidade de glícidos)
type InfoNutricional=(String,Int,Float,Float)

-- Constituição de uma refeição
-- Lista de pares (nome do alimento,quantidade usada na refeição)
type Refeicao=[(String,Int)]


-- CONSTANTES #####################################################################################

almoco::Refeicao
almoco=[("carne",120),("arroz",200),("pao",50)]

dispensa::[InfoNutricional]
dispensa=[("carne",525,25,10),("arroz",425,5,90),("pao",400,5,80)]


-- FUNÇÕES ########################################################################################

-------------------------------------------------------------
-- calcula o alimento mais calórico de uma lista de alimentos
maisCalorico::[InfoNutricional]->InfoNutricional
maisCalorico []=error "Lista de alimentos vazia"
maisCalorico [x]=x			-- se só existe um alimento na lista o resultado e o próprio
maisCalorico ((n,c,l,g):xs)=
    let (n1,c1,l1,g1)=maisCalorico xs	-- calculamos o mais calórico da cauda da lista
    in if (c>c1) then (n,c,l,g)
                 else (n1,c1,l1,g1)

{-
-- (sem 'let'; recorrem à função snd4, definida no final do documento)
-- versão 2
maisCalorico2 []=error "Lista de alimentos vazia"
maisCalorico2 [x]=x
maisCalorico2 (x:xs)=if ((snd4 x)>(snd4 (maisCalorico2 xs))) then x
                     else maisCalorico2 xs

-- versão 3
maisCalorico3 []=error "Lista de alimentos vazia"
maisCalorico3 [x]=x
maisCalorico3 (x:xs)| ((snd4 x)>(snd4 (maisCalorico3 xs))) = x
                    | otherwise = maisCalorico3 xs
-}


---------------------------------------------------------
-- determina a informação existente para um dado alimento
getInfoNutricional::[InfoNutricional]->String->InfoNutricional
getInfoNutricional [] _=error "Alimento inexistente"
getInfoNutricional ((n,c,l,p):xs) n1
    |n==n1=(n,c,l,p)			-- se o alimento é o que está no topo da lista da informação nutricional devolve essa informação
    |otherwise=getInfoNutricional xs n1	-- caso contrário continua à procura do alimento na cauda da lista


----------------------------------------------
-- calcula o total de calorias de uma refeição
-- ATENÇÃO!!!, como as calorias são inteiros, só podemos utilizar a função 'div'
-- este problema pode ser "resolvido" recorrendo às funções 'fromInteger' e 'round' (versao 2 e 3)
totalCalorias::[InfoNutricional]->Refeicao->Int
totalCalorias _ []=0
totalCalorias l ((n,q):xs)=
    let (n1,c1,l1,g1)=getInfoNutricional l n	-- determina a informação de um alimento
    in ((div c1 100)*q)+(totalCalorias l xs)	-- calcula as calorias do alimento n para a quantidade q ((div c1 100)*q); soma ao resultado de calcular as calorias do resto da lista

{-
-- versão 2
totalCalorias::[InfoNutricional]->Refeicao->Int
totalCalorias _ []=0
totalCalorias l ((n,q):xs)=
    let (n1,c1,l1,g1)=getInfoNutricional l n
    in (cal (c1,q))+(totalCalorias l xs)

cal::(Int,Int)->Int
cal (x,y)=round (cal2 (fromIntegral x,fromIntegral y))

cal2::(Float,Float)->Float
cal2 (x,y)=(x/100)*y

-- versão 3
totalCalorias l r=
    let x=totalCaloriasAux l r
    in cal (x,1)

totalCaloriasAux::[InfoNutricional]->Refeicao->Int
totalCaloriasAux _ []=0
totalCaloriasAux l ((n,q):xs)=
    let (n1,c1,l1,g1)=getInfoNutricional l n
    in (c1*q)+(totalCaloriasAux l xs)

-- em vez de fazermos a divisão para cada um dos elementos da refeição, fazemos apenas para o resultado final
--  (x1/100)+(x2/100)+...+(xn/100)=(x1+x2+...+xn)/100

-- em princípio, a melhor é a versão 3, mas para a usarem é necessário compreender bem a função 'cal'
-}



---------------------------------------------------------------------------------------
-- calcula os alimentos de uma refeição com um teor de lípidos inferior a um valor dado
saudaveis_rec::Float->[InfoNutricional]->Refeicao->[String]
saudaveis_rec m l []=[]
saudaveis_rec m l ((a,_):xs)=
    if (saudavel m a l) then (a:(saudaveis_rec m l xs))  -- se o alimento é saudável, colocamo-lo no topo da lista e juntamos o resultado de calcular os saudáveis na cauda
                        else saudaveis_rec m l xs	 -- caso contrário, calculamos os saudáveis da cauda


---------------------------------------------------------------------------------------
-- calcula os alimentos de uma refeição com um teor de lípidos inferior a um valor dado
saudaveis_nrec::Float->[InfoNutricional]->Refeicao->[String]
saudaveis_nrec m l r=[a | (a,q)<-r,saudavel m a l]


---------------------------------------------------------------------------
-- calcula os alimentos de uma refeição com um teor de lípidos inferior a 5
saudaveis5::[InfoNutricional]->Refeicao->[String]
saudaveis5 l r=saudaveis_rec 5 l r

{-
-- versão 2 (versão pointfree)
saudaveis5=saudaveis_rec 5

-- versão 3 (versão com listas por compreensão)
saudaveis5 l r=[a | (a,q)<-r,saudavel 5 a l]
-}


-- FUNÇÕES AUXILIARES #############################################################################

---------------------------------------------------------------------------
-- verifica se um alimento é saudável (quantidade de lípidos menor que ...)
saudavel::Float->String->[InfoNutricional]->Bool
saudavel x a l=
    let (n1,c1,l1,g1)=getInfoNutricional l a    -- determina a informação nutricional do alimento
    in if (x>l1) then True                      -- verifica se é menor que a quantidade limite
                 else False

{-
-- versão 2
saudavel x a l=
    if (x > (trd4 (getInfoNutricional l a))) then True
    else False

-- versão 3
saudavel x a l
    |x > (trd4 (getInfoNutricional l a))=True
    |otherwise=False
-}


------------------------------------------------------------------------
-- devolve o valor das calorias da informação nutricional de um alimento
snd4::InfoNutricional->Int
snd4 (_,c,_,_)=c


--------------------------------------------------------------------------
-- devolve a quantidade de lípidos da informação nutricional de um alimento
trd4::InfoNutricional->Float
trd4 (_,_,l,_)=l
