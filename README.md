# PFL_TP1_G05_09
Primeiro Projeto de Programação Funcional e Lógica (PFL) 


## Como correr o código

Tendo já instalada uma versão do interpretador de ghc, para correr o código e ter acesso a todas as suas funcionalidades, basta utilizar os seguintes comandos:

> ghci
> :l main.hs 
> main

Caso estes comandos não estejam a funcionar, será necessário correr o seguinte comando:

> :l main.hs polynomial.hs

Para correr os testes criados com recurso ao quickCheck:

Ubuntu:
> cabal update 
> cabal install quickcheck

Windows:
>cabal install --lib --package-env . QuickCheck


## Representação dos Polinomios

### Estrutura e justificação

Com o objetivo de escolher uma representação adequada para um polinómio, pensamos em duas alternativas diferentes: criar um data type específico ou utilizar apenas types já existentes. De modo a simplificar a solução ao problema proposto, foi escolhida a segunda opção.

Assim sendo, foram feitas as seguintes definições:
type Polynomial = [Monomial]
type Monomial = (Int, [Literal])
type Literal = (Char, Int)


O polinómio é representado por uma lista de monómios e um monómio é por sua vez um tuplo constituído por um Int, o coeficiente e uma lista de elementos do type Literal que corresponde à parte literal do monómio. O type Literal é definido por um tuplo (Char, Int) onde o caracter simboliza a variável e o int o seu expoente.


## Funcionalidades

### Parse de um polinómio

Para possibiltar o input como uma string, foi desenvolvida a função parsePolynomial. Esta recebe como parametro uma String e retorna um elemento do type Polynomial. Recorrendo à recursão, foi criada a função auxiliar parsePolynomialAux, que por sua vez chama parseMonomial onde irá ser criado cada monómio com as diferentes partes da String inicial. 


### Normalizar polinómios

Para normalizar um polinómio foi desenvolvida a função normalizePolynomial. Esta recebe e retorna um parâmetro do type Polynomial. 
No âmbito de dividir os processos, foi feita a composição de três funções diferentes:
reducePolynomial- soma monómios do polinómio com a mesma parte literal e remove os monómios com coeficiente igual a zero. 
reduceLiteralsPolynomial - remove do polinómio monómios com coeficientes igual a zero,
remove da parte literal de cada monómio do polinómio variáveis elevadas a zero e junta literais do mesmo monómio com a mesma variável.
sortPolynomial - ordena os monómios do polinómio por ordem crescente consoante o grau e coeficiente.

### Adicionar polinómios

Para somar dois polinómios foi implementada a função sum2Polynomials. Esta recebe dois parametros do type Polynomial e retorna apenas um. A função recorre à normalização da concatenação de duas listas que representam os polinómios.


### Multiplicar polinómios

Para multiplicar dois polinómios foi implementada a função prod2Polynomials. Esta recebe dois parametros do type Polynomial e retorna apenas um. A função recorre à normalização de uma lista, sendo esta formada pela multiplicação entre todos os elementos do primeiro polinómio e do segundo. Para a criação de cada monómio é chamada a função prod2Monomials.

### Derivada de um polinómio

Para derivar dois polinómios foi implementada a função derivPolynomial. Esta recebe como parametros um elemento do type Polynomial e um caracter e retorna um elemento do type Polynomial. A função recorre à normalização de uma lista, sendo esta formada pela derivação entre todos os elementos do polinómio em função do caracter. Para a criação de cada monómio é chamada a função derivMonomial.

### Output de um polinómio

Para dar output de um polinómio, foi desenvolvida a função outPolynomial.Esta recebe como parametro um elemento do type Polynomial e retorna uma String.
A função coloca em formato String todos os monómios do polinómio através de chamadas à função outMonomial e por fim, junta-os.

## Exemplos

Correndo os comandos : 

* **test1** : irá ser disponibilizado um exemplo de cada uma das funcionalidades (normalização, soma, multiplicação, derivação), onde é mostrado os diferentes operadores e o respetivo resultado com recurso às funções de parse e de outputting do programa;
* **test2** : irá ser disponibilizado um exemplo de cada uma das funcionalidades (normalização, soma, multiplicação, derivação), onde é  mostrado os diferentes operadores e o respetivo resultado com a representação interna do programa;


### **Parse**
>parsePolynomial "5000 + 100 + 2" 
>**Result :** [(5000,[]),(100,[]),(2,[])]

>parsePolynomial "x^5y + y^2 + 0x^6 + o^3"
>**Result :**  [(1,[('x',5),('y',1)]),(1,[('y',2)]),(0,[('x',6)]),(1,[('o',3)])]

>parsePolynomial "x^20z^30p^12" 
>**Result :** [(1,[('x',20),('z',30),('p',12)])]

### **Normalize**

> normalizePolynomial [(9,[('x',2), ('x',3)]),(2,[('x',1)]),(5,[('x',1)])] 
> **Result :** [(9,[('x',5)]),(7,[('x',1)])]

> normalizePolynomial [(0,[('x',2)]),(2,[('x',0)]),(8,[('u',1),('i',0)])] 
> **Result :** [(8,[('u',1)]),(2,[])]

> normalizePolynomial [(-2,[('x',2)]),(2,[('x',2)]),(8,[('u',1),('i',0)]),(8,[('u',1)]) ]
> **Result :** [(16, [('u',1)])]


### **Sum**
> sum2Polynomials [(-2,[('x',3),('x',1)])] [(2,[('x',2),('x',2)])]
>**Result :** []

> sum2Polynomials [(9,[('x',2)]),(2,[('x',1)]),(5,[('x',1)])] [(2,[('x',1),('p',1)])]
>**Result :**  [(2,[('p',1),('x',1)]),(9,[('x',2)]),(7,[('x',1)])]

> sum2Polynomials [(8,[]),(2,[('x',1),('e',0)]),(5,[('x',1),('r',0)])] [(2,[('x',1),('p',0)]),(-4,[])] 
> **Result :** [(9,[('x',1)]),(4,[])]


### **Multiplication**
> prod2Polynomials [(2,[('x',1),('p',0)]),(-4,[])] [(2,[]),(-4,[])] 
> **Result :** [(-4,[('x',1)]),(8,[])]

> prod2Polynomials [(5,[('t',1), ('p',9), ('t',1) ])] [(-1,[('f',0)])] 
>**Result :** [(-5,[('t',2),('p',9)])]

> prod2Polynomials [(2,[('x',1),('p',0),('k',6)]),(-4,[])] [(2,[('x',0),('l',2)])]
> **Result :**  [(4,[('k',6),('l',2),('x',1)]),(-8,[('l',2)])]

### **Derivation**
> derivPolynomial 'x' [(2,[('x',0),('p',1)]),(-4,[('x',0)]),(2,[]),(-4,[('i',19)])] 
>**Result :** []

> derivPolynomial 'o' [(1,[('o',0),('c',1)]),(-4,[('o',1)]),(2,[('o',3),('r',3)]),(-4,[('o',6)])] 
>**Result :** [(-24,[('o',5)]),(6,[('o',2),('r',3)]),(-4,[])]

> derivPolynomial 'y' [(2,[('y',3)]),(-4,[('y',3)]),(9,[('y',3)]),(7,[('y',3)])]
> **Result :** [(42,[('y',2)])]

### **Output**
>outPolynomial [(5000,[]),(100,[]),(2,[])] 
>**Result :** "5000 + 100 + 2"

>outPolynomial [(1,[('x',5),('y',1)]),(1,[('y',2)]),(0,[('x',6)]),(1,[('o',3)])] 
>**Result :** "x^5y + y^2 + 0x^6 + o^3"

>outPolynomial [(1,[('x',20),('z',30),('p',12)])] 
>**Result :** "x^20 z^30 p^12" 

### **Tests**

De modo a a testar o programa, como foi sugestionado, utilizou-se a biblioteca quickCheck. Para as funcionalidades: normalização, soma, multiplicação e derivação foram desenvolvidas funções que permitiam garantir o bom funcionamento das mesmas. Estes testes foram colocados num ficheiro à parte chamado *test.hs*. É ainda de referir que para cada funcionalidade foram criados dois tipos de testes diferentes, testes de propriedades e testes unitários. 
