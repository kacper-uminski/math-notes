#import "@preview/physica:0.9.2": *
#show math.integral: math.limits.with(inline: false)
#show math.integral.double: math.limits.with(inline: false)
#show math.integral.triple: math.limits.with(inline: false)

#let title = [
  TATA76 - Föreläsningsanteckningar
]


#let titled_block(title, txt) = align(center,block(
    width: 90%,
    fill: luma(230),
    inset: 8pt,
    radius: 4pt,
    align(left,[
	*#title*

	#txt
    ])
))

#let example(num, txt) = [#titled_block([Exempel #num], [#txt])]
#let theorem(title, txt) = [#titled_block([Sats #title], [#txt])]



#set document(title: [#title], author: "Kacper Uminski")

#set text(size: 12pt, font: "IBM Plex Serif")

#align(center, text(17pt)[
  *#title*
])

#align(center, text(14pt)[
  *Kacper Uminski*
])

= Rummet $RR^3$, Grundbegrepp, och Funktioner av Flera Variabler
= Gränsvärden och Kontinuitet

= Differentierbarhet och Partiella Derivator
  Kom ihåg från envariabelanalysen att
  $f'(a) = lim_(h->0) (f(a+h)-f(a))/h$. $f'(a)$ är lutningen hos
  tangenten i punkten $(a,f(a))$.
  == Flera variabler
    Låt $z=f(x,y)$. Fixera $y=b$, det vill säga studera $z=f(x,b)$. När $x$
    varierar beskriver detta samband en kurva. Vi definierar:

    $ f'_x (a,b) = lim_(h->0) (f(a+h,b)-f(a,b))/h $

    $f'_x (a,b)$ blir lutningen i $x$-led i punkten $(a,b,f(a,b))$.
    Detta betecknas $f'_x$, $pdv(f, x)$, $D_x f$. På motsvarande sätt, om vi
    fixerar $x=a$ och låter $y$ variera, definieras och betecknas derivatan
    analogt.

    Krokiga $diff$ betecknar att funktionen beror på _flera_ variabler. Raka
    d betecknar att den enbart beror på _en_ variabel.
    
    Av definitionen framgår att räkneregler för derivata ser ut som förr.
    Deriverar man med avseende på en variabel så är alla andra variabler
    konstanta.

    Observera att om $f$ är en funktion av två variabler, så har grafen till
    $f$ inte _en_ lutning. Lutningen beror på åt vilket håll man tittar.

    #example(1,
      $ f(x,y,z) = x y^2 z^3 => cases(
	  pdv(f,x) = x^2 z^3,
	  pdv(f,y) = 2 x y z,
	  pdv(f,z) = 3 x y^2 z^2,
	  ) $
    )

    #example(2,
      $ f(x,y) = y e^(x y)+sin(x^2+2y) => cases(
	  pdv(f,x) = y^2e^(x y)+2x cos(x^2+2y),
	  pdv(f,y) = (1+x y)e^(x y)+2cos(x^2+2y),
	  ) $
    )

    Om de partiella derivatorna $(f'_x, f'_y, ...)$ är kontinuerliga så sägs
    $f$ vara av klass $C^1$, eller $f in C^1$. Alla elementära funktioner och
    sammansättningar, summor, produkter, och kvoter av sådana är kontinuerliga
    (och därför av klass $C^n, forall n =< oo$.)

    Observera att i envariabelanalysen så gäller det att om $f$ är deriverbar
    så är $f$ kontinuerlig. Detta gäller _inte_ i flervariabelanalysen.

    #example(3,
    $ f(x,y) = cases(
	(x y)/(x^2+y^2)\, (x,y) != (0,0),
	0\, (x,y) = (0,0)
	) $
    )

= Kedjeregeln och Partiella Differentialekvationer
  Kom ihåg från envariabelanalysen att $D(f(g(x))) = f'(g(x)) dot g'(x)$,
  eller med $t = g(x)$ så fås $f(g(x)) = f(t)$ och
  $dv(f,x) = dv(f,t) dot dv(t,x)$.

  #example(1, [
      Betrakta Eulerekvationen $x^2y''-2x y'+2y = 2x^2, x>0$

      Byt variabel, $x = e^t$, det vill säga $t = ln x$: $
	  dv(y,x) = dv(y,t) dot dv(t,x) = 1/x dot dv(y,t)
	  <==> dv(,x) = 1/x dot dv(,t) $

      Så fås: $
	  dv(y,x,2) & = dv(,x)(dv(y,x)) \
	      & = dv(,x)(1/x dot dv(y,t)) \
	      & = \/\/ "Produktregeln" \/\/ \
	      & = -1/(x^2) dot dv(y,t) + 1/x dot dv(,x)(dv(y,t)) \
	      & = -1/(x^2)(dv(y,t,2) - dv(y,t))
      $

      Detta ger den nya ekvationen: $
	  "VL" & = dv(y,x,2)-2x dv(y,x)+2y \
	      & = ... \
	      & = dv(y,x,2)-3dv(y,t)+2y \
	      & = 2x^2 = \/ x = e^t \/ =2e^(2t)
      $

      Alltså blir $dv(y,t,2)-3dv(y,t)+2y = 2e^(2t)$, som har lösningarna
      $y = A e^t+B e^(2t)-2t e^(2t) = A x+B x^2-2x^2ln x$
  ])
  == Kedjeregeln i flera variabler
    Vi förbereder med att titta på Tangentplan.

    Tangentplanets ekvation:
    $ z = f(a,b) + f'_x (a,b) dot (x-a)+f'_y (a,b) dot (y-b) $

    Jämför med tangentens ekvation i envariabelanalys:
    $ y = f(a)+f'(a) dot (x-a) $

    Med $h = Delta x, k = Delta y$, och med
    $Delta z = f(a+Delta x, b+Delta y) - f(a,b)$ fås
    $Delta z approx pdv(f,x) (Delta x)/(Delta t)
	+ pdv(f,x) dot (Delta y)/(Delta t)$.
    Om $Delta t$ är litet, $f in C^1$ och om vi låter $Delta t -> 0$ så
    kan man visa att:
    $ dv(z,t) = pdv(f,x) dot dv(x,t) + pdv(f,y) dot dv(y,t) <- "Kedjeregeln" $

    Alltså, om $z = f(x,y)$, där $x=x(t), y=y(t)$, så är
    $dv(z,t) = pdv(f,x) dot dv(x,t) + pdv(f,y) dot dv(y,t)$, eller med
    $f=f(x,y)$ så fås $dv(f,t) = pdv(f,x) dot dv(x,t) + pdv(f,y) dot dv(y,t)$.

    #example(2,[
	Låt $f(u,v) = v sin(u v)$ där $u=x^2, v=3x$. Då är:
	$f = 3x sin(3x^3)$. Kedjeregeln ger $dv(f,x)=$
    ])

= Tangentplan, Gradient, och Riktningsderivata
  == Gradient
    Om $f=f(x,y)$ har kontinuerliga partiella derivator (eg: f differentierbar)
    så definieras $grad f$ (gradienten av $f$) som
    $ grad f = mat(f'_x; f'_y) $

    #example(1,[
	$ f(x,y,z) = ln(x^2+y)+e^(y z) => grad f = mat(pdv(f,x); pdv(f,y); pdv(f,z)) $
	Observera att $grad f$ är en _vektor_
    ])
    
  == Tangent till kurva
    Låt $Gamma$ vara en kurva i planet (eller rummet), som ges av
    $(x,y) = (x(t),y(t)) = va(phi)(t)$. En tangent till $Gamma$ söks. Undersök
    $t$ och $t+h$:
    $ lim_(h->0) (va(phi)(t+h)-va(phi)(t))/h $

    Om gränsvärdet finns, $!= va(0)$, så kommer vi att få tangentvektorn till
    kurvan, $va(phi)'(t) = (x'(t),y'(t))$.

    #example(2, [
	Bestäm ekvationen för linjen som tangerar kurvan
	$(x,y,z) = (t, t^2, t^3)$ i punkten där $t = -1$.

	$t = -1$ ger $(x,y,z) = (-1,1,-1)$. En tangentvektor fås av
	$ mat(x'(t); y'(t); z'(t)) = mat(1; 2t; 3t^2)
	    = \/ t = -1 \/ = mat(1; -2; -3) $ 
	
	Så tangentens ekvation är
	$ mat(x; y; z) = mat(-1; 1; -1) + t mat(1; -2; 3), t in RR $
    ])

    Betrakta en nivåyta, $f(x,y,z) = C$ och låt $Gamma$ vara en kurva på denna
    yta. $Gamma$ ges av $(x,y,z)=(x(t),y(t),z(t))$. Längs denna kurva är alltså
    $f(x(t), y(t), z(t)) = C$. Derivera med avseende på $t$, så fås
    $dv(f,t)=0$. Dock är
    $ dv(f,t) = pdv(f,x) dot dv(x,t)
	+ pdv(f,y) dot dv(y,t)
	+ pdv(f,z) dot dv(z,t)
	= mat(pdv(f,x); pdv(f,y); pdv(f,z)) dot mat(dv(x,t); dv(y,t); dv(z,t))
	= grad f dot va(phi)'(t) = 0 $
    Men $va(phi)$ ligger i ytan, så $va(phi)'$ är tangentvektor till ytan och
    detta gäller allla kurvor i ytan. Alltså är $grad f$ vinkelrät mot alla
    vektorer som tangerar ytan, då $grad f$ är en normalvektor till nivåytan
    $f=C$.

    #example(3,[
	Sök tangentplanet till ytan $z = x^2+y^2$ i punkten $(-1,1)$.

	$x=-1, y=1 => z=2$.

	Skriv om ytan som en nivåyta.

	$ z = x^2+y^2 <=> F(x,y,z) = x^2+y^2-z = 0 => grad F = mat(2x; 2y; -1) $

	Så $grad F$ är en normal till ytan (i varje punkt på ytan.) Speciellt i
	givna punkten är $(x,y,z)=(-1,1,2)$ så $grad F = (-2,2,-1)$ och
	$(x,y,z)$ ligger i planet omm $(-2,2,-1) dot (x+1, y-1, z-2) = 0$, det
	vill säga $-2x+2y-z = 2$.
    ])

  == Riktningsderivata
    Låt $z = f(x,y)$. Studera $z$ då $(x,y) = (a_1,a_2)+t(v_1,v_2)$ (beskriver en
    linje i planet, genom $(a_1,a_2)$, riktning $(v_1, v_2)$) där
    $|va(v)| = |(v_1, v_2)| = 1$. Då definieras (om gränsvärdet finns)
    $ f'_va(x) (va(a)) = lim_(t->0) (f(va(a)+t va(v))-f(va(a)))/t $

    Observera att om $va(v) = vu(x)$ så är $f'_va(v)(va(a)) = f'_x (va(a))$.
    Samma sak gäller i $y$-led.

    Om man låter $h(t) = f(va(a)+t va(v)) = f(va(g)(t))$, där
    $va(g)(t) = va(a)+t va(v)$, så fås
    $ (va(f)(va(a)+t va(v))-f(va(a)))/t = (h(t)-h(0))/t -> h'(0), t -> 0 $
    men
    $ & h'(0) \
	& = \/ "Kedjeregeln" \/ \
	& = grad f(va(g)(0)) dot va(g)'(0) \
	& = grad f(va(a)) dot va(g)'(0) \
	& = grad f(va(a)) dot va(v), |va(v)| = 1 $

    Således blir $f'_va(v) = grad f(va(a)) dot vu(v)$ om $f$ differentierbar.

    Observera även att $f'_va(v)(va(a)) = grad  f(va(a)) dot vu(v)
	= |grad f(va(a))||vu(v)| cos alpha $ där $cos alpha in [-1,1]$, så när
    $|va(v)|=1$ antar
    $ f'_va(v)(va(a)) = cases(
	"Max: " &|grad f(va(a))| \, & alpha=0,
	"Min: " -&|grad f(va(a))| \, & alpha=-pi
	) $

    Alltså, om $z=f(x,y)$ är en funktionsyta så pekar $grad f$ ut den riktning
    i $x y$-planet där funktionen växer snabbast.
  == Sammanfattning
    Vi har två tolkningar av gradient:
    - Funktionsyta, $z=f(x,y)$. $grad f(va(a))$ pekar ut riktning i
      $x y$-planet där funktionen växer snabbast.
    - Nivåyta, $F(x,y,z)=C$. $grad F(a,b,c)$ är normal till ytan.

= Kurvor, Ytor, och Funktionsdeterminanter
  En kurva på parameterform har utseendet $(x,y,z) = (x(t),y(t),z(t))$. På
  motsvarande sätt beskriver $va(r)(s,t) = (x(s,t), y(s,t), z(s,t))$
  en yta i rummet. Fixt $s = s_0$ ger kurvan
  $va(r)(s_0,t) = (x(s_0,t), y(s_0,t), z(s_0,t))$ som ligger på ytan.
  Denna kurvan har tangentvektorn $pdv(va(r),s)(s_0,t)$.
  Fixt $t = t_0$ ger på samma sätt en kurva på ytan, med tangentvektorn
  $pdv(va(r),s)(s,t_0)$. En normalvektor till ytan, i punkten
  $(s_0,t_0)$, fås av:
  $ va(n) = pdv(va(r),s)(s_0,t_0) times pdv(va(r),t)(s_0,t_0) $

  #example(1, [
      Bestäm tangentplanet till ytan
      $ cases(
	  x & = & 2sin theta cos phi,
	  y & = & 3sin theta sin phi,
	  z & = & 4cos theta,
	  phi & in & [0,2pi],
	  theta & in & [0,pi]
	  ) $
      i punkten $va(r) = (x,y,z) = (0,3,0)$

      Vi bestämmer $phi$ och $theta$ i denna punkt.
  ])


= Dubbelintegraler
  == Integraler i en variabel
    Dela ett intervall $[a,b]$ i $n$ delar. Skapa sedan en undertrappfunktion
    $phi_l (x)$ och en överfunktion $phi_u (x)$ på dessa intervall. Gränsvärdet
    av dessa trappfunktioner kommer vara $sum c phi_n (x)$. Om båda dessa
    konvergerar när intervallet går mot 0 existerar integralen för $f$.
  == Integraler i två variabler
    I två variabler sker integraler öve en rektangel,
    $D={(x,y) | x in [a,b], y in [c, d]}$. Dela upp $D$ i små rektanglar.
    Titta på över- och undertrappor (som i envar), addera alla "volymer"
    $f(x,y)dd(x,y)$ och förfina indelningen. Det nedanstående definieras som det
    gemensamma gränsvärdet om ett sådant existerar:
    $ integral.double_D f(x,y)dd(x,y) $

    Om $f(x,y) >= 0$ på $D$ så kan $integral.double_D f(x,y)dd(x,y)$
    tolkas som volymen mellan $x y$-planet och ytan $z=f(x,y)$, där
    $(x,y) in D$.

    #theorem([], [
	Om $D={(x,y) | x in [a,b], y in [c,d]}$ och $f$ är kontinuerlig på $D$
	så är
	$ integral.double_D f(x,y) dd(x,y) = integral_a^b (integral_c^d f(x,y)
	    dd(y))dd(x) $
    ])

    Observera att integralen inom parentesen är "arean" på en skiva för ett
    fixt $x$. $dd(x)$ är i detta sammanhang "tjockleken" av skivan.

    #example(1, [
	$ & integral.double_D x/(1+x y)^2dd(x,y) \
	    & D={(x,y) | x in [1,4], y in [0,1]} $

	Integrera först med avseende på $y$ ty detta verkar enklast.
	$ & integral_1^4 (integral_0^1 x/(1+x y)^2 dd(y))dd(x) \
	    & = \/ 1+x y = t, x dd(y) = dd(t) \/ \
	    & = integral_1^4 [-1/(1+x y)]_(y=0)^(y=1)dd(x) \
	    & = integral_1^4 (-1/(1+x)+1)dd(x) \
	    & = [x-ln|1+x|]_1^4 = \
	    & = 4-ln 5-(1-ln 2) \
	    & = 3-ln 5/2 $
    ])

    Mer allmänna områden, $D = {(x,y) | x in [a,b], y in [phi(x), psi(x)]}$, ger
    $ integral.double_D f(x,y)dd(x,y) =
	integral_a^b (integral_phi(x)^psi(x) f(x,y) dd(y))dd(x) $
    Observera att gränserna fortfarande är konstanta på den yttre integralen.
    
    #example(2, [
	$ integral.double_D (x^2+y^2)dd(x,y) $ där $D$ är en triangel med hörn i
	$(0,0), (1,0), (1,2)$. Området kan beskrivas på två sätt:
	#enum([
	    Integrera med avseende på $y$ först. (Ska ha konstanta gränser på
	    $x$.) Detta ger $x in [0,1]$. För varje fixt $x$ fås $y in [0,2x]$.
	    Därav blir:
	    $ integral.double_D (x^2+y^2)dd(x,y)
		& = integral_0^1 (integral_0^(2x)(x^2+y^2)dd(y))dd(x) \
		& = integral_0^1 [x^2y+(y^3)/3]_(y=0)^(y=2x) dd(x) \
		& = integral_0^1 (2x^3+8/3x^3)dd(x) \
		& = integral_0^1 14/3x^3dd(x) \
		& = [14/12x^4]_0^1 \
		& = 7/6 $
	],[
	    Integrera med avseende på $x$ först. Konstanta gränser med avseende
	    på $y$. Detta ger $D = {(x,y) | y in [0,2], x in [y/2,1]}$. Vi får
	    då:
	    $ integral.double_D (x^2+y^2)dd(x,y)
		& = integral_0^2 (integral_(y/2)^1 (x^2+y^2)dd(x))dd(y) \
		& = ... \
		& = 7/6 $
	])
    ])

    #example(3, [
	$ integral.double_D (x+2y)dd(x,y) $ Där $D$ är enhetscirkeln. Området
	kan beskrivas som
	$D = {(x,y) | x in [-1,1], y in [-sqrt(1-x^2), sqrt(1-x^2)]}$. Vi får
	då:
	$ integral.double_D (x+2y)dd(x,y)
	    & = integral_(-1)^1 (integral_(-sqrt(1-x^2))^sqrt(1-x^2)(x+2y)
		dd(y))dd(x) \
	    & = integral_(-1)^1 [x y+y^2]_(-sqrt(1-x^2))^sqrt(1-x^2) dd(x) \
	    & = integral_(-1)^1 2x sqrt(1-x^2)dd(x) \
	    & = [-2/3(1-x^2)^(3/2)]_(-1)^1 \
	    & = 0 $
    ])

    #example(4, [
	Beräkna $ integral_0^2 integral_y^2e^(x^2)dd(x,y) $ Rita först området.
	Det blir en triangel med kanterna i $(0,0), (0,2), (2,2)$. Alltså blir
	$D = {(x,y) | x in [0,2], y in [0,x]}$. Därav blir:
	$ integral_0^2 (integral_y^2e^(x^2)dd(x))dd(y)
	    & = integral.double_D e^(x^2)dd(x) \
	    & = integral_0^2 (integral_0^x e^(x^2)dd(y))dd(x) \
	    & = integral_0^2 [y e^(x^2)]_0^x dd(x) \
	    & = integral_0^2 x e^(x^2)dd(x) \
	    & = [1/2e^(x^2)]_0^2 \
	    & = (e^4 -1)/2 $
    ])

    Ifall $f(x,y) = g(x) dot h(y)$ och om området är rektangeln
    $D = {(x,y) | x in [a,b], y in [c,d]}$ så erhålls:
    $ integral.double_D f(x,y) dd(x,y)
	& = integral.double_D g(x) dot h(y) dd(x,y) \
	& = integral_c^d (integral_a^b g(x) dot h(y) dd(x))dd(y) \
	& = integral_c^d (h(y) integral_a^b g(x) dd(x))dd(y) \
	& = integral_a^b g(x) dd(x) dot integral_c^d h(y)dd(y) $
    
= Variabelbyte i dubbelintegraler
  Betrakta linjära variabelbytet:
  $ cases(u = a x+b y, v = c x+d y) <=> vec(u,v) = mat(a,b; c,d)vec(x,y) $
  med determinanten 
  $ underbrace(mu(D), "Arean av " D) = 1, \
      underbrace(mu(D'), "Arean av " D') =
      underbrace(mat(delim: "||", a,b; c,d), "Beloppet av\n determinanten") $

  Vid linjära byten gäller det att
  $ mat(delim: "|", pdv(u,x), pdv(u,y); pdv(v,x), pdv(v,y))
      = mat(delim: "|", a,b; c,d) $

  Betrakta nu ett allmänt byte. Om $u=u(x,y),v=v(x,y) in C^1$, så anger
  $abs(dv((u,v),(x,y)))$ den lokala ytskalan vid avbildning
  från $(x,y)$ till $(u,v)$.

  #theorem([],[
      Om $x=x(u,v), y=y(u,v)$ är en omvändbar $C^1$-avbildning av $D'$ (i
      $u v$-planet) på $D$ (i $x y$-planet), sådan att
      $abs(dv((x,y),(u,v))) != 0$ i $D'$ så är
      $ integral.double_D f(x,y)dd(x,y)
	  = integral.double_(D') f(x(u,v),y(u,v))
	  underbrace(abs(dv((x,y),(u,v))),
	  "Tar hand om\n area-ändringen")
	  dd(u,v) $
  ])

  Jämför i med en variabel:
  $ integral_x(a)^x(b) f(x)dd(x) = \/ x=x(u), dd(x)=dv(x,u)dd(u)\/
      = integral_a^b f(x(u)) dot dv(x,u)dd(u) $
  
  #example(1, [
      Beräkna
      $ integral.double_D x/(x+2y)dd(x,y) \
	  D = {(x,y) | 2x-y in [0,2], x+2y in [1,2]} $

      Om vi vill integrera utan variabelbyte så behöver vi dela upp området.
      Annars blir formen för svår. Byt istället variablerna för att få ett
      bättre område. Sätt $u=2x-y, v=x+2y$, det vill säga
      $ vec(u,v) = mat(2,-1; 1,2)vec(x,y) $

      Så fås nya området $D' = {(u,v) | u in [0,2], v in [1,2]}$ Observera att
      determinanten för matrisen är nollskillt ($=5$), så bytet är inverterbart.
      Då fås
      $ integral.double_D x/(x+2y)dd(x,y)
	  = integral_(D') x(u,v)/(x(u,v)+2y(u,v))
	  mat(delim: "||", dv((x,y),(u,v)))dd(u,v) $

      Vi får
      $ vec(x,y) = 1/5 mat(2,1; -1,2)vec(u,v) $

      Så $x=(2u+v)/5, y=(-u+2v)/5$. Alltså blir det ovanstående
      $ integral.double_(D') (2u+v)/5/v dot 1/5 dd(u,v)
	  = 1/25 integral_1^2 integral_0^2 (2u/v+1)dd(u,v)
	  = ... = 1/25 (4ln 2 + 2) $
  ])

  #example(2, [
      Beräkna
      $ integral.double_D e^(-(x^2+y^2))dd(x,y) \
	  D = {(x,y) | x^2+y^2 in [1,3], y-x >= 0} $
      Arean blir en halv, ihålig cirkel. Använd därför polära koordinater,
      $x = r cos phi, y = r sin phi, mat(delim: "||", dv((x,y),(r,phi)))=r$.

      Vi har nu för det nya området
      $ D' = {(r,phi) | r^2 in [1,3], r(sin phi-cos phi) >= 0} \
	  => cases(
	  r in [1, sqrt(3)],
	  sin phi >= cos phi <=> phi in [pi/4, 5pi/4]
	  ) $

      Vi får därmed
      $ integral.double_D e^(-(x^2+y^2))dd(x,y) 
	  = & integral.double_(D') r e^(-r^2) dd(r,phi) \
	  = & integral_(pi/4)^(5pi/4) dd(phi) dot
	  integral_1^(sqrt(3)) r e^(-r^2) dd(r) \
	  = & pi [-1/2e^(-r^2)]_1^sqrt(3)
	  = & pi/2(e^(-1)-e^(-3)) $
  ])
  
= Trippelintegraler
  $ integral.triple_D f(x,y,z)dd(x,y,z) $
  Definieras som i dubbelintegral (småbitar, summera, förfina).

  Kan tolkas som:
  - $f(x,y,z) = 1$: $integral.triple_D 1 dd(x,y,z) =$ Volymen av $D$.
  - Om $f(x,y,z)=$ Densiteten i punkten $(x,y,z)$:
    $integral.triple_D f(x,y,z) =$ massan av $D$.

  #theorem([],[
      Om $D = {(x,y) in D_0 | g(x,y) <= z <= h(x,y)}$ och $f$ är kontinuerlig,
      så är:
      $ integral.triple_D f(x,y,z)dd(x,y,z)
	  = integral.double_(D_0) (integral_(z=g(x,y))^(z=h(x,y))f(x,y,z)
	      dd(z))dd(x,y) $

      Speciellt om
      $D = {(x,y,z) | x in [a,b], y in [phi(x), psi(x)], z in [g(x,y), h(x,y)] }$
      så är:
      $ integral.triple_D f(x,y,z)dd(x,y,z)
	  = integral_a^b (integral_phi(x)^psi(x)(integral_g(x,y)^h(x,y)
	      f(x,y,z)dd(z))dd(y))dd(x) $
  ])

  #theorem([],[
      Om $D = {(x,y,z) | x in [a,b], (x,y) in D_z}$ så är:
      $ integral.triple_D f(x,y,z)dd(x,y,z)
	  = integral_a^b (integral.double_(D_z)f(x,y,z)dd(x,y))dd(z) $
      Detta liknar skivformeln i envariabelanalysen.
  ])

  #example(1, [
      $ I = integral.triple_D x z e^(x y)dd(x,y,z) \
	  D = {(x,y,z) | x,y in [0,1], z in [0,2]} $
      Vi får:
      $ I & = integral_0^1 (integral_0^1 (integral_0^2 x z e^(x y)
	  dd(z))dd(y))dd(x) \
	  & = integral_0^1 (integral_0^1 [(z^2)/2x e^(x y)]dd(y))dd(x) \
	  & = ... \
	  & = 2e-4 $
  ])

  #example(2, [
      Volymen av en tetraheder med hörn i $(0,0,0), (1,0,0), (0,1,0), (0,0,2)$.
      Börja med att sätta $D_0$ till projektionen av tetrahedern i $x y$-planet,
      som ges av triangeln $D_0 = {x in [0,1], y in [0, 1-x]}$. I $z$-ledd blir
      "golvet" $x y$ planet, det vill säga $x=0$, och "taket" blir planet genom
      $(1,1,0), (0,1,0), (0,0,2)$. Alltså, enligt linjära algebran,
      $2x+2y+z = 2$ eller $z = 2-x-2y$. Då blir
      $D = {(x,y,z) | x in [0,1], y in [0,1-x], z in [0,2-2x-2y]}$ och

      $ V = integral_0^1 (integral_0^(1-x)(integral_0^(2-2x-2y)1
	  dd(z))dd(y))dd(x) = ... = 1/3 $
  ])

  #example(3, [
      Volymen av kroppen som ligger inom klotet $x^2+y^2+z^2 >= 1$ och ovanför
      konen $z = sqrt(x^2+y^2)$. Alternativ 1 är att projicera området där
      konen och klotet skär varandra på $x y$-planet och sätta $D_0$ till
      skuggan. Då området ges av $D = {(x,y,z) | x^2+y^2+z^2=1, z=sqrt(x^2+y^2)}$
      så blir $D_0 = {(x,y,z) | z = 1/sqrt(2), x^2+y^2=1/2}$. För alla
      $(x,y,z)$ i $D_0$ variera nu $z$ från golv, $z=sqrt(x^2+y^2)$, till tak
      $x^2+y^2+z^2=1, z>=0$, så

      $ V & = integral.double_(D_0) (integral_sqrt(x^2+y^2)^sqrt(1-x^2-y^2) 
	  dd(z))dd(x,y) \
	  & = \/ "Polärt byte," x=r cos phi, y=r sin phi, \
	  & r in [0,1/2], phi in [0,2pi], dv((x,y),(r,phi))=r \/ \
	  & = integral.double_(D_0) (sqrt(1-r^2)-r)r dd(r,phi) \
	  & = integral_0^(1/sqrt(2))(r sqrt(1-r^2)-r^2) dd(r)
	  dot integral_0^(2pi) dd(phi) \
	  & = 2pi dot [-1/3 (1-r^2)^(3/2)-(r^3)/3]_0^(1/sqrt(2)) \
	  & = (2pi)/3 (1-1/sqrt(2)) $
  ])

  == Variabelbyte
    Variabelbyte i $integral.triple$ fungerar som i $integral.double$. Låt
    $ cases(x = x(u,v,w), y = y(u,v,w), z = z(u,v,w)) $ vara en omvändbar
    avbildning av $D'$ i $u v w$-rummet på $D$ i $x y z$-rummet. Då
    definieras:
    $ 0 != dv((x,y,z), (u,v,w)) = mat(delim: "|",
	pdv(x,u), pdv(x,v), pdv(x,w);
	pdv(y,u), pdv(y,v), pdv(y,w);
	pdv(z,u), pdv(z,v), pdv(z,w)
	) $

    Om $dv((x,y,z),(u,v,w)) != 0$ så är
    $ integral.triple_D f(x,y,z)dd(x,y,z)
	= integral.triple_(D')f(x(u,v,w), y(u,v,w),z(u,v,w))
	abs(dv((x,y,z),(u,v,w))) dd(u,v,w) $

    #example(4, [
	Bestäm volymen av området
	$ D = {(x,y,z) | x in [0,1], x+y+z in [0,2], x+y-z in [0,3]} $

	Sätt $D' = {(u,v,w) | u=x, v=x+y+z, w=x+y-z}$ så fås determinanten av
	$ dv((u,v,w), (x,y,z)) =
	    mat(delim: "|",
	    1,0,0;
	    1,1,1;
	    1,1,-1)
	    = mat(delim: "|",
	    1,1;
	    1,-1
	    ) = -2 $
	Så $abs(dv((u,v,w), (x,y,z))) = 2 != 0$ och
	$abs(dv((x,y,z), (u,v,w))) = 1/2$. Volymen blir då
	$ V = integral.triple_D 1dd(x,y,z)
	    = integral.triple_(D') 1 dot 1/2 dd(u,v,w)
	    = 1/2 dot 1 dot 2 dot 3 = 3 $ 
    ])

  == Rymdpolära koordinater
    Sätt:
    $ cases(
	x = r sin theta cos phi,
	y = r sin theta sin phi,
	z = r cos theta
	) $
    Determinanten för bytet blir då:
    $ abs(dv((x,y,z), (r, phi, theta))) = r^2 sin theta $
    
    
= Integraltillämpningar
  #example(1, [
      Beräkna $integral.triple_D (x-y)dd(x)dd(y)dd(z)$ där $D$ är tetrahedern
      med hörn i $(0,0,0), (1,1,1), (1,1,0), (1,0,1)$.

      Byt först till bättre variabler. Gör ett basbyte, utgående från tre av
      kantvektorerna. Linjära algebran ger att ett basbyte från en gammal bas,
      $underline(e)$, till en ny bas, $underline(f)$ ges av
      $underline(f) = underline(e)T$ så att $T$:s kolonner består av
      koordinaterna för $overline(f)_1, overline(f)_2, overline(f)_3$
      Beteckningarna kommer vara:

      $ cases(X = "Gamla koordinater",
	  Y = "Nya koordinater",
	  X = T Y) $

      Inför:
      $ underline(f) = underline(e) T = underline(e) mat(1,1,1; 1,1,0; 1,0,1) $

      Så:
      $ X = vec(x,y,z) = T vec(u,v,w) = mat(1,1,1; 1,1,0; 1,0,1) vec(u,v,w) $

      Alltså blir:
      $ cases(x = u+v+w, y = u+v, z = u+w) $

      Så fås att
      $ vec(u,v,w) = vec(1,0,0) <=> vec(x, y, z) = vec(1,1,1) $
      och på samma sätt
      $ vec(u,v,w) = (0,1,0) <=> vec(x,y,z) = vec(1,1,0) $
      samt
      $ vec(u,v,w) = (0,0,1) <=> vec(x,y,z) = (1,0,1) $
      Origo flyttas ej.

      Så nya området (i $u v w$-rummet) blir en tetraheder med hörn i
      enhetsvektorerna. Det ger:

      $ |dv((x,y,z),(u,v,w))| = || mat(1,1,1; 1,1,0; 1,0,1) || = |-1| = 1 $

      Då fås:

      $ integral.triple_D (x-4)dd(x)dd(y)dd(z) = & \
	  = & \/ x = u+v+w, y = u+v \/ \
	  = & integral.triple_(D') w dv((x,y,z), (u,v,w))dd(u)dd(v)dd(w)\
	  = & integral.triple_(D') w dd(u)dd(v)dd(w) \
	  => I = & integral.triple_(D') w dd(u)dd(v)dd(w) \
	  = & integral_0^1 (
	      integral_0^(1-u) (
		  integral_0^(1-u-w)
		  w
		  dd(w))dd(v)) dd(u)\
	  = & ... \
	  = & 1/24 $
  ])

  #example(2, [
      Berkäkna $integral.triple_D x^2 dd(x)dd(y)dd(z)$, där $D$ ges av
      $0 <= y <= z^2 <= x^4 <= 1$.

      Försök först beskriva området på ett användbart sätt. Tittar vi bara på
      $x$, så ser vi att $0 <= x^4 <= 1$, det vill säga $-1 <= x <= 1$. För dessa
      $x$ gäller dessutom $0 <= y <= z^2 <= x^4$ (där $x$ är fixerat.) Så vi har:
      $ cases(y >= 0, y <= z^2, z^2 <= x^4 <=> -x^2 <= z <= x^2) $

      Alltså är:
      $ integral.triple_D x^2dd(x,y,z) 
	  = & integral_(-1)^1(integral.double_(D_x) x^2
	      dd(y,z))dd(x) \
	  = & integral_(-1)^1(
	      integral_(-x^2)^(x^2)(
		  integral_0^(z^2)
		  x^2
	      dd(y))dd(z))dd(x) \
	  = & ... \
	  = & 4/27 $ 
  ])

  #example(4, [
      $D$ är den del av enhetscirkeln som ligger i 1:a kvadranten med ytdensitet
      $rho(x,y) = sqrt(x^2+y^2)$ Vad blir massan?

      Ett litet element med arean $dd(x,y)$ och ytdensitet $rho(x,y)$ har
      massan $rho(x,y)dd(x,y)$. Plattans totala massa är
      $ m = & integral.double_D rho(x,y)dd(x,y) \
	  = & integral.double_D sqrt(x^2+y^2)dd(x,y) \
	  = & \/ "Polära koord:" x = r cos(phi), y = r sin(phi),
	  |dv((x,y),(r,phi))| = r, \ & D' = {phi in [0, pi/2], r in [0,1]} \/ \
	  = & integral.double_(D') r dot r dd(r,phi) \
	  = & integral_0^1 r^2 dd(r)
	  dot integral_0^(pi/2) dd(phi) \
	  = & pi/6 $
  ])

  #example(5, [
      Bestäm tyngdpunkten hos ett homogent halvklot:
      $ cases(x^2+y^2+z^2 = R^2, z >= 0) $

      Masscentrum (tyngdpunkten) ligger på $z$-axeln (av rättviseskäl)
      och $z$-koordinaten $0 <= z_(t p) <= R$. Vi har alltså tyngdpunkt
      $(x,y,z) = (0,0,z_(t p))$ där

      $ z_(t p) = (integral.triple_D z dd(m))
	  /(integral.triple_D dd(m)) $
      där $dd(m) = rho(x,y,z)dd(x,y,z)$. Kroppen är homogen, så
      $rho(x,y,z) = rho = "Konstant"$. Vi har

      $ z_(t p)
	  = (integral.triple_D z rho dd(x,y,z))/(integral.triple_D rho
	      dd(x,y,z))
	  = (integral.triple_D z dd(x,y,z))/(integral.triple_D dd(x,y,z)) $

      $integral.triple_D dd(x,y,z)$ är volymen av $D = 1/2 dot (4 pi R^3)/3
	  = (2 pi R^3)/3$. Inför rymdpolära koordinater för den andra:
      $ cases(x = r sin theta cos phi, y = r sin theta sin phi, z = r cos theta,
	  |dv((x,y,z), (r, theta, phi))| = r^2 sin theta) $

      Vi får gränserna:
      $ D' = cases(r in [0,R], theta in [0, pi/2], phi in [0, 2pi]) $

      Detta ger:
      $ & integral.triple_(D') underbrace(r cos theta, z)
	  underbrace(r sin theta, | dv((x,y,z),(r,theta,phi))|)
	  dd(r, theta, phi) \
	  = & integral_0^R r^3 dd(r)
	  dot integral_0^(pi/2) cos theta sin theta dd(theta)
	  dot integral_0^(2pi) dd(phi) \
	  = & [(r^4)/4]_0^R dot [(sin^2 theta)/2]_0^(pi/2) dot 2pi \
	  = & (pi R^4)/4
      $

      Alltså är:
      $ z_(t p) = ((pi R^4)/4)/((2pi R^3)/3) = 3/8 R $

      Så $(x_(t p), y_(t p), z_(t p)) = (0,0,3/8 R)$
  ])
