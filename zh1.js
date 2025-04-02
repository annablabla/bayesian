// 1. Diszkalkuliás modell
/*1.1 Írj következtetésre olyan ComplexModel1-et, ami ilyen premisszákat generál véletlenszerűen: Panni "könyvtáros/tanár" "és/vagy" "csendes/cserfes" és ebből logikailag helyesen következtetne olyan konklúzióra, hogy "Panni csendes/cserfes", DE! sajnos diszlexiás az ágens és a "csendes/cserfes"-t valamilyen valószínűséggel felcseréli.*/

var feature1 = ['könyvtáros','tanár']; 
var feature2 = ['csendes','cserfes'];
var operator = [' és ',' vagy '];
var dyslexProb = 0.1; // 10% esély az és-vagy felcserélésre

var ComplexModel1 = function() {
    // choose word uniformly from lists
    var word1 = uniformDraw(feature1) 
    var op = uniformDraw(operator)
    var word2  = uniformDraw(feature2)
    print('Premissza: Panni ' + word1 + op + word2 + '.'); 
    var word3 = uniformDraw(feature2)
    
    // felcserélt szó:
    var dyslexWord3 = flip(dyslexProb) // ha felcseréli
                    ? (word3 == 'csendes' ? 'cserfes' : 'csendes') // akkor a csendesből cserfes lesz, a cserfesből csendes
                    : word3; // nem cseréli fel
                    
    print('Konklúzió: Panni ' + dyslexWord3 + '.'); 
    
    var ervenyes = (op == ' és ') // ha 'és' a kötőszó
                   ? ((word2 == word3) ? 'érvényes' : 'nem érvényes') // és a két szó egyezik, akkor érvényes a következtetés
                   : 'nem érvényes'
                   
    print(ervenyes); 
    return ervenyes
}
 
var output = 
  Infer({model: ComplexModel1, method:'rejection', samples: 5})

/*1.2 Írj következtetésre olyan ComplexModel2-t, ami ilyen premisszákat tartalmaz: Panni "könyvtáros/tanár" "és/vagy" "csendes/cserfes" és ebből logikailag helyesen következtetne olyan konklúzióra, hogy "Panni csendes/cserfes" ill. "Panni könyvtáros/tanár", DE! az "és"-t 95%-ban a klasszikus logikának megfelelően használja, de elég gyakran, 80%-os valószínűséggel néha a vagy-ot és-nek olvassa. Vö.: "Jaj! valami ördög... vagy ha nem, hát... kis nyúl!".*/
var feature1 = ['könyvtáros','tanár']
var feature2 = ['csendes','cserfes']
var operator = [' és ',' vagy ']
var andProb = 0.95 // 95% hogy és = és
var orProb = 0.8 // 80% hogy vagy = és

var ComplexModel2 = function() {
    var word1 = uniformDraw(feature1)
    var op = uniformDraw(operator)
    var word2  = uniformDraw(feature2)
    print('Premissza: Panni ' + word1 + op + word2 + '.')
    var word3 = uniformDraw(feature2)
    
    // felcserélt operátor
    var dyslexOp = (op == 'és') // ha 'és' a kötőszó
                    ? (flip(andProb) ? ' és ' : ' vagy ') // akkor 95%, hogy 'és' is marad
                    : (flip(orProb) ? ' és ' : ' vagy ' ) // ha 'vagy' a kötőszó, akkor 80%, hogy 'és' lesz belőle
    //print(op)
    //print(dyslexOp)
    print('Konklúzió: Panni ' + word3 + '.')
    
    var ervenyes = (dyslexOp == ' és ') // érvényes, ha 'és' a kötőszó
                   ? ((word2 == word3) ? 'érvényes' : 'nem érvényes') // és egyezik a premissza- és konklúzióbeli jelző, másképp nem érvényes 
                   : 'nem érvényes'
    print(ervenyes); 
    return ervenyes
}
 
var output = 
  Infer({model: ComplexModel2, method:'rejection', samples: 5})


/*1.3 Programozz be egy olyan modellt, ami kiszámolja, hogy mi annak a valószínűsége, hogy ha két kockával dobunk, akkor az összeg legalább 4 lesz!*/
var dobás = function () {
  var kocka1 = categorical({ps: [1/6, 1/6, 1/6, 1/6, 1/6, 1/6], 
                            vs: [1, 2, 3, 4, 5, 6]});
  var kocka2 = categorical({ps: [1/6, 1/6, 1/6, 1/6, 1/6, 1/6], 
                            vs: [1, 2, 3, 4, 5, 6]});
  var kockaSum = kocka1 + kocka2;
  condition(kockaSum > 3); // két kockadobás összege legalább 4
  return kockaSum;

}

var model = Infer({method: 'enumerate'}, dobás);
viz.auto(model);

/*1.4 (King-Ace Paradox, bemelegítő) Tudjuk, hogy a klasszikus logikában a "Ha ász van a kezemben, akkor király van, vagy ha nincs ász a kezemben, akkor király van a kezemben" mondatból nem következik feltétlenül, hogy király van a kezemben (miért?), sőt, ha a mondatban szereplő "vagy"-ot kizáró vagy értelemében használjuk, akkor kifejezetten az következik belőle, hogy nincs király a kezemben. Írj programot, ami a helyzetet modellezi úgy, hogy a mondatbeli "vagy" jelentése néha "és" néha "vagy" néha "kizáró vagy". Vö.: The Cambridge Handbook of Computational Psychology, ed.: Ron Sun, 2008, Cambrigde, p. 137.*/
var kingAceParadox = function() {
  var op = uniformDraw(['és', 'vagy', 'kizáró-vagy']); // lehetséges kötőszó-jelentések

  // vagy van a kezemben ász/király, vagy nincs (50% esély)
  var ászVan = flip(0.5);
  var királyVan = flip(0.5);
  
  // ha ász van a kezemben, akkor király van a kezemben
  var impl1 = !ászVan || királyVan;  // A-->K = ~AvK
  // ha nincs ász a kezemben, akkor király van a kezemben
  var impl2 = ászVan || királyVan;   // ~A-->K = AvK
  
  var mondat = 
    (op == 'és') ? (impl1 && impl2) :        // AND
    (op == 'vagy') ? (impl1 || impl2) :      // OR
    (impl1 && !impl2 || !impl1 && impl2);  // XOR
  condition(mondat);
  
  return {király_van_a_kezemben: királyVan, operator: op};
};

var model = Enumerate(kingAceParadox);
viz(model);

// 2. Állítás, mint érték
/* Programozd le Coq-ban a következő állítások bizonyítását:*/
//2.1. Lemma problem_1 : forall A B C : Prop, A /\ (B \/ C) -> (A /\ B) \/ (A /\ C).

Lemma problem_1 : forall (A B C : Prop), A /\ (B \/ C) -> (A /\ B) \/ (A /\ C).
Proof.
  intros A B C.
  intros [H1 H2]. (*A /\ (B \/ C) --> H1: A, H2: B ∨ C*)
  destruct H2 as [Hb | Hc]. (*B ∨ C --> Hb: B, Hc: C*)
  left. split. apply H1. assumption.
  right. split. apply H1. assumption.
Qed.

//2.2. Lemma problem_2 : forall A B C : Prop, ((B -> A) /\ (C -> A)) -> ((B \/ C -> A)).

Lemma problem_2 : forall A B C : Prop, ((B -> A) /\ (C -> A)) -> ((B \/ C -> A)).
Proof.
  intros A B C.
  intros [H1 H2]. (* B ∨ C → A --> B → A, C → A*)
  intros [Hb | Hc]. (*Hb: B, Hc: C*)
  apply H1. assumption.
  apply H2. assumption.
Qed.

//2.3. Lemma problem_3 : forall A B : Prop, (A \/ ~A) -> ((A -> B) -> A) -> A.
Lemma problem_3 : forall A B : Prop, (A \/ ~A) -> ((A -> B) -> A) -> A.
Proof.
   tauto.
Qed.

//2.4. Lemma problem_4 : forall (U : Type) (A B : U -> Prop), (exists x, A x /\ B x) -> (exists x, A x) /\ (exists x, B x).*/

(* 2.1 Distributivitás *)
Lemma problem_1 : forall A B C : Prop, A /\ (B \/ C) -> (A /\ B) \/ (A /\ C).
Proof.
  intros A B C [Ha [Hb | Hc]].
  - left. split; assumption.     (* B esetén A ∧ B *)
  - right. split; assumption.   (* C esetén A ∧ C *)
Qed.

(* 2.2 Implikációk kombinációja *)
Lemma problem_2 : forall A B C : Prop, ((B -> A) /\ (C -> A)) -> (B \/ C -> A).
Proof.
  intros A B C [Hba Hca] [Hb | Hc].
  - apply Hba; assumption.       (* B-ből A *)
  - apply Hca; assumption.       (* C-ből A *)
Qed.

(* 2.3 Klasszikus logikai tétel *)
Require Import Classical_Prop.

Lemma problem_3 : forall A B : Prop, (A \/ ~A) -> ((A -> B) -> A) -> A.
Proof.
  intros A B Hem H.
  destruct Hem as [Ha | Hna].
  - exact Ha.                     (* A igaz *)
  - apply H. intros Ha.           (* A → B *)
    contradiction (Hna Ha).       (* Ellentmondás *)
Qed.

(* 2.4 Egzisztenciális kvantorok kezelése *)
Lemma problem_4 : forall (U : Type) (A B : U -> Prop), 
  (exists x, A x /\ B x) -> (exists x, A x) /\ (exists x, B x).
Proof.
  intros U A B [x [Ha Hb]].
  split.
  - exists x. exact Ha.           (* A x bizonyítása *)
  - exists x. exact Hb.           (* B x bizonyítása *)
Qed.


//----TODO----

// 3. Kombinatorikai modellezés
/*3.1 Írj programot, amelyik kiszámolja, hogy mi annak a valószínűsége, hogy 52 lapos francia kártyából 2 kártyát választva az egyik király, a másik nem király!*/
var kiraly_kartya = function () {
  // két kártya modellezése
  var szin1 = randomInteger(4) + 1;
  var figura1 = randomInteger(13) + 1;
  var szin2 = randomInteger(4) + 1;
  var figura2 = randomInteger(13) + 1;
  var kartya1 = [szin1,figura1];
  var kartya2 = [szin2,figura2];
  condition((szin1 !== szin2 || figura1 !== figura2) && // explicit condition: nem lehet ugyanaz a kártya
            (figura1 == 13 && figura2 != 13)); // az egyik király legyen, a másik ne
  //condition(figura1 == 13 && figura2 != 13); egyszerűbb condition is ugyanazt adja ki (ha a figura nem egyezik, már nem lehet ugyanaz a lap)
  return {kartyapar : [kartya1,kartya2]};
}
  
var eloszlas = Enumerate(kiraly_kartya);
viz.table(eloszlas);

/*3.2 Legyen az X Y és Z valószínűség változó olyan, ami az {0, 1, 2, 3} számok közül választ egyenletes valószínűséggel. Legyen W = X + Y + Z, mi az X változó eloszlása, ha tudjuk, hogy W = 7? (Írj programot!)*/
var szamok = function () {
  var X = uniformDraw([0, 1, 2, 3]);
  var Y = uniformDraw([0, 1, 2, 3]);
  var Z = uniformDraw([0, 1, 2, 3]);
  var W = X + Y + Z;
  condition(W == 7);
  return X;
}
  
var model = Infer({method: 'enumerate'}, szamok);
viz.table(model);

/* 3.3 Értsd meg a Monty Hall/vos Savant paradoxon programját!*/
// jó, megértettem.

/*3.4 Van egy beépített emberünk a Monty Hall/vos Savant szituációban, aki 50% százalékban helyesen mutogatja el nekünk a színfalak mögül, hogy melyik ajtó mögött van az autó. Szót fogadunk neki, és azt az ajtót választjuk, amit ő javasol. Még mindig érdemes-e azután váltani, hogy Monty megmutatot egy kecskét? A választ támasszuk alá webppl programmal!*/

// --> mindegy, hogy váltunk-e vagy sem, 50% az esély Monty kecskéje előtt és után is.
var vosSavantProblem = function () {
    var Autó = categorical({ps:[1/3,1/3,1/3], vs:[1, 2, 3]})

    // beépített emberünk tippje
    var beépítettTipp = flip(0.5) // 50%-ban jól tippel
                         ? Autó // jól tippelt
                         : (Autó == 1 // rosszul tippelt: ha az ajtó az első ajtó mögött van
                            ? categorical({ps:[1/2,1/2], vs:[2,3]}) // akkor a kettesre vagy hármasra tippel
                            : (Autó == 2 // ha az autó a kettes ajtó mögött van
                               ? categorical({ps:[1/2,1/2], vs:[1,3]}) // akkor az egyesre vagy hármasra tippel
                               : categorical({ps:[1/2,1/2], vs:[1,2]}))); // else: ha a hármas ajtó mögött van, akkor az egyesre vagy kettesre tippel
                               
    var Monty = (Autó == beépítettTipp) 
                ? ( (Autó == 1) 
                   ? categorical({ps:[1/2,1/2], vs:[2, 3]}) : 
                   ( (Autó == 2) ? categorical({ps:[1/2,1/2], vs:[1, 3]}) :
                    categorical({ps:[1/2,1/2], vs:[1, 2]}) ) )
                : ( (1 !== Autó && 1 !== beépítettTipp ) ? 1 :
                   ( (2 !== Autó && 2 !== beépítettTipp ) ) ? 2 : 3 )   
    
    var stratégia_maradás = (Autó == beépítettTipp) ? 'nyer' : 'veszít'
    
    var ÚjTipp = (Autó !== beépítettTipp) 
                ? Autó
                : ( (beépítettTipp == 1 && Monty == 2) ? 3 : 
                   ( (beépítettTipp == 1 && Monty == 3) ? 2 : 
                   ( (beépítettTipp == 2 && Monty == 1) ? 3 :
                   ( (beépítettTipp == 2 && Monty == 3) ? 1 :
                   ( (beépítettTipp == 3 && Monty == 1) ? 2 : 1 ) ) ) ) ) 
    
    var stratégia_váltás = (Autó == ÚjTipp) ? 'nyer' : 'veszít'
    
    return  {
             // stratégia_maradás: stratégia_maradás 
             stratégia_váltás: stratégia_váltás
            } 
}

var eloszlás = Enumerate(vosSavantProblem)
viz.marginals(eloszlás)


//4. Valószínűség modellezése
/*4.1 Francia kártyapakliból kiválogatjuk a figurásokat (bubi, dáma, király, ász). Ez 16 lap. Kihúzunk visszatevés nélkül belőlük két lapot. Az alábbi program azokat az eseteket sorolja fel, amikor teljesül az A = "az egyik lap nem kőr vagy a másik lap nem király" esemény. Ezt tekintsük úgy, mint egy olyan P(X,Y) joint eloszlást, ahol a lyukas helyekhez tartozó valószínűség nulla, a többihez egyenletes.

a) Rajzold fel a P(X) = P( X = xi ) = ∑jP( X = xi , Y = yj ) marginális eloszlást (vigyázat! ez nem lesz ugyanaz, mint amit a "marginals" parancs ad vissza a webppl-ben!).*/

var kartya = function () {
  var szin1 = randomInteger(4) + 1;
  var figura1 = randomInteger(4) + 1;
  var szin2 = randomInteger(4) + 1;
  var figura2 = randomInteger(4) + 1;
  var huzas1 = [szin1,figura1];
  var huzas2 = [szin2,figura2];

  condition(
    !(szin1 == szin2 && figura1 == figura2) && // visszatétel nélkül
    // az egyik lap nem kőr vagy a másik lap nem király
    (!(szin1 == 1) || !(figura2 == 3)) &&
    (!(szin2 == 1) || !(figura1 == 3))
  );
           
  return {'P(X)' : huzas1};
};

var eloszlas = Enumerate(kartya);
viz.auto(eloszlas);


/*b) Számoljuk ki a P( X = treff király vagy X = treff ász | Y = pikk dáma ) feltételes valószínűséget!*/

// tömörebb megoldás:
var kartyak = function () {
    var Y = flip(1/16) // pikk dáma esélye
    var X = flip( Y ? 2/15 : 2/16) // treff király vagy treff ász esélye Y-tól függően  
    condition(Y==1); // tegyük fel, hogy a pikk dámát kihúztuk
    return  {'P(X|Y)': [X , Y]} // X valószínűsége
}
var model = Infer({method: 'enumerate', model: kartyak})
viz.table(model)

// hosszabb megoldás:
var kartya = function() {
  // Y = pikk dáma
  var Y_szin = 4;
  var Y_figura = 2;
  // X egy lap a 16 közül (4x4 lehetőség)
  var X_szin = randomInteger(4) + 1;
  var X_figura = randomInteger(4) + 1;
  // X = treff király vagy treff ász
  var X = (X_szin == 2 && X_figura == 3) // treff király
          || (X_szin == 2 && X_figura == 4);    // treff ász
  condition(!(X_szin == Y_szin && X_figura == Y_figura)); // visszatétel nélkül
  var P = X ? 'P(X|Y)' : 'nem jó' 
  return P;
}
var eloszlas = Infer({method: 'enumerate'}, kartya);
viz.table(eloszlas);


/*4.2 A hörcsög súlyának mérési adatai: 28 g, 31 g, 44 g, 29 g. Lexikonbeli adatok: átlagos súly: 32 g, szórás: 10 g. Mi lesz az adatokkal való frissítés után a hörcsög súlyának eloszlása?*/
var hörcsög = function() {
  var p = gaussian({mu: 32, sigma: 10}); // Lexikonbeli adatok: átlagos súly: 32 g, szórás: 10 g
  // 4 megfigyelésónk hozzáadása
  observe(Gaussian({mu: p, sigma: 10}),28)     
  observe(Gaussian({mu: p, sigma: 10}),31)                 
  observe(Gaussian({mu: p, sigma: 10}),44)                 
  observe(Gaussian({mu: p, sigma: 10}),29)                 
  
var poszteriorPrediktiv = gaussian(p,20); // p-vel a szimulált adatok
var priorP = gaussian(32, 10); // érintetlen paraméter
var priorPrediktiv = gaussian(priorP,20); // érintetlen paraméterből szimulált adatok

return {prior: priorP, priorPredictive: priorPrediktiv,
       posterior: p, posteriorPredictive: poszteriorPrediktiv};
};

var output = 
    Infer({model: hörcsög, samples: 1000, method: 'MCMC'});
viz.marginals(output);

