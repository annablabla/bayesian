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


