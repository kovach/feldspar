module Numbers where

open import Data.Nat
open import Data.Bool
open import Relation.Binary.PropositionalEquality
open import Relation.Binary.EqReasoning


--data _≡_ {A : Set} : A → A → Set where
--  refl : (x : A) → x ≡ x

factorial : ℕ → ℕ
factorial zero = 1
factorial (suc n) = (suc n) * factorial n

nat-ind : (P : ℕ → Set) → P 0 → ((n : ℕ) → P n → P (suc n)) → (n : ℕ) → P n
nat-ind p p0 step zero = p0
nat-ind p p0 step (suc n) = step n (nat-ind p p0 step n)

--cong : { A B : Set } → { a b : A } → (f : A → B) → a ≡ b → f a ≡ f b
--cong f (refl x) = refl (f x)

+0 : {a : ℕ} → a ≡ a + 0
+0 {n} = nat-ind (λ a → a ≡ a + 0) (refl) (λ n → cong suc) n

1+m+n=m+1+n : {m n : ℕ} → suc (m + n) ≡ m + (suc n)
1+m+n=m+1+n {m} {n} = nat-ind (λ i → suc (i + n) ≡ i + suc n)
                        refl (λ j → cong suc) m

substitute : {A : Set} → (P : A → Set) → (x y : A) → x ≡ y → P x → P y
substitute p x .x refl px = px

swap : {m n : ℕ} → m + n ≡ n + m → suc (n + m) ≡ m + suc n
swap {m} {n} eq = substitute (λ x → x ≡ m + (suc n) )
                (suc (m + n)) (suc (n + m)) (cong suc eq) (1+m+n=m+1+n {m} {n})

swap2 : {m n : ℕ} → n + m ≡ m + n → suc n + m ≡ m + suc n
swap2 {m} {n} eq = trans (cong suc (eq)) (1+m+n=m+1+n {m} {n})


+comm : {a b : ℕ} → a + b ≡ b + a
+comm {a} {b} = nat-ind (λ a → a + b ≡ b + a) +0 (λ n → swap2 {b} {n}) a

--+assoc : {a b c : ℕ} → a + (b + c) ≡ (a + b) + c
--+assoc = {!!}

data ℤ : Set where
  _——_ : ℕ → ℕ → ℤ

infixl 6 _+ᶻ_
_+ᶻ_ : ℤ → ℤ → ℤ
(p1 —— n1) +ᶻ (p2 —— n2) = (p1 + p2) —— (n1 + n2)

znormalize : ℤ → ℤ
znormalize (suc p —— suc n) = znormalize (p —— n)
znormalize z = z

infixl 5 _=ᶻ_ 
data _=ᶻ_ : ℤ → ℤ → Set where
  zrefl : {x y : ℤ} → (znormalize x ≡ znormalize y) → x =ᶻ y

z+comm : {a b : ℤ} → a +ᶻ b =ᶻ b +ᶻ a
z+comm = {!!}
