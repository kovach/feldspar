module Mat where

open import Data.Nat
open import Data.Bool

data Vec (A : Set) : ℕ -> Set where
  [] : Vec A zero
  _::_ : {n : ℕ} (x : A) (xs : Vec A n) -> Vec A (suc n)

length : {n : ℕ } {A : Set} -> Vec A n -> ℕ
length = λ {n} {A} _ → n

infixr 5 _×_ _++_
data Mat : ℕ → ℕ → Set where
  unit : Mat 1 1
  _||_ : {k m n : ℕ} → Mat k m → Mat k n → Mat k (m + n)
  _//_ : {k m n : ℕ} → Mat m k → Mat n k → Mat (m + n) k
  _×_  : {k m n : ℕ} → Mat k m → Mat m n → Mat k n
  _++_ : {k m : ℕ} → Mat k m → Mat k m → Mat k m
  diag : (n : ℕ) → Mat n n
  inv  : {n : ℕ} → Mat n n → Mat n n
  _ᵀ   : {k m : ℕ} → Mat k m → Mat m k

t1 = 
 let row12 = (unit || unit) 
     col12 = (unit // unit)
 in
   (row12 × row12 ᵀ)


data Pair (A : Set) (B : Set) : Set where
  _,_ : A → B → Pair A B

Operator : ℕ → Set
Operator n = Mat n n
Covariance = Operator
State : ℕ → Set
State n = Mat n 1

predict : {dim : ℕ}
        → Operator dim → State dim → Covariance dim → Covariance dim
        → Pair (State dim) (Covariance dim)
predict f x p q =
  let x' = f × x
      p' = f × q × f ᵀ ++ q
  in
   (x' , p')
