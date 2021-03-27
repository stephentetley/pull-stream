/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace StateMonad {

    pub opaque type StateM[s, a] = s ~> (s, a)

    def apply1(ma: StateM[s, a], s1: s): (s, a) & Impure = 
        let StateM(f) = ma;
        f(s1)

    pub def runStateM(ma: StateM[s, a], s1: s): (s, a) & Impure = apply1(ma, s1)

    pub def return(a: a): StateM[s,a] = StateM(s -> (s, a) as & Impure)

    pub def map(f: a -> b & ef, ma: StateM[s, a]): StateM[s, b] = 
        StateM(s -> 
            let (s1, a) = apply1(ma, s) ;
            let ans = f(a) as & Impure;
            (s1, ans)
        )
    
    pub def ap(mf: StateM[s, a -> b & ef], ma: StateM[s, a]): StateM[s, b] =
        StateM(s ->
            let (s1, f) = apply1(mf, s);
            let (s2, a) = apply1(ma, s1);
            let ans = f(a) as & Impure;
            (s2, ans)
        )

    pub def bind(ma: StateM[s, a], k: a -> StateM[s, b]): StateM[s, b] = 
        StateM(s -> 
            let (s1, a) = apply1(ma, s);
            apply1(k(a), s1)
        )

    pub def flatMap(k: a -> StateM[s, b], ma: StateM[s, a]): StateM[s, b] = bind(ma, k)

    pub def get(): StateM[s, s] = StateM(s -> (s, s) as & Impure)

    pub def set(s1: s): StateM[s, Unit] = StateM(_ -> (s1, ()) as & Impure)

    pub def setsX(f: s -> s): StateM[s, Unit] = StateM(s -> (f(s), ()) as & Impure)
    
    pub def sets(f: s -> (s, a)): StateM[s, a] = StateM(s -> f(s) as & Impure)

    pub def liftA2(f: (a,b) -> c & ef, ma: StateM[s, a], mb: StateM[s, b]): StateM[s, c] = 
        StateM(s ->
            let (s1, a) = apply1(ma, s);
            let (s2, b) = apply1(mb, s1);
            let ans = f(a, b) as & Impure;
            (s2, ans)
        )
    

    pub def seqL(ma: StateM[s,a], mb: StateM[s,b]): StateM[s,a] = 
        StateM(s ->
            let (s1, a) = apply1(ma, s);
            let (s2, _) = apply1(mb, s1);
            (s2, a)
        )

    pub def seqR(ma: StateM[s,a], mb: StateM[s,b]): StateM[s,b] = 
        StateM(s ->
            let (s1,_) = apply1(ma, s);
            apply1(mb, s1)            
        )


    /// No notion of failure / choice so no `mzero` & `mplus`

    
    pub def mapM(f: a -> StateM[s,b], xs: List[a]): StateM[s, List[b]] = 
        StateM(s -> mapMHelper(f, s, xs, (s1, ks) -> (s1, ks)))
        
    def mapMHelper( f: a -> StateM[s,b]
                  , s: s, xs: List[a]
                  , k: (s, List[b]) -> (s, List[b])): (s, List[b]) & Impure = match xs {
        case Nil => k(s, Nil)
        case x :: rs => {
            let (s1, a) = apply1(f(x), s);
            mapMHelper(f, s1, rs, (s2, ks) -> k(s2, a :: ks))
        }
    }

    pub def liftAction(fn: Unit -> a & ef): StateM[s, a] = 
        StateM(s ->
            let ans = fn() as & Impure;            
            (s, ans)
        )

}