/*
 * Copyright 2021 Stephen Tetley
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

 namespace Cursor3 {


    type alias Cursor3Alg[a] = { hasNext: Unit ~> Bool, next: Unit ~> a, finalize: Unit ~> Unit }


    opaque type Cursor3[a] = Cursor3Alg[a]

    pub def fold(f: (b, a) -> b & ef, ac: b, xs: Cursor3[a]): b & Impure = 
        let Cursor3(xs1) = xs;
        foldHelper(f, ac, xs1)

    def foldHelper(f: (b, a) -> b & ef, ac: b, o: Cursor3Alg[a]): b & Impure = 
        let hasNext = o.hasNext;
        match (hasNext()) {
            case true => { 
                let next = o.next;
                let x1 = next();
                let ac1 = f(ac, x1) as & Impure;
                foldHelper(f, ac1, o)
            }
            case false => {
                let finalize = o.finalize;
                let _ = finalize();
                ac
            }
        }

    pub def map(f: a -> b & ef, xs: Cursor3[a]): Cursor3[b] = 
        let Cursor3(xs1) = xs;
        let next1: Unit ~> b = () -> {let nextF = xs1.next; let a = nextF(); f(a) as & Impure};
        let hasNext1: Unit ~> Bool = xs1.hasNext;
        let finalize1: Unit ~> Unit = xs1.finalize;
        Cursor3({next = next1, hasNext = hasNext1, finalize = finalize1})

 }