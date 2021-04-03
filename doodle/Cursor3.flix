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


    type alias Cursor3Alg[e, a] = { hasNext: Unit ~> Bool, next: Unit ~> Result[a, e], finalize: Unit ~> Unit }

    type alias Cursor3Impl[e, a] = { hasNext: Unit ~> Bool, next: Unit ~> Step[e, a], finalize: Unit ~> Unit }

    enum Step[e, a] {
        case Ans(a)
        case Skip
        case Failed(e)
    }


    opaque type Cursor3[e, a] = Cursor3Impl[e, a]

    pub def fold(f: (b, a) -> b & ef, ac: b, xs: Cursor3[e, a]): Result[b, e] & Impure = 
        let Cursor3(xs1) = xs;
        foldHelper(f, ac, xs1)

    def foldHelper(f: (b, a) -> b & ef, ac: b, o: Cursor3Impl[e, a]): Result[b, e] & Impure = 
        let hasNext = o.hasNext;
        match (hasNext()) {
            case true => { 
                let next = o.next;
                match next() {
                    case Ans(x1) => {let ac1 = f(ac, x1) as & Impure; foldHelper(f, ac1, o)}
                    case Skip => foldHelper(f, ac, o)
                    case Failed(e) => { 
                        let finalize = o.finalize;
                        let _ = finalize();
                        Err(e)
                    }
                }
            }
            case false => {
                let finalize = o.finalize;
                let _ = finalize();
                Ok(ac)
            }
        }

    pub def map(f: a -> b & ef, xs: Cursor3[e, a]): Cursor3[e, b] = 
        let Cursor3(xs1) = xs;
        let next1 = () -> {let nextF = xs1.next; let x1 = nextF(); mapStep(f, x1)};
        let hasNext1 = xs1.hasNext;
        let finalize1 = xs1.finalize;
        Cursor3({next = next1, hasNext = hasNext1, finalize = finalize1})
        
    def mapStep(f: a -> b & ef, x: Step[e, a]): Step[e, b] & Impure = match x {
        case Ans(x1) => Ans(f(x1))
        case Skip => Skip
        case Failed(e) => Failed(e)
    } as & Impure


    pub def filter(f: a -> Bool, xs: Cursor3[e, a]): Cursor3[e, a] = 
        let Cursor3(xs1) = xs;
        let next1 = () -> {let nextF = xs1.next; let x1 = nextF(); filterStep(f, x1)};
        let hasNext1 = xs1.hasNext;
        let finalize1 = xs1.finalize;
        Cursor3({next = next1, hasNext = hasNext1, finalize = finalize1})
        
    def filterStep(f: a -> Bool, x: Step[e, a]): Step[e, a] = match x {
        case Ans(x1) => if (f(x1)) Ans(x1) else Skip
        case Skip => Skip
        case Failed(e) => Failed(e)
    }


    pub def foreach(f: a ~> Unit, xs: Cursor3[e, a]): Result[Unit, e] & Impure =
        let Cursor3(xs1) = xs;
        foreachHelper(f, xs1)

    def foreachHelper(f: a ~> Unit, o: Cursor3Impl[e, a]): Result[Unit, e] & Impure = 
        let hasNext = o.hasNext;
        match (hasNext()) {
            case true => { 
                let next = o.next;
                match next() {
                    case Ans(x1) => {let _ = f(x1); foreachHelper(f, o)}
                    case Skip => foreachHelper(f, o)
                    case Failed(e) => { 
                        let finalize = o.finalize;
                        let _ = finalize();
                        Err(e)
                    }
                }
            }
            case false => {
                let finalize = o.finalize;
                finalize();
                Ok()
            }
        }

    ///
    /// This should only be called after a call to `hasNext` has indicated more data is available.
    ///  
    pub def next!(xs: Cursor3[e, a]): Result[a, e] & Impure = 
        let Cursor3(xs1) = xs;
        let next = xs1.next;
        match next() { 
            case Ans(a) => Ok(a)
            case Skip => next!(xs)
            case Failed(e) => Err(e)
        }
        
    pub def hasNext(xs: Cursor3[e, a]): Bool & Impure = 
        let Cursor3(xs1) = xs;
        let hasNext = xs1.hasNext;
        hasNext()

    pub def finalize!(xs: Cursor3[e, a]): Unit & Impure = 
        let Cursor3(xs1) = xs;
        let finalize = xs1.finalize;
        finalize()
        

 }
