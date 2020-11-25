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

namespace TestPullStream {

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def empty01(): Bool & Impure =
    let s = PullStream.empty();
    PullStream.toList(s) == Nil

/////////////////////////////////////////////////////////////////////////////
// range                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def range01(): Bool & Impure =
    let s = PullStream.range(1, 0);
    PullStream.toList(s) == Nil

@test
def range02(): Bool & Impure =
    let s = PullStream.range(1, 1);
    PullStream.toList(s) == Nil

@test
def range03(): Bool & Impure =
    let s = PullStream.range(1, 2);
    PullStream.toList(s) == 1 :: Nil

@test
def range04(): Bool & Impure =
    let s = PullStream.range(1, 3);
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def range05(): Bool & Impure =
    let s =  PullStream.range(1, 4);
    PullStream.toList(s) ==  1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def toList01(): Bool & Impure =
    let s = PullStream.fromList(Nil): PullStream[Int32];
    PullStream.toList(s) == Nil

@test
def toList02(): Bool & Impure =
    let s = PullStream.fromList(0 :: 1 :: Nil);
    PullStream.toList(s) == 0 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// foreach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def foreach01(): Bool & Impure =
    let r = ref 21;
    let s = PullStream.fromList(Nil): PullStream[Int32];
    PullStream.foreach(x -> r := x, s);
    21 == deref r

@test
def foreach02(): Bool & Impure =
    let r = ref 21;
    let s = PullStream.fromList(1 :: 2 :: 3 :: Nil);
    PullStream.foreach(x -> r := x, s);
    3 == deref r

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unfold01(): Bool & Impure =
    let s = PullStream.unfold(st -> if (true) None else Some(Char.fromInt32(st + 48), st + 1), 0);
    PullStream.toList(s) == Nil

@test
def unfold02(): Bool & Impure =
    let s = PullStream.unfold(st -> if (st > 0) None else Some(Char.fromInt32(st + 48), st + 1), 0);
    PullStream.toList(s) == '0' :: Nil

@test
def unfold03(): Bool & Impure =
    let s = PullStream.unfold(st -> if (st > 1) None else Some(Char.fromInt32(st + 48), st + 1), 0);
    PullStream.toList(s) == '0' :: '1' :: Nil

@test
def unfold04(): Bool & Impure =
    let s = PullStream.unfold(st -> if (st >= 10) None else Some(Char.fromInt32(st + 48), st + 1), 0);
    PullStream.toList(s) == '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfold05(): Bool & Impure =
    let s = PullStream.unfold(st -> if (st >= 10) None else Some(Char.fromInt32(st + 48), st + 1), 5);
    PullStream.toList(s) == '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfold06(): Bool & Impure =
    let s = PullStream.unfold(st -> if (st >= 10) None else Some(Char.fromInt32(st + 48), st + 2), 0);
    PullStream.toList(s) == '0' :: '2' :: '4' :: '6' :: '8' :: Nil

/////////////////////////////////////////////////////////////////////////////
// unfoldWithIter                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def unfoldWithIter01(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (true)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    let s = PullStream.unfoldWithIter(step);
    PullStream.toList(s) == Nil

@test
def unfoldWithIter02(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    let s = PullStream.unfoldWithIter(step);
    PullStream.toList(s) == '0' :: Nil

@test
def unfoldWithIter03(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    let s = PullStream.unfoldWithIter(step);
    PullStream.toList(s) == '0' :: '1' :: Nil

@test
def unfoldWithIter04(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    let s = PullStream.unfoldWithIter(step);
    PullStream.toList(s) == '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfoldWithIter05(): Bool & Impure =
    let x = ref 5;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    let s = PullStream.unfoldWithIter(step);
    PullStream.toList(s) == '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfoldWithIter06(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 2;
            Some(c)
        };
    let s = PullStream.unfoldWithIter(step);
    PullStream.toList(s) == '0' :: '2' :: '4' :: '6' :: '8' :: Nil

/////////////////////////////////////////////////////////////////////////////
// map!                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def map01!(): Bool & Impure =
    let s1 = PullStream.fromList(Nil): PullStream[Int32];
    let s2 = PullStream.map!(i -> i > 2, s1): PullStream[Bool];
    PullStream.toList(s2) == Nil

@test
def map02!(): Bool & Impure =
    let s = PullStream.fromList(1 :: Nil) |> PullStream.map!(i -> i > 2);
    PullStream.toList(s) == false :: Nil

@test
def map03!(): Bool & Impure =
    let s = PullStream.fromList(3 :: Nil) |> PullStream.map!(i -> i > 2);
    PullStream.toList(s) == true :: Nil

@test
def map04!(): Bool & Impure =
    let s = PullStream.fromList(1 :: 2 :: Nil) |> PullStream.map!(i -> i > 2);
    PullStream.toList(s) ==  false :: false :: Nil

@test
def map05!(): Bool & Impure =
    let s = PullStream.fromList(1 :: 8 :: Nil) |> PullStream.map!(i -> i > 2);
    PullStream.toList(s) == false :: true :: Nil

@test
def map06!(): Bool & Impure =
    let s = PullStream.fromList(8 :: 1 :: Nil) |> PullStream.map!(i -> i > 2);
    PullStream.toList(s) == true :: false :: Nil

@test
def map07!(): Bool & Impure =
    let s = PullStream.fromList(7 :: 8 :: Nil) |>  PullStream.map!(i -> i > 2);
    PullStream.toList(s) == true :: true :: Nil

/////////////////////////////////////////////////////////////////////////////
// fromList                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def fromList01(): Bool & Impure =
    let s = PullStream.fromList(Nil): PullStream[Int32];
    PullStream.toList(s) == Nil

@test
def fromList02(): Bool & Impure =
    let s = PullStream.fromList(0 :: 1 :: Nil);
    PullStream.toList(s) == 0 :: 1 :: Nil

@test
def fromList03(): Bool & Impure =
    let s = PullStream.fromList(1 :: 0 :: Nil);
    PullStream.toList(s) == 1 :: 0 :: Nil

@test
def fromList04(): Bool & Impure =
    let s = PullStream.fromList('a' :: 'b' :: Nil);
    PullStream.toList(s) == 'a' :: 'b' :: Nil

@test
def fromList05(): Bool & Impure =
    let s = PullStream.fromList("abc" :: "def" :: "ghi" :: Nil);
    PullStream.toList(s) == "abc" :: "def" :: "ghi" :: Nil

/////////////////////////////////////////////////////////////////////////////
// fromArray                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def fromArray01(): Bool & Impure =
    let s = PullStream.fromArray([]: Array[Int32]): PullStream[Int32];
    PullStream.toList(s) == Nil

@test
def fromArray02(): Bool & Impure =
    let s = PullStream.fromArray([0, 1]);
    PullStream.toList(s) == 0 :: 1 :: Nil

@test
def fromArray03(): Bool & Impure =
    let s = PullStream.fromArray([1, 0]);
    PullStream.toList(s) == 1 :: 0 :: Nil

@test
def fromArray04(): Bool & Impure =
    let s = PullStream.fromArray(['a', 'b']);
    PullStream.toList(s) == 'a' :: 'b' :: Nil

@test
def fromArray05(): Bool & Impure =
    let s = PullStream.fromArray(["abc", "def", "ghi"]);
    PullStream.toList(s) == "abc" :: "def" :: "ghi" :: Nil

/////////////////////////////////////////////////////////////////////////////
// cons                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def cons01(): Bool & Impure =
    let s = PullStream.cons(1, PullStream.fromArray([]: Array[Int32]));
    PullStream.toList(s) == 1 :: Nil

@test
def cons02(): Bool & Impure =
    let s = PullStream.cons(1, PullStream.fromArray([2]));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def cons03(): Bool & Impure =
    let s = PullStream.cons(1, PullStream.fromArray([2, 3]));
    PullStream.toList(s) == 1 :: 2 :: 3 :: Nil

@test
def cons04(): Bool & Impure =
    let s = PullStream.cons('a', PullStream.fromArray(['b']));
    PullStream.toList(s) == 'a' :: 'b' :: Nil

@test
def cons05(): Bool & Impure =
    let s = PullStream.cons("abc", PullStream.fromArray(["def", "ghi"]));
    PullStream.toList(s) == "abc" :: "def" :: "ghi" :: Nil

/////////////////////////////////////////////////////////////////////////////
// view                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def view01(): Bool & Impure =
    let s = PullStream.fromList(Nil): PullStream[Int32];
    match PullStream.view(s) {
        case None => true
        case _ => false
    }

@test
def view02(): Bool & Impure =
    let s = PullStream.fromList(1 :: Nil);
    match PullStream.view(s) {
        case Some(1, s2) => PullStream.toList(s2) == Nil
        case _ => false
    }

@test
def view03(): Bool & Impure =
    let s = PullStream.fromList(1 :: 2 :: Nil);
    match PullStream.view(s) {
        case Some(1, s2) => PullStream.toList(s2) == 2 :: Nil
        case _ => false
    }

@test
def view04(): Bool & Impure =
    let s = PullStream.fromList(1 :: 2 :: 3 :: Nil);
    match PullStream.view(s) {
        case Some(1, s2) => PullStream.toList(s2) == 2 :: 3 :: Nil
        case _ => false
    }

@test
def view05(): Bool & Impure =
    let s = PullStream.fromList("abc" :: "def" :: "ghi" :: Nil);
    match PullStream.view(s) {
        case Some("abc", s2) => PullStream.toList(s2) == "def" :: "ghi" :: Nil
        case _ => false
    }

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def zip01(): Bool & Impure =
    let s = PullStream.zip(PullStream.empty(): PullStream[Int32], PullStream.empty(): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def zip02(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(1 :: Nil), PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def zip03(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(Nil): PullStream[Int32], PullStream.fromList(2 :: Nil));
    PullStream.toList(s) == Nil

@test
def zip04(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(1 :: Nil), PullStream.fromList(2 :: Nil));
    PullStream.toList(s) == (1, 2) :: Nil

@test
def zip05(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(1 :: 3 :: Nil), PullStream.fromList(2 :: 4 :: Nil));
    PullStream.toList(s) == (1, 2) :: (3, 4) :: Nil

@test
def zip06(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(1 :: 3 :: 5 :: Nil), PullStream.fromList(2 :: 4 :: 6 :: Nil));
    PullStream.toList(s) ==(1, 2) :: (3, 4) :: (5, 6) :: Nil

@test
def zip07(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(1 :: 3 :: 5 :: 7 :: Nil), PullStream.fromList(2 :: 4 :: 6 :: 8 :: Nil));
    PullStream.toList(s) == (1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil

@test
def zip08(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(1 :: 3 :: 5 :: 7 :: Nil), PullStream.fromList('2' :: '4' :: '6' :: '8' :: Nil));
    PullStream.toList(s) == (1, '2') :: (3, '4') :: (5, '6') :: (7, '8') :: Nil

@test
def zip09(): Bool & Impure =
    let s = PullStream.zip(PullStream.fromList(1 :: 3 :: 5 :: 7 :: Nil), PullStream.fromList("two" :: "four" :: "six" :: "eight" :: Nil));
    PullStream.toList(s) == (1, "two") :: (3, "four") :: (5, "six") :: (7, "eight") :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def zipWith01(): Bool & Impure =
    let s1 = PullStream.fromList(Nil): PullStream[Int32];
    let s2 = PullStream.fromList(Nil): PullStream[Bool];
    let s3 = PullStream.zipWith((a, b) -> if (b) a+1 else a, s1, s2): PullStream[Int32];
    PullStream.toList(s3) == Nil

@test
def zipWith02(): Bool & Impure =
    let s = PullStream.zipWith((a, b) -> if (b) a+1 else a, PullStream.fromList(1 :: Nil), PullStream.fromList(Nil): PullStream[Bool]);
    PullStream.toList(s) == Nil

@test
def zipWith03(): Bool & Impure =
    let s = PullStream.zipWith((a, b) -> if (b) a+1 else a, PullStream.fromList(Nil): PullStream[Int32], PullStream.fromList(true :: Nil));
    PullStream.toList(s) == Nil

@test
def zipWith04(): Bool & Impure =
    let s = PullStream.zipWith((a, b) -> if (b) a+1 else a, PullStream.fromList(1 :: Nil), PullStream.fromList(true :: Nil));
    PullStream.toList(s) == 2 :: Nil

@test
def zipWith05(): Bool & Impure =
    let s = PullStream.zipWith((a, b) -> if (b) a+1 else a, PullStream.fromList(1 :: Nil), PullStream.fromList(false :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def zipWith06(): Bool & Impure =
    let s = PullStream.zipWith((a, b) -> if (b) a+1 else a, PullStream.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil),
                      PullStream.fromList(false :: true :: true :: false :: false :: true :: true :: true ::Nil));
    PullStream.toList(s) == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil

@test
def zipWith07(): Bool & Impure =
    let s = PullStream.zipWith((a,b) -> a + b, PullStream.fromList(1 :: 3 :: 5 :: 7 :: Nil), PullStream.fromList(2 :: 4 :: 6 :: 8 :: Nil));
    PullStream.toList(s) == 3 :: 7 :: 11 :: 15 :: Nil

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def fold01(): Bool & Impure =
    PullStream.fold((i, e) -> (i - e)*(e % 2 + 1), 100, PullStream.fromList(Nil): PullStream[Int32]) == 100

@test
def fold02(): Bool & Impure =
    PullStream.fold((i, e) -> (i - e)*(e % 2 + 1), 100, PullStream.fromList(1 :: Nil)) == 198

@test
def fold03(): Bool & Impure =
    PullStream.fold((i, e) -> (i - e)*(e % 2 + 1), 100, PullStream.fromList(1 :: 2 :: Nil)) == 196

@test
def fold04(): Bool & Impure =
    PullStream.fold((i, e) -> (i - e)*(e % 2 + 1), 100, PullStream.fromList(1 :: 2 :: 3 :: Nil)) == 386

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def append01(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(Nil): PullStream[Int32], PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def append02(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(Nil): PullStream[Int32], PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def append03(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(Nil): PullStream[Int32], PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def append04(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(1 :: Nil), PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) ==  1 :: Nil

@test
def append05(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(1 :: 2 :: Nil), PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def append06(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(1 :: Nil), PullStream.fromList(2 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def append07(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(1 :: 2 :: Nil), PullStream.fromList(3 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: 3 :: Nil

@test
def append08(): Bool & Impure =
    let s = PullStream.append(PullStream.fromList(1 :: Nil), PullStream.fromList(2 :: 3 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def take01(): Bool & Impure =
    let s = PullStream.take(-1, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def take02(): Bool & Impure =
    let s = PullStream.take(0, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def take03(): Bool & Impure =
    let s = PullStream.take(1, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil
@test

def take04(): Bool & Impure =
    let s = PullStream.take(-1, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == Nil

@test
def take05(): Bool & Impure =
    let s = PullStream.take(0, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == Nil

@test
def take06(): Bool & Impure =
    let s = PullStream.take(1, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def take07(): Bool & Impure =
    let s = PullStream.take(2, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def take08(): Bool & Impure =
    let s = PullStream.take(0, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == Nil

@test
def take09(): Bool & Impure =
    let s = PullStream.take(1, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def take10(): Bool & Impure =
    let s = PullStream.take(2, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def take11(): Bool & Impure =
let s = PullStream.take(2, PullStream.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def take12(): Bool & Impure =
    let s = PullStream.take(4, PullStream.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhile01(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def takeWhile02(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == Nil

@test
def takeWhile03(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(4 :: Nil));
    PullStream.toList(s) == 4 :: Nil

@test
def takeWhile04(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == Nil

@test
def takeWhile05(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(1 :: 5 :: Nil));
    PullStream.toList(s) == Nil

@test
def takeWhile06(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(5 :: 1 :: Nil));
    PullStream.toList(s) == 5 :: Nil

@test
def takeWhile07(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(5 :: 8 :: Nil));
    PullStream.toList(s) == 5 :: 8 :: Nil

@test
def takeWhile08(): Bool & Impure =
    let s = PullStream.takeWhile(i -> i > 3, PullStream.fromList(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil));
    PullStream.toList(s) == 4 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def drop01(): Bool & Impure =
    let s = PullStream.drop(-1, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def drop02(): Bool & Impure =
    let s = PullStream.drop(0, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def drop03(): Bool & Impure =
    let s = PullStream.drop(1, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def drop04(): Bool & Impure =
    let s = PullStream.drop(-1, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def drop05(): Bool & Impure =
    let s = PullStream.drop(0, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def drop06(): Bool & Impure =
    let s = PullStream.drop(1, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == Nil

@test
def drop07(): Bool & Impure =
    let s = PullStream.drop(2, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == Nil

@test
def drop08(): Bool & Impure =
    let s = PullStream.drop(0, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def drop09(): Bool & Impure =
    let s = PullStream.drop(1, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 2 :: Nil

@test
def drop10(): Bool & Impure =
    let s = PullStream.drop(2, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == Nil

@test
def drop11(): Bool & Impure =
    let s = PullStream.drop(2, PullStream.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil));
    PullStream.toList(s) == 3 :: 4 :: 5 :: 6 :: Nil

@test
def drop12(): Bool & Impure =
    let s = PullStream.drop(4, PullStream.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil));
    PullStream.toList(s) == 5 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhile01(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def dropWhile02(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def dropWhile03(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(4 :: Nil));
    PullStream.toList(s) == Nil

@test
def dropWhile04(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def dropWhile05(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(1 :: 5 :: Nil));
    PullStream.toList(s) == 1 :: 5 :: Nil

@test
def dropWhile06(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(5 :: 1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def dropWhile07(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(5 :: 8 :: Nil));
    PullStream.toList(s) == Nil

@test
def dropWhile08(): Bool & Impure =
    let s = PullStream.dropWhile(i -> i > 3, PullStream.fromList(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil));
    PullStream.toList(s) == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def filter01(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def filter02(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(2 :: Nil));
    PullStream.toList(s) == Nil

@test
def filter03(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(4 :: Nil));
    PullStream.toList(s) == 4 :: Nil

@test
def filter04(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(1 :: 3 :: Nil));
    PullStream.toList(s) == Nil

@test
def filter05(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(1 :: 8 :: Nil));
    PullStream.toList(s) == 8 :: Nil

@test
def filter06(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(8 :: 1 :: Nil));
    PullStream.toList(s) == 8 :: Nil

@test
def filter07(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(8 :: 9 :: Nil));
    PullStream.toList(s) == 8 :: 9 :: Nil

@test
def filter08(): Bool & Impure =
    let s = PullStream.filter(i -> i > 3, PullStream.fromList(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil));
    PullStream.toList(s) == 4 :: 11 :: 17 :: Nil

/////////////////////////////////////////////////////////////////////////////
// sameElements                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def sameElements01(): Bool & Impure =
    let s1 = PullStream.fromList(Nil): PullStream[Int32];
    let s2 = PullStream.fromList(Nil): PullStream[Int32];
    PullStream.sameElements(s1, s2) == true

@test
def sameElements02(): Bool & Impure =
    let s1 = PullStream.fromList(1 :: Nil);
    let s2 = PullStream.fromList(Nil): PullStream[Int32];
    PullStream.sameElements(s1, s2) == false

@test
def sameElements03(): Bool & Impure =
    let s1 = PullStream.fromList(Nil): PullStream[Int32];
    let s2 = PullStream.fromList(1 :: Nil);
    PullStream.sameElements(s1, s2) == false

@test
def sameElements04(): Bool & Impure =
    let s1 = PullStream.fromList(1 :: Nil);
    let s2 = PullStream.fromList(1 :: Nil);
    PullStream.sameElements(s1, s2) == true

@test
def sameElements05(): Bool & Impure =
    let s1 = PullStream.fromList(1 :: Nil);
    let s2 = PullStream.fromList(0 :: Nil);
    PullStream.sameElements(s1, s2) == false

@test
def sameElements06(): Bool & Impure =
    let s1 = PullStream.fromList(1 :: 2 :: Nil);
    let s2 = PullStream.fromList(1 :: 2 :: Nil);
    PullStream.sameElements(s1, s2) == true

@test
def sameElements07(): Bool & Impure =
    let s1 = PullStream.fromList(1 :: 2 :: Nil);
    let s2 = PullStream.fromList(2 :: 1 :: Nil);
    PullStream.sameElements(s1, s2) == false

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def slice01(): Bool & Impure =
    let s = PullStream.slice(0, 0, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def slice02(): Bool & Impure =
    let s = PullStream.slice(-1, 1, PullStream.fromList(Nil): PullStream[Int32]);
    PullStream.toList(s) == Nil

@test
def slice03(): Bool & Impure =
    let s = PullStream.slice(0, 0, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == Nil

@test
def slice04(): Bool & Impure =
    let s = PullStream.slice(0, 1, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def slice05(): Bool & Impure =
    let s = PullStream.slice(0, 2, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def slice06(): Bool & Impure =
    let s = PullStream.slice(2, 5, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == Nil

@test
def slice07(): Bool & Impure =
    let s = PullStream.slice(-1, 1, PullStream.fromList(1 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def slice08(): Bool & Impure =
    let s = PullStream.slice(0, 1, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 1 :: Nil

@test
def slice09(): Bool & Impure =
    let s = PullStream.slice(0, 2, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def slice10(): Bool & Impure =
    let s = PullStream.slice(1, 2, PullStream.fromList(1 :: 2 :: Nil));
    PullStream.toList(s) == 2 :: Nil

@test
def slice11(): Bool & Impure =
    let s = PullStream.slice(0, 3, PullStream.fromList(1 :: 2 :: 3 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: 3 :: Nil

@test
def slice12(): Bool & Impure =
    let s = PullStream.slice(0, 2, PullStream.fromList(1 :: 2 :: 3 :: Nil));
    PullStream.toList(s) == 1 :: 2 :: Nil

@test
def slice13(): Bool & Impure =
    let s = PullStream.slice(1, 3, PullStream.fromList(1 :: 2 :: 3 :: Nil));
    PullStream.toList(s) == 2 :: 3 :: Nil

@test
def slice14(): Bool & Impure =
    let s = PullStream.slice(1, 2, PullStream.fromList(1 :: 2 :: 3 :: Nil));
    PullStream.toList(s) == 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def repeat01(): Bool & Impure =
    let s = PullStream.repeat(1, -1);
    PullStream.toList(s) == Nil

@test
def repeat02(): Bool & Impure =
    let s = PullStream.repeat(1, 0);
    PullStream.toList(s) == Nil

@test
def repeat03(): Bool & Impure =
    let s = PullStream.repeat(1, 1);
    PullStream.toList(s) == 1 :: Nil

@test
def repeat04(): Bool & Impure =
    let s = PullStream.repeat(1, 2);
    PullStream.toList(s) == 1 :: 1 :: Nil

@test
def repeat05(): Bool & Impure =
    let s = PullStream.repeat(1, 3);
    PullStream.toList(s) == 1 :: 1 :: 1 :: Nil

}
