

def main(_args: Array[String]): Int64 & Impure = 
    Console.printLine("Running...");
    test01()



def test01(): Int32 & Impure = 
    let s1:PullStream[Int32] = PullStream.range(0, 10);
    let s2:PullStream[Float64] = s1 |> PullStream.map(Int32.toFloat64);
    Console.printLine("average: ${PullStream.average(s2)}");
    Console.printLine("length: ${PullStream.length(s1)}");
    Console.printLine("toList(s1): ${PullStream.toList(s1)}");
    0