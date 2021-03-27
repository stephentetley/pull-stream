/// Does a traversal deplete a stream?


def main(_args: Array[String]): Int32 & Impure = 
    if (test01()) 
        {Console.printLine("Streams can be replayed"); 0}
    else 
        {Console.printLine("Streams get depleted and cannot be replayed"); 1}

/// It looks like streams are replayable!
def test01(): Bool & Impure = 
    let s1 = PullStream.range(0, 10);
    let sum1 = PullStream.sum(s1);
    Console.printLine("sum: ${sum1}");
    Console.printLine("length: ${PullStream.length(s1)}");
    let sum2 = PullStream.sum(s1);
    Console.printLine("sum again: ${sum2}");
    sum1 == sum2
