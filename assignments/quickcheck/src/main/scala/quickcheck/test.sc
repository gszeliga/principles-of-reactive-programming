package quickcheck

object test {
val h = new BinomialHeap() with IntHeap           //> h  : quickcheck.BinomialHeap with quickcheck.IntHeap = quickcheck.test$$anonf
                                                  //| un$main$1$$anon$1@6ea079d0
h.insert(1, h.insert(2, h.insert(3, h.empty)))    //> res0: quickcheck.test.h.H = List(Node(1,0,List()), Node(2,1,List(Node(3,0,Li
                                                  //| st()))))
h.insert(3, h.insert(2, h.insert(1, h.empty)))    //> res1: quickcheck.test.h.H = List(Node(3,0,List()), Node(1,1,List(Node(2,0,Li
                                                  //| st()))))
}