package quickcheck

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(73); 
val h = new BinomialHeap() with IntHeap;System.out.println("""h  : quickcheck.BinomialHeap with quickcheck.IntHeap = """ + $show(h ));$skip(47); val res$0 = 
h.insert(1, h.insert(2, h.insert(3, h.empty)));System.out.println("""res0: quickcheck.test.h.H = """ + $show(res$0));$skip(47); val res$1 = 
h.insert(3, h.insert(2, h.insert(1, h.empty)));System.out.println("""res1: quickcheck.test.h.H = """ + $show(res$1))}
}
