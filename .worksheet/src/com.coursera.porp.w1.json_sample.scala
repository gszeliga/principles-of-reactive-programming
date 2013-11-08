package com.coursera.porp.w1

object json_sample {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(487); 

  val data = new JObj(Map(
  	"firstname" -> JStr("John"),
  	"lastname" -> JStr("Smith"),
  	"address" -> JObj(Map(
  		"streetAddress" -> JStr("21 2nd Street"),
  		"state" -> JStr("NY"),
  		"postalCode" -> JNum(10021)
  	)),
  	"phoneNumbers" -> JSeq(List(
  		JObj(Map(
  			"type" -> JStr("home"),"number" -> JStr("212 555-1234")
  		)),
  		JObj(Map(
  			"type" -> JStr("fax"),"number" -> JStr("646 555-4567")
  		))
  	))
  ));System.out.println("""data  : com.coursera.porp.w1.JObj = """ + $show(data ));$skip(16); val res$0 = 
  
  show(data);System.out.println("""res0: String = """ + $show(res$0));$skip(54); 
 
 val f: String => String = { case "ping" => "pong"};System.out.println("""f  : String => String = """ + $show(f ));$skip(67); 
 val pf: PartialFunction[String,String] = { case "ping" => "pong"};System.out.println("""pf  : PartialFunction[String,String] = """ + $show(pf ));$skip(13); val res$1 = 
 
 f("ping");System.out.println("""res1: String = """ + $show(res$1));$skip(26); val res$2 = 
 
 pf.isDefinedAt("ping");System.out.println("""res2: Boolean = """ + $show(res$2));$skip(24); val res$3 = 
 pf.isDefinedAt("pong");System.out.println("""res3: Boolean = """ + $show(res$3))}
}
