package com.coursera.porp.w1

object json_sample {

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
  ))                                              //> data  : com.coursera.porp.w1.JObj = JObj(Map(firstname -> JStr(John), lastna
                                                  //| me -> JStr(Smith), address -> JObj(Map(streetAddress -> JStr(21 2nd Street),
                                                  //|  state -> JStr(NY), postalCode -> JNum(10021.0))), phoneNumbers -> JSeq(List
                                                  //| (JObj(Map(type -> JStr(home), number -> JStr(212 555-1234))), JObj(Map(type 
                                                  //| -> JStr(fax), number -> JStr(646 555-4567)))))))
  
  show(data)                                      //> res0: String = {"firstname": "John", "lastname": "Smith", "address": {"stree
                                                  //| tAddress": "21 2nd Street", "state": "NY", "postalCode": 10021.0}, "phoneNum
                                                  //| bers": [{"type": "home", "number": "212 555-1234"}, {"type": "fax", "number"
                                                  //| : "646 555-4567"}]}
 
 val f: String => String = { case "ping" => "pong"}
                                                  //> f  : String => String = <function1>
 val pf: PartialFunction[String,String] = { case "ping" => "pong"}
                                                  //> pf  : PartialFunction[String,String] = <function1>
 
 f("ping")                                        //> res1: String = pong
 
 pf.isDefinedAt("ping")                           //> res2: Boolean = true
 pf.isDefinedAt("pong")                           //> res3: Boolean = false
}