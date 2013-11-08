package com.coursera.porp.w1

trait Generator[+T] {
  
	self => //an alias of "this"
  
	def generate: T
	
	def map[S](f: T => S): Generator[S] = new Generator[S]{
	  def generate = f(self.generate)  //Si utilizamos this.generate sea recursivo dado que el this referencia al método que acabamos de crear
	}
	
	def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S]{
	  def generate = f(self.generate).generate
	}
	
}