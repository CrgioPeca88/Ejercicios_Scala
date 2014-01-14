/**
 * Ejercicios de la pagina http://projecteuler.net/
 */
import scala.annotation.tailrec
import scala.math.BigInt

object Ejercicios3 {

	/**
	 * Ejercicio número 1
	 * http://projecteuler.net/problem=1
	 */
	def problema1(max:Int)={
		@tailrec
		def loop(i:Int, suma:Int):Int={
				if(i == max)suma
				else if(i%3==0 || i%5==0) loop(i+1, suma+i)
				else loop(i+1, suma)
		}
		loop(0,0);
	}

	/**
	 * Ejercicio número 3
	 * http://projecteuler.net/problem=3
	 */
	def problema3(numero:BigInt):Int={
		@tailrec
		def loop(valor:BigInt,numeroComienzo:Int,maxPrimo:Int):Int={
				if(valor==1)maxPrimo
				else if(valor % numeroComienzo==0)loop(valor/numeroComienzo, 2, numeroComienzo)
				else loop(valor, numeroComienzo+1, maxPrimo)
		}
		loop(numero, 2, -1)
	}

	def main(args: Array[String]) {

		println("Problema 1: "+problema1(1000))
		println("Problema 3: "+problema3(BigInt("600851475143")))
	}

}