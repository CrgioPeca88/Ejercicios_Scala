/**
 * Ejercicios de la pagina http://www0.cs.ucl.ac.uk/teaching/3C11/exercises.html
 */
object Ejercicios1 {


	/**
	 * Provide a function to check if a character is alphanumeric (i.e. lower case, upper case or numeric).
	 */
	def esAlfanumerico[A](objeto:A):Boolean = objeto match {
	case _:Int => true
	case _:String => true
	case _:Char => true
	case _ => false
	}

	/**
	 * Define a function intmax which takes a number pair and returns the greater of its two components.
	 */
	def intmax(a:Int,b:Int):Int={
			if(a>=b)a else b
	}

	/**
	 * Write the function plus using a stack recursive style.
	 */
	def plus_resursive(lista:List[Int],comienzo:Int):Int={
			def loop(i:Int,resultado:Int):Int={
					if (i==lista.size)resultado
					else loop(i+1, resultado+lista(i))
			}
			loop(comienzo, 0)
	}

	/**
	 * Realiza la division entre dos enteros haciendo restas
	 * devuelve el piso de la division (numero entero)
	 */
	def divide_floor(a:Int,b:Int):Int ={
			def loop(i:Int,first:Int,second:Int):Int={
			  if (first<second)i
			  else loop(i+1, first-second, second)
			}
			loop(0,a,b)
	}

	def main(args: Array[String]) {
		println("¿2 Es alfanumerico? : "+esAlfanumerico(2))
		println("¿h Es alfanumerico? : "+esAlfanumerico("h"))
		println("¿Null Es alfanumerico? : "+esAlfanumerico(null))
		println("El maximo entre 100 y 101 es: "+intmax(100, 101))
		println("La suma recursiva de la lista (1,2,3,4,5,6,7,8,9) es: "+plus_resursive(List(1,2,3,4,5,6,7,8,9), 0))
		println("La division recursiva utilizando restas entre 64 y 12 es "+divide_floor(64, 12))
	}
}