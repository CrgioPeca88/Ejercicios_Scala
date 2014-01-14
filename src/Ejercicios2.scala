import scala.annotation.tailrec

object Ejercicios2
{
	//Funcion recursiva que calcula el factorial de un numero
	//Funcion optimizada recursiva en cola
	def factorial(n: Int): Int = 
		{	@annotation.tailrec
			def go(n: Int, acc: Int): Int =
			if (n <= 0) acc
			else go(n-1, n*acc)
			go(n, 1)
		}

	//Funcion que calcula el valor absoluto de un numero
	def abs(n: Int): Int = {
			if(n>=0) n else -n
	}

	//Funcion que devuelve un mensaje con un formato
	//Recibe como parametros un nombre de la funcion a calcular, un numero y la funcion a calcular
	//La funcion a calcular necesita recibe un parametro entero y retorna otro entero
	def formatResult(name: String, n: Int, f: Int => Int) = {
		val msg = "El %s de %d is %d."
				msg.format(name, n, f(n))
	}

	//Funcion que calcula fibonacci de un numero
	//Funcion recursiva NO optimizada en cola
	def fib2(n:Int):Int={
		if(n==0||n==1)n
		else fib2(n-2)+fib2(n-1)
	}

	//Funcion que calcula fibonnacci de un numero
	//Funcion recursiva optimizada en cola
	def fib( n : Int) : Int = { 
		@annotation.tailrec
		def fib_tail( n: Int, a:Int, b:Int): Int = n match {
		case 0 => a 
		case _ => fib_tail( n-1, b, a+b )
		}
		fib_tail(n, 0, 1)
	}

	//Funcion abstracta que encuentra el primer elemento en un arreglo que cumple una condicion
	//Funcion recursiva optimizada en cola
	//La condicion se recibe como una funcion
	def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
			@annotation.tailrec
			def loop(n: Int): Int =
			if (n >= ds.length) -1
			else if (p(ds(n))) n
			else loop(n + 1)
			loop(0)
	}

	//Funcion que revisa si un arreglo se encuentra ordenado
	//Funcion recursiva optimizada en cola
	//La condicion se recibe como una funcion
	def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
			@annotation.tailrec
			def loop(x:Int, organizado:Boolean): Boolean={
					if(x == as.length-1) organizado
					else if (gt(as(x),as(x+1))) loop(x+1, true)
					else false 
			}
			loop(0,true)
	}

	//Funcion del merge sort recursiva
	def mergeSort(input: List[Int]) = {

			def merge(left: List[Int], right: List[Int]): Stream[Int] = (left, right) match {
			case (x :: xs, y :: ys) if x <= y => x #:: merge(xs, right)
			case (x :: xs, y :: ys) => y #:: merge(left, ys)
			case _ => if (left.isEmpty) right.toStream else left.toStream
			}

			def sort(input: List[Int], length: Int): List[Int] = input match {
			case Nil | List(_) => input
			case _ =>
			val middle = length / 2
			val (left, right) = input splitAt middle
			merge(sort(left, middle), sort(right, middle + length % 2)).toList
			}
			sort(input, input.length)
	}

	def main(args: Array[String]) 
	{
		println(formatResult("factorial", 10, factorial))
		println(formatResult("valor absoluto", -10, abs))
		println("La funcion recursiva (no optimizada) de fibonacci de 8 es: "+fib2(8))
		println("La funcion recursiva (optimizada) de fibonacci de 8 es: "+fib(8))
		println("La posicion del arreglo (1,8,6,3,7,9,2) donde se encuentra el numero 7 es: "+findFirst(Array(1,8,6,3,7,9,2), (x: Int) => x == 7))
		println("La posicion del arreglo (Hola,Como,Estas) donde se encuentra la palabra estas estas es: "+findFirst(Array("Hola","Como","Estas"), (x:String) => x.equalsIgnoreCase("estas") ))
		println("¿El arreglo (4,6,9,7,1,0,8) esta organizado? "+isSorted(Array(4,6,9,7,1,0,8), (x:Int,y:Int)=> x<=y))
		println("¿El arreglo de letras (a,d,z) esta organizado alfabeticamente? "+isSorted(Array("a","d","z"), (x:String,y:String)=> x.compareTo(y)<0))
		println("Organizar esta lista (0,6,7,4,3,8,9,1,2,5) => "+mergeSort(List(0,6,7,4,3,8,9,1,2,5)))
		
	}
}