/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package proyecto

import datos._
import scala.concurrent.ExecutionContext.Implicits.global

object App{

  def saludo() = "Proyecto final"

  def main(args: Array[String]): Unit = {
    val itinerarioSeq = new Itinerario()
    val itinerarioPar = new ItinerariosPar()
    val pruebas = new Benchmarking()
    val (seq, par) = pruebas.itinerarios(itinerarioSeq.itinerariosTiempo(vuelosC1, aeropuertos), itinerarioPar.itinerariosTiempoPar(vuelosC1, aeropuertos))
    println(saludo())
    println("Secuencial itienerariotiempo: " + seq, "Paralelo itinerariotiempo: " + par)
  }
 }
