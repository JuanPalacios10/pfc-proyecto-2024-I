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

    val (seq1, par1) = pruebas.itinerarios(itinerarioSeq.itinerarios(vuelosC1, aeropuertos), itinerarioPar.itinerariosPar(vuelosC1, aeropuertos))
    val (seq2, par2) = pruebas.itinerarios(itinerarioSeq.itinerariosTiempo(vuelosC1, aeropuertos), itinerarioPar.itinerariosTiempoPar(vuelosC1, aeropuertos))
    val (seq3, par3) = pruebas.itinerarios(itinerarioSeq.itinerariosEscalas(vuelosC1, aeropuertos), itinerarioPar.itinerariosEscalasPar(vuelosC1, aeropuertos))
    val (seq4, par4) = pruebas.itinerarios(itinerarioSeq.itinerariosAire(vuelosC1, aeropuertos), itinerarioPar.itinerariosAirePar(vuelosC1, aeropuertos))
    val (seq5, par5) = pruebas.itinerariosSalida(itinerarioSeq.itinerariosSalida(vuelosC1, aeropuertos), itinerarioPar.itinerariosSalidaPar(vuelosC1, aeropuertos))

    println(saludo())
    println(pruebas.mostrarPrueba(seq1, par1, "itinerarios"))
    println(pruebas.mostrarPrueba(seq2, par2, "itinerariosTiempo"))
    println(pruebas.mostrarPrueba(seq3, par3, "itinerariosEscalas"))
    println(pruebas.mostrarPrueba(seq4, par4, "itinerariosAire"))
    println(pruebas.mostrarPrueba(seq5, par5, "itinerariosSalida"))
  }
 }
