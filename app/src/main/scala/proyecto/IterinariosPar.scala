package proyecto

import common._

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.concurrent.{ExecutionContext, Future}

class ItinerariosPar() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[List[Vuelo]] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
    def generarItinerarios(cod1: String, cod2: String): List[List[Vuelo]] = {
      def buscar(actual: String, destino: String, visitados: Set[String] = Set()): List[List[Vuelo]] = {
        if (actual == destino) List(List())
        else {
          for {
            v <- vuelos if v.Org == actual && !visitados.contains(v.Dst)
            i <- buscar(v.Dst, destino, visitados + actual)
          } yield v::i
        }
      }

      buscar(cod1, cod2)
    }

    generarItinerarios
  }

  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe vuelos, una lista de vuelos y aeropuertos, una lista de aeropuertos y retorna una funcion que recibe dos strings y retorna una lista de itinerarios
    //Devuelve una función que recibe c1 y c2, códigos de aeropuertos
    //y devuelve una función que devuelve los tres (si los hay) itinerarios que minimizan el tiempo total de viaje
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])
                           (implicit ec: ExecutionContext): (String, String) => Future[List[List[Vuelo]]] = {

    def minimoEscalas(cod1: String, cod2: String): Future[List[List[Vuelo]]] = {

      def calcularEscalas(itinerario: List[Vuelo]): Int = {
        val escExp = itinerario.count(v => v.Dst != cod2)
        val escTec = itinerario.map(v => v.Esc)
        escExp + escTec.sum
      }

      def encontrarMenor(pivote: List[Vuelo], its: List[List[Vuelo]]): Boolean = {
        its.forall(it => calcularEscalas(pivote) <= calcularEscalas(it))
      }

      def buscarVuelo(busqueda: List[Vuelo], its: List[List[Vuelo]]): List[Vuelo] = {
        val primero = its.find(it => calcularEscalas(it) == calcularEscalas(busqueda) && it.length < busqueda.length)

        primero match {
          case Some(value) => value
          case None => busqueda
        }
      }

      def minimoEscalasAux(its: List[List[Vuelo]], itsFiltrada: List[List[Vuelo]]): List[List[Vuelo]] = {
        its match {
          case Nil => Nil
          case h :: t =>
            if (encontrarMenor(h, itsFiltrada)) {
              val menor = buscarVuelo(h, itsFiltrada)
              menor :: minimoEscalasAux(t, itsFiltrada.filter(it => it != menor))
            } else {
              minimoEscalasAux(t, itsFiltrada)
            }
        }
      }

      Future {
        val itsAll = itinerariosPar(vuelos, aeropuertos)(cod1, cod2)
        minimoEscalasAux(itsAll, itsAll)
      }
    }

    minimoEscalas
  }

  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el tiempo en itinerarios
    (cod1:String, cod2:String)=> List[Itinerario]()
  }

  def itinerariosSalidaPar(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto]): (String, String, Int, Int) => List[Itinerario] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos y dos enteros, que es la hora de la cita
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que permiten llegar a una hora de la cita
    (cod1:String, cod2:String, HC:Int, MC:Int)=> List[Itinerario]()
  }

}
