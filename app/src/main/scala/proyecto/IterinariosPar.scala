package proyecto
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.parallel.CollectionConverters._
import scala.concurrent.{ExecutionContext, Future}

class ItinerariosPar() {
  val itinerarioObj = new Itinerario()
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]

  private val objitinerarioSeq = new Itinerario()

  def itinerariosPar(vuelos: List[Vuelo], aeropuertos:List[Aeropuerto])(implicit ec: ExecutionContext): (String, String) => Future[List[List[Vuelo]]] = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los itinerarios posibles de cod1 a cod2
    def generarItinerarios(cod1: String, cod2: String): Future[List[List[Vuelo]]] = {
      Future {
        objitinerarioSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      }
    }

    generarItinerarios
  }

  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => Future[List[List[Vuelo]]] = {
    def calcularDuracionVuelo(vuelo: Vuelo, aeropuertos: List[Aeropuerto]): Int = {
      val aeropuertoOrigen = aeropuertos.find(_.Cod == vuelo.Org).get
      val aeropuertoDestino = aeropuertos.find(_.Cod == vuelo.Dst).get

      val salidaEnMinutos = (vuelo.HS * 60) + vuelo.MS
      val llegadaEnMinutos = (vuelo.HL * 60) + vuelo.ML

      val diferenciaGMT = (aeropuertoDestino.GMT - aeropuertoOrigen.GMT) / 100
      val diferenciaGMTEnMinutos = (diferenciaGMT * 60).toInt

      val duracionEnMinutos = llegadaEnMinutos - (salidaEnMinutos + diferenciaGMTEnMinutos)

      if (duracionEnMinutos < 0) duracionEnMinutos + 1440 else duracionEnMinutos
    }

    def calcularTiempoEspera(vuelo1: Vuelo, vuelo2: Vuelo): Int = {
      val llegadaEnMinutos = (vuelo1.HL * 60) + vuelo1.ML
      val salidaEnMinutos = (vuelo2.HS * 60) + vuelo2.MS

      val esperaEnMinutos = salidaEnMinutos - llegadaEnMinutos

      if (esperaEnMinutos < 0) esperaEnMinutos + 1440 else esperaEnMinutos
    }

    def calcularTiempoTotal(itinerario: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
      val tiemposDeVuelo = itinerario.map(vuelo => calcularDuracionVuelo(vuelo, aeropuertos))
      val tiemposDeEspera = itinerario.zip(itinerario.tail).map { case (v1, v2) => calcularTiempoEspera(v1, v2) }
      tiemposDeVuelo.sum + tiemposDeEspera.sum
    }

    def minimoTiempo(cod1: String, cod2: String): Future[List[List[Vuelo]]] = {
      val itinerariosFuturo = Future {
        itinerarioObj.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      }

      itinerariosFuturo.map { itinerarios =>
        itinerarios.par.map(it => (it, calcularTiempoTotal(it, aeropuertos)))
          .toList
          .sortBy(_._2)
          .take(3)
          .map(_._1)
      }
    }

    minimoTiempo
  }
  
  def itinerariosEscalasPar(vuelos:List[Vuelo], aeropuertos:List[Aeropuerto]):(String, String)=>List[Itinerario]
  = {
    //Recibe una lista de vuelos y aeropuertos
    //Retorna una función que recibe los codigos de dos aeropuertos
    //Retorna todos los tres mejores itinerarios posibles de cod1 a cod2
    //que minimizan el número de escalas
    (cod1:String, cod2:String)=> List[Itinerario]()
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
