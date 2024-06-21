package proyecto
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.concurrent.{ExecutionContext, Future}

class ItinerariosPar() {
  type aeropuertos = List[Aeropuerto]
  type vuelos = List[Vuelo]
  val objitinerarioSeq = new Itinerario()

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

  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])(implicit ec: ExecutionContext): (String, String) => Future[List[List[Vuelo]]] = {
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
        val itsAll = objitinerarioSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
        minimoEscalasAux(itsAll, itsAll)
      }
    }

    minimoEscalas
  }

  def itinerariosAirePar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])(implicit ec: ExecutionContext): (String, String) => Future[List[List[Vuelo]]] = {
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

    def calcularTiempoTotal(itinerario: List[Vuelo], aeropuertos: List[Aeropuerto]): Int = {
      itinerario.map(v => calcularDuracionVuelo(v, aeropuertos)).sum
    }
    
    def minimoAirePar(cod1: String, cod2: String): Future[List[List[Vuelo]]] = {
      val itsAllFuture = Future {
        objitinerarioSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      }

      itsAllFuture.map { itsAll =>
        val itsConDuracion = itsAll.map(it => (it, calcularTiempoTotal(it, aeropuertos)))

        itsConDuracion.sortBy(_._2).map(_._1).take(3)
      }
    }

    minimoAirePar
  }

  def itinerariosSalidaPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto])(implicit ec: ExecutionContext): (String, String, Int, Int) => Future[List[Vuelo]] = {
    def convertirAMinutos(hora: Int, minutos: Int): Int = {
      hora * 60 + minutos
    }

    def minimaSalidaPar(cod1: String, cod2: String, horaCita: Int, minCita: Int): Future[List[Vuelo]] = {
      val tiempoCita = convertirAMinutos(horaCita, minCita)

      // Ejecutar la obtención de itinerarios en un Future para paralelizar
      val itsAllFuture = Future {
        objitinerarioSeq.itinerarios(vuelos, aeropuertos)(cod1, cod2)
      }

      itsAllFuture.map { itsAll =>
        val itsValidos = itsAll.filter(it => convertirAMinutos(it.last.HL, it.last.ML) <= tiempoCita)

        if (itsValidos.isEmpty) List()
        else {
          itsValidos.sortBy(it => (tiempoCita - convertirAMinutos(it.last.HL, it.last.ML), -convertirAMinutos(it.head.HS, it.head.MS))).take(1).head
        }
      }
    }

    minimaSalidaPar
  }
}
