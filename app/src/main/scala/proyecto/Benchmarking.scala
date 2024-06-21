package proyecto

import org.scalameter
import org.scalameter.{Quantity, Warmer, measure, withWarmer}
import scala.concurrent.Future

class Benchmarking {
  def itinerarios(itinerarioSeq: (String, String) => List[List[Vuelo]], itinerarioPar: (String, String) => Future[List[List[Vuelo]]]): (Quantity[Double], Quantity[Double]) = {
    val seq = withWarmer(new Warmer.Default) measure {
      itinerarioSeq("DFW", "ATL")
    }

    val par = withWarmer(new Warmer.Default) measure {
      itinerarioPar("DFW", "ATL")
    }

    (seq, par)
  }

  def itinerariosSalida(itinerarioSeq: (String, String, Int, Int) => List[Vuelo], itinerarioPar: (String, String, Int, Int) => Future[List[Vuelo]]): (Quantity[Double], Quantity[Double]) = {
    val seq = withWarmer(new Warmer.Default) measure {
      itinerarioSeq("DFW", "ATL", 16, 5)
    }

    val par = withWarmer(new Warmer.Default) measure {
      itinerarioPar("DFW", "ATL", 16, 5)
    }

    (seq, par)
  }

  def mostrarPrueba(seq: Quantity[Double], par: Quantity[Double], funcion: String): String = {
    val aceleracion = seq.value / par.value
    s"Tiempo de $funcion: Secuencial: $seq, Paralelo: $par, Aceleracion: $aceleracion"
  }
}
