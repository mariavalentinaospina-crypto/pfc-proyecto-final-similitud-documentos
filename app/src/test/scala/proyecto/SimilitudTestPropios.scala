package proyecto

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import Similitud._

@RunWith(classOf[JUnitRunner])
class SimilitudTestPropios extends AnyFunSuite {
  val eps = 1e-9
   
  test("normalizar propio: puntuación y mayúsculas"){
    assert(normalizar("Hola, MUNDO! 2024.") == Vector("hola", "mundo", "2024"))
  }
  
  test("normalizar propio: solo signos devuelve vacío"){
    assert(normalizar("!!! ??? ...").isEmpty)
  }

  test("normalizar propio: cadena vacía"){
    assert(normalizar("").isEmpty)
  }
  
  test("normalizar propio: dígitos se conservan"){
    assert(normalizar("abc123 def456") == Vector("abc123", "def456"))
  }
  
  test("normalizar propio: separadores múltiple colapsan"){
    assert(normalizar("a b\tc\nd") == Vector("a", "b", "c", "d"))
  }

  test("fragmentos propio: k igual al largo da un solo fragmento"){
    assert(fragmentos(Vector("a", "b", "c"), 3) == Vector(Vector("a", "b", "c")))
  }

  test("fragmentos propio: t < k devuelve vacío"){
    assert(fragmentos(Vector("a", "b"), 5).isEmpty)
  }

  test("fragmentos propio: k = 1 devuelve cada palabra"){
    assert(fragmentos(Vector("x", "y", "z"), 1) ==
      Vector(Vector("x"), Vector("y"), Vector("z")))
  }

  test("fragmentos propio: vector vacío"){
    assert(fragmentos(Vector.empty, 2). isEmpty)
  }

  test("fragmentos propio: cantidad t - k + 1"){
    assert(fragmentos(Vector("a", "b", "c", "e"), 3).length == 3)
  }


}
