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

  test("huella propio: repetidos cuentan una vez"){
    val d: Documento = ("x", Vector("a","b","a","b","a"))
    assert(huella(d,2) == Set(Vector("a","b"), Vector("b", "a")))
  }

  test("huella propio: documento más corto que k da vacío"){
    val d: Documento = ("x", Vector("a", "b"))
    assert(huella(d, 5).isEmpty)
  }

  test("huella propio: k = 1 es el conjunto de palabras"){
    val d: Documento = ("x", Vector("a","b","a"))
    assert(huella(d,1) == Set(Vector("a"), Vector("b")))
  }

  test("huella propio: todas iguales da un fragmento"){
    val d: Documento = ("x", Vector("a","a","a","a"))
    assert(huella(d,2) == Set(Vector("a","a")))
  }

  test("huella propio: documento vacío"){
    assert(huella(("x", Vector.empty), 3).isEmpty)
  }

  test("huellas propio: tamaño igual al corpus"){
    val c: Corpus = Vector(("a", Vector("x","y")), ("b", Vector("z","w")))
    assert(huellas(c,2).length == 2)
  }

  test("huellas propio: corpus vacío"){
    assert(huellas(Vector.empty, 3).isEmpty)
  }

  test("huellas propio: conserva el orden de documentos"){
    val c: Corpus = Vector(("a", Vector("p","q","r")), ("b", Vector("p","q","r")))
    val hs = huellas(c,2)
    assert(hs(0) == hs(1))
  }

  test("huellas propio: cada huella coincide con huella individual"){
    val c: Corpus = Vector(("a", Vector("m","n","o")), ("b", Vector("n","o","p")))
    assert(huellas(c, 2)(0) == huella(c(0), 2))
  }

  test("huellas propio: documentos cortos dan huellas vacías"){
    val c: Corpus = Vector(("a", Vector("x")), ("b", Vector("y")))
    assert(huellas(c, 3).forall(_.isEmpty))
  }


}
