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

 test("parejasCandidatas propio: índice vacío da vacío") {
    assert(parejasCandidatas(Map.empty).isEmpty)
  }
  test("parejasCandidatas propio: un fragmento en tres documentos da 3 parejas") {
    val g = Vector("a")
    assert(parejasCandidatas(Map(g -> Set(0, 1, 2))) == Vector((0, 1), (0, 2), (1, 2)))
  }
  test("parejasCandidatas propio: no repite aunque aparezca en varios fragmentos") {
    val idx: Indice = Map(Vector("a") -> Set(0, 1), Vector("b") -> Set(0, 1))
    assert(parejasCandidatas(idx) == Vector((0, 1)))
  }
  test("parejasCandidatas propio: ordenadas por (i, j)") {
    val idx: Indice = Map(Vector("a") -> Set(2, 0), Vector("b") -> Set(0, 1))
    assert(parejasCandidatas(idx) == Vector((0, 1), (0, 2)))
  }
  test("parejasCandidatas propio: conjuntos unitarios no aportan parejas") {
    val idx: Indice = Map(Vector("a") -> Set(0), Vector("b") -> Set(1))
    assert(parejasCandidatas(idx).isEmpty)
  }

  test("ranking propio: umbral alto descarta todo") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.3))
    assert(ranking(ms, 0.9, 10).isEmpty)
  }
  test("ranking propio: empates se rompen por posición") {
    val ms: Vector[Medicion] = Vector((2, 3, 0.5), (0, 1, 0.5), (1, 2, 0.5))
    assert(ranking(ms, 0.0, 10) == Vector((0, 1, 0.5), (1, 2, 0.5), (2, 3, 0.5)))
  }
  test("ranking propio: p = 0 da vacío") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.9))
    assert(ranking(ms, 0.0, 0).isEmpty)
  }
  test("ranking propio: umbral incluye el igual") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.5), (0, 2, 0.4))
    assert(ranking(ms, 0.5, 10).map(_._3) == Vector(0.5))
  }
  test("ranking propio: similitudes ya ordenadas se mantienen") {
    val ms: Vector[Medicion] = Vector((0, 1, 0.9), (1, 2, 0.6), (0, 2, 0.3))
    assert(ranking(ms, 0.0, 10) == ms)
  }

  test("detectarDirecto propio: corpus de un documento da vacío") {
    val c: Corpus = Vector(("a", Vector("x","y","z")))
    assert(detectarDirecto(c, 2, 0.0, 10).isEmpty)
  }
  test("detectarDirecto propio: umbral 1.0 solo deja copias exactas") {
    val c: Corpus = Vector(
      ("a", Vector("p","q","r")),
      ("b", Vector("p","q","r")),
      ("c", Vector("s","t","u")))
    val r = detectarDirecto(c, 2, 1.0, 10)
    assert(r.map(m => (m._1, m._2)) == Vector((0, 1)))
  }
  test("detectarDirecto propio: p trunca el resultado") {
    val c: Corpus = Vector(
      ("a", Vector("p","q","r")), ("b", Vector("p","q","r")),
      ("c", Vector("p","q","r")), ("d", Vector("p","q","r")))
    assert(detectarDirecto(c, 2, 0.0, 2).length == 2)
  }
  test("detectarDirecto propio: ordenado por similitud descendente") {
    val c: Corpus = Vector(
      ("a", Vector("p","q","r","s")),
      ("b", Vector("p","q","r","s")),
      ("c", Vector("p","q","x","y")))
    val r = detectarDirecto(c, 2, 0.0, 10)
    assert(r.map(._3) == r.map(._3).sorted.reverse)
  }
  test("detectarDirecto propio: corpus vacío da vacío") {
    assert(detectarDirecto(Vector.empty, 3, 0.0, 10).isEmpty)
  }

  test("detectarConIndice propio: coincide con directo en corpus sin copias") {
    val c: Corpus = Vector(
      ("a", Vector("p","q","r")), ("b", Vector("s","t","u")), ("c", Vector("v","w","x")))
    assert(detectarConIndice(c, 2, 0.0, 10) == detectarDirecto(c, 2, 0.0, 10))
  }
  test("detectarConIndice propio: descarta parejas sin fragmentos comunes") {
    val c: Corpus = Vector(
      ("a", Vector("p","q","r")), ("b", Vector("s","t","u")))
    assert(detectarConIndice(c, 2, 0.0, 10).isEmpty)
  }
  test("detectarConIndice propio: detecta copia exacta") {
    val c: Corpus = Vector(
      ("a", Vector("p","q","r")), ("b", Vector("p","q","r")))
    val r = detectarConIndice(c, 2, 0.5, 10)
    assert(r.length == 1 && math.abs(r(0)._3 - 1.0) < eps)
  }
  test("detectarConIndice propio: umbral alto puede dejar vacío") {
    val c: Corpus = Vector(
      ("a", Vector("p","q","r")), ("b", Vector("p","q","s")))
    assert(detectarConIndice(c, 2, 0.99, 10).isEmpty)
  }
  test("detectarConIndice propio: corpus de un documento da vacío") {
    val c: Corpus = Vector(("a", Vector("x","y","z")))
    assert(detectarConIndice(c, 2, 0.0, 10).isEmpty)
  }
}