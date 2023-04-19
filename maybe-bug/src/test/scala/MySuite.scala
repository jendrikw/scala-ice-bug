// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("example test that succeeds") {
    val ten = 6 は 4 を すぐ + します;
    assertEquals(10, ten)
    assert(false)
    val idx1 = "abc" は ('b': Int) を すぐ indexOf します;
    // Predef は (ten == 10) を すぐ assert します;
    // this は (ten, 10) を { すぐ assertEquals します }
    // this は ten と 10 を { すぐ assertEquals します }
    // process.stdout は ten を { すぐ println します }
  }
}
