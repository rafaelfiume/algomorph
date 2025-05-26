package numbers

class OutliersSpec extends munit.FunSuite:

  test("returns the index of the heaviest ball"):
    assertEquals(Outliers.find(Vector(5)), expected = 0) // N=1
    assertEquals(Outliers.find(Vector(1, 2)), expected = 1) // N=2
    assertEquals(Outliers.find(Vector.tabulate(9) { i => if i == 4 then 2 else 1 }), expected = 4) // N=9
    assertEquals(Outliers.find(Vector.tabulate(11) { i => if i == 8 then 2 else 1 }), expected = 8) // N=11
