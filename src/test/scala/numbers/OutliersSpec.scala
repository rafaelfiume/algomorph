package numbers

class OutliersSpec extends munit.FunSuite:

  test("returns the index of the heaviest ball"):
    assertEquals(Outliers.findByWeighing(Vector(5)), expected = 0) // N=1
    assertEquals(Outliers.findByWeighing(Vector(1, 2)), expected = 1) // N=2
    assertEquals(Outliers.findByWeighing(Vector.tabulate(9) { i => if i == 4 then 2 else 1 }), expected = 4) // N=9
    println(Vector.tabulate(9) { i => if i == 4 then 2 else 1 })
    assertEquals(Outliers.findByWeighing(Vector.tabulate(11) { i => if i == 8 then 2 else 1 }), expected = 8) // N=11

  test("finds the index of a heaviest outlier"):
    val heaviestBottleIndex = 16
    val sample = Vector.tabulate(20) { i => if i == heaviestBottleIndex then 1.1 else 1 }
    assertEquals(Outliers.findByEncodedSum(sample, weightDelta = 0.1), expected = heaviestBottleIndex)

  test("finds the index of a lightiest outlier"):
    val heaviestBottleIndex = 3
    val sample = Vector.tabulate(20) { i => if i == heaviestBottleIndex then 0.9 else 1 }
    assertEquals(Outliers.findByEncodedSum(sample, weightDelta = 0.1), expected = heaviestBottleIndex)
