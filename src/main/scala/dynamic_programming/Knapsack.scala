package dynamic_programming

import scala.collection.mutable

object Knapsack:
  case class Item(name: String, weight: Int, value: Int)

  def greedyKnapsack(toConsider: List[Item], availability: Int): List[Item] =
    def gknapsack0(toConsider: List[Item], selected: List[Item], availability: Int): List[Item] =
      toConsider match
        case Nil                                 => selected
        case i :: Nil if i.weight > availability => selected
        case i :: is if i.weight <= availability => gknapsack0(is, i :: selected, availability - i.weight)
        case _ :: is                             => gknapsack0(is, selected, availability)

    gknapsack0(toConsider, List.empty, availability)

  def bruteKnapsack(toConsider: List[Item], availability: Int): List[Item] =
    def bruteKnapsack0(toConsider: List[Item], availability: Int): (List[Item], Int) =
      toConsider match
        case Nil                                      => (Nil, 0)
        case item :: is if item.weight > availability => bruteKnapsack0(is, availability)
        case item :: is                               =>
          // for each item, explores two branchs: include or exclude the item (see decision tree).
          val (withoutItem, withoutItemT) = bruteKnapsack0(is, availability)
          val (withItem, withItemT) = bruteKnapsack0(is, availability - item.weight) match
            case (selected, total) => (item :: selected, total + item.value)
          if withItemT > withoutItemT
          then withItem -> withItemT
          else withoutItem -> withoutItemT

    bruteKnapsack0(toConsider, availability)._1

  def knapsack(toConsider: List[Item], availability: Int): List[Item] =
    type ToConsiderSize = Int
    type Capacity = Int
    type Selected = List[Item]
    type TotalValue = Int

    def knapsack0(toConsider: List[Item],
                  availability: Capacity,
                  memo: mutable.Map[(ToConsiderSize, Capacity), (Selected, TotalValue)]
    ): (Selected, TotalValue) =
      if memo.contains(toConsider.size -> availability) then memo(toConsider.size -> availability)
      else
        val r = toConsider match
          case Nil                                      => Nil -> 0
          case item :: is if item.weight > availability => knapsack0(is, availability, memo)
          case item :: is =>
            val (withoutItem, withoutItemT) = knapsack0(is, availability, memo)
            val (withItem, withItemT) = knapsack0(is, availability - item.weight, memo) match
              case (selected, total) => (item :: selected, total + item.value)
            if withItemT > withoutItemT
            then withItem -> withItemT
            else withoutItem -> withoutItemT
        val _ = memo.put(key = toConsider.size -> availability, value = r)
        r

    knapsack0(toConsider, availability, mutable.Map.empty)._1

object PlayKnapsack:
  import Knapsack.*
  import ItemGenerators.*

  def main(args: Array[String]): Unit =
    val toConsider = myPersonalItems() ++ otherStuff()

    // val r = greedyKnapsack(toConsider, 23_000)
    // val r = bruteKnapsack(toConsider, 160_000) // total of 264550 with 140kg
    val r = knapsack(toConsider, 160_000)
    println(s"I'm taking a total value of ${r.map(_.value).sum}; Items taken are: ${r.sortBy(i => 1 / i.value).map(_.name)}")

object ItemGenerators:
  import Knapsack.*

  def myPersonalItems(): List[Item] =
    List(
      new Item("e-Bike", weight = 31_000, value = 500_00),
      new Item("bike", weight = 24_000, value = 100_00),
      new Item("Binocular", weight = 10_000, value = 10_00),
      new Item("Math Books", weight = 12_500, value = 300_00),
      new Item("Literature Books", weight = 17_000, value = 95_50),
      new Item("Comic Books", weight = 20_000, value = 255_50),
      new Item("Sandman", weight = 20_000, value = 300_00),
      new Item("Film DVDs", weight = 7_000, value = 40_00),
      new Item("Series DVDs", weight = 15_000, 200_00),
      new Item("Clothes", weight = 7_000, value = 70_00),
      new Item("Sound System", weight = 15_000, value = 150_00),
      new Item("Tools", weight = 13_000, value = 300_00),
      new Item("Beer Glasses", weight = 1_000, 50_00),
      new Item("Kitchenware", weight = 5_000, value = 50_00),
      new Item("Coffee Grinder", weight = 5_000, value = 100_00),
      new Item("Coffeeware", weight = 2_000, value = 70_00),
      new Item("Eufy", weight = 6_000, value = 200_00),
      new Item("Massage gun", weight = 6_000, value = 180_00),
      new Item("Massage Boot", weight = 4_000, value = 190_00)
    )

  def otherStuff(): List[Item] =
    List(
      Item("Heavy Rock", weight = 25_000, value = 50),
      Item("Old Anvil", weight = 50_000, value = 10_00),
      Item("Broken TV", weight = 22_000, value = 5_00),
      Item("Concrete Block", weight = 30_000, value = 1_00),
      Item("Rusty Tools", weight = 21_000, value = 8_00),
      Item("Damaged Furniture", weight = 45_000, value = 15_00),
      Item("Worn-out Tires", weight = 28_000, value = 12_00),
      Item("Scrap Metal", weight = 35_000, value = 20_00),
      Item("Bricks", weight = 40_000, value = 1_00),
      Item("Water Tank", weight = 60_000, value = 25_00),
      Item("Exercise Weights", weight = 32_000, value = 30_00),
      Item("Old Generator", weight = 55_000, value = 35_00),
      Item("Industrial Fan", weight = 26_000, value = 18_00),
      Item("Steel Beam", weight = 75_000, value = 40_00),
      Item("Car Battery", weight = 20_000, value = 22_00),
      Item("Engine Block", weight = 90_000, value = 45_00),
      Item("Marble Slab", weight = 50_000, value = 30_00),
      Item("Lead Pipes", weight = 38_000, value = 15_00),
      Item("Cast Iron Pan", weight = 25_000, value = 10_00),
      Item("Antique Safe", weight = 120_000, value = 50_00),
      Item("Tool Chest", weight = 42_000, value = 35_00),
      Item("Metal Shelving", weight = 48_000, value = 20_00),
      Item("Washing Machine", weight = 65_000, value = 40_00),
      Item("Dryer", weight = 60_000, value = 38_00),
      Item("Old Refrigerator", weight = 85_000, value = 45_00),
      Item("Water Heater", weight = 55_000, value = 30_00),
      Item("Steel Door", weight = 70_000, value = 42_00),
      Item("Concrete Statue", weight = 95_000, value = 48_00),
      Item("Iron Gate", weight = 80_000, value = 35_00),
      Item("Farm Plow", weight = 110_000, value = 50_00),
      Item("Industrial Scale", weight = 45_000, value = 25_00),
      Item("Metal Workbench", weight = 75_000, value = 40_00),
      Item("Railroad Tie", weight = 100_000, value = 30_00),
      Item("Engine Hoist", weight = 85_000, value = 45_00),
      Item("Hydraulic Jack", weight = 30_000, value = 35_00),
      Item("Metal Lathe", weight = 150_000, value = 50_00),
      Item("Vintage Typewriter", weight = 22_000, value = 28_00),
      Item("Old Radio", weight = 25_000, value = 15_00),
      Item("Metal Filing Cabinet", weight = 50_000, value = 20_00),
      Item("Steel Drum", weight = 35_000, value = 18_00),
      Item("Anvil", weight = 55_000, value = 40_00),
      Item("Blacksmith Tools", weight = 42_000, value = 38_00),
      Item("Metal Sculpture", weight = 65_000, value = 45_00),
      Item("Industrial Press", weight = 20_000, value = 50_00),
      Item("Steel Safe", weight = 180_000, value = 50_00),
      Item("Metal Bed Frame", weight = 48_000, value = 25_00),
      Item("Weight Bench", weight = 52_000, value = 30_00),
      Item("Treadmill", weight = 95_000, value = 45_00),
      Item("Elliptical Machine", weight = 90_000, value = 42_00),
      Item("Rowing Machine", weight = 65_000, value = 38_00),
      Item("Smith Machine", weight = 150_000, value = 50_00),
      Item("Dumbbell Set", weight = 70_000, value = 40_00),
      Item("Kettlebell Collection", weight = 58_000, value = 35_00),
      Item("Barbell Set", weight = 85_000, value = 45_00),
      Item("Weight Plates", weight = 120_000, value = 50_00),
      Item("Power Rack", weight = 160_000, value = 50_00),
      Item("Metal Lockers", weight = 75_000, value = 30_00),
      Item("Steel Bookshelf", weight = 62_000, value = 28_00),
      Item("Metal Cabinet", weight = 55_000, value = 25_00),
      Item("Industrial Table", weight = 80_000, value = 35_00),
      Item("Steel Chair", weight = 35_000, value = 20_00),
      Item("Metal Stool", weight = 28_000, value = 15_00),
      Item("Welding Equipment", weight = 65_000, value = 40_00),
      Item("Metal Cutting Saw", weight = 45_000, value = 38_00),
      Item("Drill Press", weight = 75_000, value = 42_00),
      Item("Bench Grinder", weight = 32_000, value = 30_00),
      Item("Air Compressor", weight = 85_000, value = 45_00),
      Item("Metal Bender", weight = 95_000, value = 48_00),
      Item("Pipe Threader", weight = 55_000, value = 35_00),
      Item("Hydraulic Press", weight = 180_000, value = 50_00),
      Item("Metal Brake", weight = 120_000, value = 45_00),
      Item("Shearing Machine", weight = 150_000, value = 50_00),
      Item("Steel Vise", weight = 42_000, value = 30_00),
      Item("Metal Grinder", weight = 48_000, value = 32_00),
      Item("Industrial Sander", weight = 65_000, value = 38_00),
      Item("Steel Welding Table", weight = 95_000, value = 45_00),
      Item("Metal Work Cart", weight = 58_000, value = 35_00),
      Item("Tool Cabinet", weight = 75_000, value = 40_00),
      Item("Steel Ladder", weight = 42_000, value = 25_00),
      Item("Metal Scaffolding", weight = 120_000, value = 45_00),
      Item("Iron Fence", weight = 85_000, value = 38_00),
      Item("Steel Gate", weight = 65_000, value = 35_00),
      Item("Metal Railing", weight = 55_000, value = 30_00),
      Item("Iron Balcony", weight = 95_000, value = 42_00),
      Item("Steel Fire Escape", weight = 150_000, value = 50_00),
      Item("Metal Roofing", weight = 75_000, value = 35_00),
      Item("Iron Gutter", weight = 45_000, value = 25_00),
      Item("Steel Drainage", weight = 58_000, value = 30_00),
      Item("Metal Siding", weight = 65_000, value = 32_00),
      Item("Industrial Duct", weight = 55_000, value = 28_00),
      Item("Steel Beam", weight = 95_000, value = 40_00),
      Item("Metal Truss", weight = 85_000, value = 38_00),
      Item("Iron Support", weight = 75_000, value = 35_00),
      Item("Steel Column", weight = 120_000, value = 45_00),
      Item("Metal Girder", weight = 150_000, value = 50_00),
      Item("Iron Crossbeam", weight = 95_000, value = 42_00),
      Item("Steel Framework", weight = 180_000, value = 50_00),
      Item("Metal Structure", weight = 20_000, value = 50_00)
    )

//I'm taking a total value of 264550; Items taken are: List(e-Bike, Math Books, Comic Books, Sandman, Series DVDs, Tools, Kitchenware, Coffee Grinder, Coffeeware, Eufy, Massage gun, Massage Boot)
//I'm taking a total value of 264550; Items taken are: List(e-Bike, Math Books, Comic Books, Sandman, Series DVDs, Tools, Kitchenware, Coffee Grinder, Coffeeware, Eufy, Massage gun, Massage Boot)
