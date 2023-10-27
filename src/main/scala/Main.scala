import com.opencsv.CSVReader
import java.io.FileReader
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Try
import scala.util.control.Breaks._
import java.io.FileWriter
import scala .collection.immutable._


//TO-DO:
// -  ask if in 3rd prompt the order matters and how does the sorting happen on the basis of 2 rows.
// - Ask whats the diff in the 2 sizes in:  dishesLiked
// - 2nd last question not done.

object Main {

  case class Restaurant(id : Int,
                        restaurantName: String,
                        rating: Double,
                        numberOfVotes:Int,
                        location:String,
                        restaurantType:List[String],
                        dishesLiked:List[String],
                        typesOfCuisines:List[String],
                        costForTwo:String
                       )

  case class DishesGroup(restaurantName: String, NoDishes:Int, noDishesAlso:Int)

  case class DistinctCuisine(location: String,
                             NoCuisine:Int)


  def main(args: Array[String]): Unit = {
    println("Hello world!")

    val csvFileName = "src/main/zomato_cleaned.csv"
    val fileReader = new FileReader(csvFileName)
    val csvReader = new CSVReader(fileReader)

    val allRows:                                                                                                                                                    List[Array[String]] = csvReader.readAll().asScala.toList
    val csvSchema = Array("id", "restaurantName", "rating", "numberOfVotes", "location", "restaurantType", "dishesLiked", "typesOfCuisines", "costForTwo")
        println("the schema" + csvSchema.mkString("Array(", ", ", ")"))
    csvReader.close()

    val dataObject = allRows.map( rows => {
      val id = rows(0).toInt
      val name = rows(1)
      val rating = rows(2) // so we change how it is displayed.
      val strippedrating = Try(rating.stripSuffix("/5").toDouble).getOrElse(0.0) //This is to put the values of ratings which are null as 0.0 so that we dont even refer to them
      val votes = rows(3).toInt
      val location = rows(4)
      val rType = rows(5).split(", ").toList
      val dishLiked = rows(6).split(", ").toList
      val typecuisine = rows(7).split(", ").toList
      val fortwo = rows(8)

      Restaurant(id,name,strippedrating,votes,location,rType,dishLiked,typecuisine,fortwo)
    })


    topNRatedRest(dataObject, 10)
    topNRatedlocation(dataObject, 3, "Banashankari", "Casual Dining")
    topNLeastRated(dataObject, 4, "Banashankari")
    dishesLiked(dataObject)
    distinctLocations(dataObject)
    distinctCuisines(dataObject, "Banashankari")
//    distinctCuisineLocation(dataObject)
    countRestaurants(dataObject)

  }


  //Top N restaurants by rating
  def topNRatedRest(restaurant: List[Restaurant], n: Int) = {
    // the sortBy(_.rating), applies on list and takes the _.  as the one you wanna sort according
    val sortedRest = restaurant.sortBy(_.rating)(Ordering[Double].reverse)
    // the take gets applied to lists.
    val topN = sortedRest.take(n)
    println(topN)
  }

  //Top N restaurants by rating in a given location and restaurant type
  def topNRatedlocation(restaurant: List[Restaurant],n:Int,selectLocation:String,selectType:String) = {
    //we need to select the rows with the location
    //and the selectType should be in the list of types
    val filterData = restaurant.filter(i => i.location == selectLocation)
    val filterData2 = filterData.filter(i => i.restaurantType.contains(selectType))
    //now sort the data
    val sortedRest = filterData2.sortBy(_.rating)(Ordering[Double].reverse)
    val topN = sortedRest.take(n)
    println(topN)
  }

  //Top N restaurants by rating and least number of votes in a given location
  def topNLeastRated(restaurant: List[Restaurant], n: Int, selectLocation: String) = {
    val filterData = restaurant.filter(i => i.location == selectLocation)
    //sort by least number of votes? or by rating?
    val sortData = filterData.sortBy(r => (r.numberOfVotes, -r.rating))
//    val sortData2 = filterData.sortBy(r => (-r.rating,r.numberOfVotes)) // primary sorting via highest ratings....
    val topN = sortData.take(n)
    println(topN)
  }

  //No. of dishes liked in every restaurant
  def dishesLiked(restaurant: List[Restaurant]) = {
    //create another list with restaurant and size of dishesLiked.
    val dishesLiked = restaurant.map(r => {
      val rName = r.restaurantName
      val noDishes = r.dishesLiked.size
      val noDishesalso = r.dishesLiked.length
      DishesGroup(rName,noDishes, noDishesalso)
    })
    println(dishesLiked.take(10))
  }

  //No. of distinct locations - answer 94
  def distinctLocations(restaurant: List[Restaurant]) = {
    val distinctLocations = restaurant.map(r => r.location).distinct
    val noDistinct = distinctLocations.length
    println(noDistinct)
  }

  //No. of distinct cuisines at a certain location
  def distinctCuisines(restaurant: List[Restaurant], selectLocation: String) = {
    val filterData = restaurant.filter(r => r.location == selectLocation)
    //now count the cuisines - North Indian, Mughali, Chinese and so on
    //access all the Lists, and start storing? in another val?
    //and then find distinct
    val mappedCuisine = restaurant.flatMap(r => r.typesOfCuisines)

    //the flatMap will have all the cuisines in a list
    val noCuisine = mappedCuisine.distinct.length
    println(noCuisine)
  }

//  No. of distinct cuisines at each location
  def distinctCuisineLocation(restaurant: List[Restaurant]) = {
    //create location to number object
    //by map we make a mapping:
    //of [ STring - > list[Restaurant] ]

    val restaurantsByLocation = restaurant.map(_.location).distinct

    val distinctCuisine = restaurantsByLocation.map(location => {
      val number = distinctCuisines(restaurant, location)
      //this finds the number of cuisines in that loc. already written function
      (location,number)
    })
    println(distinctCuisine.take(10))
  }


  //Count of restaurants for each cuisine type
  def countRestaurants(restaurant: List[Restaurant]) = {
    //for each cuisine first lets create a flatmap which is distinct
    val cuisines = restaurant.flatMap(_.typesOfCuisines).distinct
    println(cuisines.length)
    //now take this list into another object
    //which contains the count of restaurants of that cuisine

    val countcuisine = cuisines.map(cuisine => {
      //so go through each and for that cuisine add that restaurant infront of it?
      val cuisineNumber = restaurant.count(_.typesOfCuisines.contains(cuisine))
      (cuisine, cuisineNumber)
    })
//    For each cuisine, the count variable is calculated by using the count method on the restaurants list.
    //    This method takes a predicate function as an argument and counts the number of elements in the list
    //    for which the predicate is true. In this case, the predicate is _.typesOfCuisines.contains(cuisine),
    //    which checks if the list of cuisine types for a restaurant contains the current cuisine.

    println(countcuisine.take(20))
  }
}