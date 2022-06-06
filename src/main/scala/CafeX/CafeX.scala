package CafeX

import Currency.{EUR, GBP, USD}
import FoodOrDrink.Food
import FoodOrDrink.Drink
import HotOrCold.Hot
import HotOrCold.Cold

import java.time.LocalTime

object CafeX extends App {

  val cola = MenuItem("Cola", Drink, Cold, 0.75, false)
  val coffee = MenuItem("Coffee", Drink, Hot, 1.00, false)
  val cheeseSandwich = MenuItem("Cheese Sandwich", Food, Hot, 2.50, false)
  val steakSandwich = MenuItem("Steak Sandwich", Food, Hot, 3.50, false)
  val lobster = MenuItem("Lobster", Food, Hot, 25.00, true)

  val yonis = Customer("Yonis", 5, 1301)
  val connie = Customer("Connie", 5, 1401)
  val cristian = Customer("Cristian", 4, 1402)
  val jake = Customer("Jake", 4, 1403)
  val sarina = Customer("Sarina", 3, 1404)
  val robyn = Customer("Robyn", 3, 1405)

  def currencyExchange(currency: Currency) = {
    currency match {
      case GBP => 1
      case EUR => 1.18
      case USD => 1.25
      case _ => 0
    }
  }

  def currencySymbol(currency: Currency) = {
    currency match {
      case GBP => "£"
      case EUR => "€"
      case USD => "$"
      case _ => "We don't accept that currency"
    }
  }


  def sumCustomerOrder(items: List[MenuItem]) = items.map(x => x.cost).sum

  def loyaltyDiscount(customer: Customer) = {
    if (customer.loyaltyStars < 1) 0
    else if (customer.loyaltyStars < 3) customer.loyaltyStars * 0.125
    else 0.15
  }

  def checkHappyHour(): Boolean = {
    val time = LocalTime.now().getHour
    (time > 12 && time < 14)
  }

  def getDrink(items: List[MenuItem]): List[MenuItem] = items.filter(x => (x.foodOrDrink == Drink))
  def getHotFood(items: List[MenuItem]): List[MenuItem]  = items.filter(x => (x.hotOrCold == Hot) && (x.foodOrDrink == Food))
  def getColdFood(items: List[MenuItem]): List[MenuItem]  = items.filter(x => (x.hotOrCold == Cold) && (x.foodOrDrink == Food))
  def getPremium(items: List[MenuItem]): List[MenuItem]  = items.filter(x => (x.isPremium == true))

  def serviceChargeCalculator(items: List[MenuItem]): = {
    if (getPremium(items).nonEmpty) 0.40
    else if (getDrink(items).nonEmpty) 0.15
    else if (getHotFood(items).nonEmpty) 0.25
    else if (getColdFood(items).nonEmpty) 0.20
    else 0
  }

  def billReceipt(items: List[MenuItem], customer: Customer, currency: Currency): Unit ={
    val price = sumCustomerOrder(items) * currencyExchange(currency) // sum of entire order

    val loyaltyDiscount = {
      val nonDiscountPrice = sumCustomerOrder(getPremium(items)) * currencyExchange(currency)
      (price - nonDiscountPrice) * loyaltyDiscount(customer)
    }

    val happyHourDiscount = {
      val drinksCost = sumCustomerOrder(getDrink(items) * currencyExchange(currency))
      (price - drinksCost) * happyHourDiscount
    }

    // STATEMENTS TO THE BILL
    println(s"Thank you for ordering at Cafe X, ${customer.name}")
    println("Please see your order summary")
    // Order
    // Loyalty Discount
    // Happy Hour Discount - if applicable
    // Service Charge
    // Total
  }
}
