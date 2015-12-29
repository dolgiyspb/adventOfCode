
import scala.util.parsing.combinator._




case class Point(x: Int, y: Int) {
}
abstract class BaseLight {
  def turnOn(): Unit
  def turnOff(): Unit
  def toggle(): Unit
  def symbol : String
  def state: Int
}
case class Light(_state: Int = 0) extends BaseLight{
  var state = _state
  def turnOn() = {
    state = 1
  }

  def turnOff() = {
    state = 0
  }

  def toggle() = {
    state = if(state == 0) 1 else 0
  }

  def symbol = {
    if(state == 1) "+"
    else "-"
  }
}

case class LightWithBrightness(_state: Int = 0) extends BaseLight{
  var state = _state
  override def turnOn() = {
    state = state + 1
  }

  override def turnOff() = {
    state = state - 1
    if(state < 0) state = 0
  }

  override def toggle() = {
    state = state + 2
  }

  def symbol = {
    if(state == 0) "-"
    else if (state == 1) "+"
    else "*"
  }
}

class Field[T<:BaseLight](createT: () => T) {
  val field: IndexedSeq[T] = for(x <- 0 until 1000; y <- 0 until 1000) yield createT()

  def processLight(command: Command) = {
    for(x <- command.leftCorner.x until (command.rightCorner.x + 1))
        for(y <- command.leftCorner.y until (command.rightCorner.y + 1))
          yield command.command(field(x*1000 + y))
  }

  override def toString(): String = {
    val builder = new StringBuilder
    for (x <- 0 until 1000) {
      for (y <- 0 until 1000) {
        val light = field(x * 1000 + y)
        builder ++= light.symbol
      }
      builder ++= "\n"
    }
    builder.toString()
  }

  def lightsOnState(state: Int) = {
    field.foldLeft(0)((acc, light) => if(light.state == state) acc + 1 else acc)
  }

  def stateSum() = {
    field.foldLeft(0)((acc, light) => acc + light.state)
  }
}

case class Command(command: BaseLight => Unit, leftCorner: Point, rightCorner: Point) {
}


object LightCommandsParser extends JavaTokenParsers {
  def turnOn : Parser[BaseLight => Unit] = "turn on" ^^ (_ => (l: BaseLight) => l.turnOn)
  def turnOff : Parser[BaseLight => Unit] = "turn off" ^^ (_ => (l: BaseLight) => l.turnOff)
  def toggle : Parser[BaseLight => Unit] = "toggle" ^^ (_ => (l: BaseLight) => l.toggle)
  def command: Parser[BaseLight => Unit] = turnOn | turnOff | toggle
  def coordinate: Parser[Point] = decimalNumber ~ "," ~ decimalNumber ^^ {case first ~ "," ~ second => Point(first.toInt, second.toInt) }
  def delimiterThrough: Parser[String] = "through"
  def fullCommand: Parser[Command] = command ~ coordinate ~ delimiterThrough ~ coordinate ^^ {
    case parsedCommand ~ coordinate1 ~ delimiter ~ coordinate2 => new Command(parsedCommand, coordinate1, coordinate2)
  }

  def process[T <: BaseLight](command: String, field: Field[T]) = {
    val parsed = parseAll(fullCommand, command).get
    field.processLight(parsed)
  }
}



object Run {
  def main(args: Array[String]) {
    val fieldWithSimpleLights = new Field[Light](() => new Light())
    val fieldWithBrightnessLights = new Field[LightWithBrightness](() => new LightWithBrightness())
    val commands = "turn on 887,9 through 959,629\nturn on 454,398 through 844,448\nturn off 539,243 through 559,965\nturn off 370,819 through 676,868\nturn off 145,40 through 370,997\nturn off 301,3 through 808,453\nturn on 351,678 through 951,908\ntoggle 720,196 through 897,994\ntoggle 831,394 through 904,860\ntoggle 753,664 through 970,926\nturn off 150,300 through 213,740\nturn on 141,242 through 932,871\ntoggle 294,259 through 474,326\ntoggle 678,333 through 752,957\ntoggle 393,804 through 510,976\nturn off 6,964 through 411,976\nturn off 33,572 through 978,590\nturn on 579,693 through 650,978\nturn on 150,20 through 652,719\nturn off 782,143 through 808,802\nturn off 240,377 through 761,468\nturn off 899,828 through 958,967\nturn on 613,565 through 952,659\nturn on 295,36 through 964,978\ntoggle 846,296 through 969,528\nturn off 211,254 through 529,491\nturn off 231,594 through 406,794\nturn off 169,791 through 758,942\nturn on 955,440 through 980,477\ntoggle 944,498 through 995,928\nturn on 519,391 through 605,718\ntoggle 521,303 through 617,366\nturn off 524,349 through 694,791\ntoggle 391,87 through 499,792\ntoggle 562,527 through 668,935\nturn off 68,358 through 857,453\ntoggle 815,811 through 889,828\nturn off 666,61 through 768,87\nturn on 27,501 through 921,952\nturn on 953,102 through 983,471\nturn on 277,552 through 451,723\nturn off 64,253 through 655,960\nturn on 47,485 through 734,977\nturn off 59,119 through 699,734\ntoggle 407,898 through 493,955\ntoggle 912,966 through 949,991\nturn on 479,990 through 895,990\ntoggle 390,589 through 869,766\ntoggle 593,903 through 926,943\ntoggle 358,439 through 870,528\nturn off 649,410 through 652,875\nturn on 629,834 through 712,895\ntoggle 254,555 through 770,901\ntoggle 641,832 through 947,850\nturn on 268,448 through 743,777\nturn off 512,123 through 625,874\nturn off 498,262 through 930,811\nturn off 835,158 through 886,242\ntoggle 546,310 through 607,773\nturn on 501,505 through 896,909\nturn off 666,796 through 817,924\ntoggle 987,789 through 993,809\ntoggle 745,8 through 860,693\ntoggle 181,983 through 731,988\nturn on 826,174 through 924,883\nturn on 239,228 through 843,993\nturn on 205,613 through 891,667\ntoggle 867,873 through 984,896\nturn on 628,251 through 677,681\ntoggle 276,956 through 631,964\nturn on 78,358 through 974,713\nturn on 521,360 through 773,597\nturn off 963,52 through 979,502\nturn on 117,151 through 934,622\ntoggle 237,91 through 528,164\nturn on 944,269 through 975,453\ntoggle 979,460 through 988,964\nturn off 440,254 through 681,507\ntoggle 347,100 through 896,785\nturn off 329,592 through 369,985\nturn on 931,960 through 979,985\ntoggle 703,3 through 776,36\ntoggle 798,120 through 908,550\nturn off 186,605 through 914,709\nturn off 921,725 through 979,956\ntoggle 167,34 through 735,249\nturn on 726,781 through 987,936\ntoggle 720,336 through 847,756\nturn on 171,630 through 656,769\nturn off 417,276 through 751,500\ntoggle 559,485 through 584,534\nturn on 568,629 through 690,873\ntoggle 248,712 through 277,988\ntoggle 345,594 through 812,723\nturn off 800,108 through 834,618\nturn off 967,439 through 986,869\nturn on 842,209 through 955,529\nturn on 132,653 through 357,696\nturn on 817,38 through 973,662\nturn off 569,816 through 721,861\nturn on 568,429 through 945,724\nturn on 77,458 through 844,685\nturn off 138,78 through 498,851\nturn on 136,21 through 252,986\nturn off 2,460 through 863,472\nturn on 172,81 through 839,332\nturn on 123,216 through 703,384\nturn off 879,644 through 944,887\ntoggle 227,491 through 504,793\ntoggle 580,418 through 741,479\ntoggle 65,276 through 414,299\ntoggle 482,486 through 838,931\nturn off 557,768 through 950,927\nturn off 615,617 through 955,864\nturn on 859,886 through 923,919\nturn on 391,330 through 499,971\ntoggle 521,835 through 613,847\nturn on 822,787 through 989,847\nturn on 192,142 through 357,846\nturn off 564,945 through 985,945\nturn off 479,361 through 703,799\ntoggle 56,481 through 489,978\nturn off 632,991 through 774,998\ntoggle 723,526 through 945,792\nturn on 344,149 through 441,640\ntoggle 568,927 through 624,952\nturn on 621,784 through 970,788\ntoggle 665,783 through 795,981\ntoggle 386,610 through 817,730\ntoggle 440,399 through 734,417\ntoggle 939,201 through 978,803\nturn off 395,883 through 554,929\nturn on 340,309 through 637,561\nturn off 875,147 through 946,481\nturn off 945,837 through 957,922\nturn off 429,982 through 691,991\ntoggle 227,137 through 439,822\ntoggle 4,848 through 7,932\nturn off 545,146 through 756,943\nturn on 763,863 through 937,994\nturn on 232,94 through 404,502\nturn off 742,254 through 930,512\nturn on 91,931 through 101,942\ntoggle 585,106 through 651,425\nturn on 506,700 through 567,960\nturn off 548,44 through 718,352\nturn off 194,827 through 673,859\nturn off 6,645 through 509,764\nturn off 13,230 through 821,361\nturn on 734,629 through 919,631\ntoggle 788,552 through 957,972\ntoggle 244,747 through 849,773\nturn off 162,553 through 276,887\nturn off 569,577 through 587,604\nturn off 799,482 through 854,956\nturn on 744,535 through 909,802\ntoggle 330,641 through 396,986\nturn off 927,458 through 966,564\ntoggle 984,486 through 986,913\ntoggle 519,682 through 632,708\nturn on 984,977 through 989,986\ntoggle 766,423 through 934,495\nturn on 17,509 through 947,718\nturn on 413,783 through 631,903\nturn on 482,370 through 493,688\nturn on 433,859 through 628,938\nturn off 769,549 through 945,810\nturn on 178,853 through 539,941\nturn off 203,251 through 692,433\nturn off 525,638 through 955,794\nturn on 169,70 through 764,939\ntoggle 59,352 through 896,404\ntoggle 143,245 through 707,320\nturn off 103,35 through 160,949\ntoggle 496,24 through 669,507\nturn off 581,847 through 847,903\nturn on 689,153 through 733,562\nturn on 821,487 through 839,699\nturn on 837,627 through 978,723\ntoggle 96,748 through 973,753\ntoggle 99,818 through 609,995\nturn on 731,193 through 756,509\nturn off 622,55 through 813,365\nturn on 456,490 through 576,548\nturn on 48,421 through 163,674\nturn off 853,861 through 924,964\nturn off 59,963 through 556,987\nturn on 458,710 through 688,847\ntoggle 12,484 through 878,562\nturn off 241,964 through 799,983\nturn off 434,299 through 845,772\ntoggle 896,725 through 956,847\nturn on 740,289 through 784,345\nturn off 395,840 through 822,845\nturn on 955,224 through 996,953\nturn off 710,186 through 957,722\nturn off 485,949 through 869,985\nturn on 848,209 through 975,376\ntoggle 221,241 through 906,384\nturn on 588,49 through 927,496\nturn on 273,332 through 735,725\nturn on 505,962 through 895,962\ntoggle 820,112 through 923,143\nturn on 919,792 through 978,982\ntoggle 489,461 through 910,737\nturn off 202,642 through 638,940\nturn off 708,953 through 970,960\ntoggle 437,291 through 546,381\nturn on 409,358 through 837,479\nturn off 756,279 through 870,943\nturn off 154,657 through 375,703\nturn off 524,622 through 995,779\ntoggle 514,221 through 651,850\ntoggle 808,464 through 886,646\ntoggle 483,537 through 739,840\ntoggle 654,769 through 831,825\nturn off 326,37 through 631,69\nturn off 590,570 through 926,656\nturn off 881,913 through 911,998\nturn on 996,102 through 998,616\nturn off 677,503 through 828,563\nturn on 860,251 through 877,441\nturn off 964,100 through 982,377\ntoggle 888,403 through 961,597\nturn off 632,240 through 938,968\ntoggle 731,176 through 932,413\nturn on 5,498 through 203,835\nturn on 819,352 through 929,855\ntoggle 393,813 through 832,816\ntoggle 725,689 through 967,888\nturn on 968,950 through 969,983\nturn off 152,628 through 582,896\nturn off 165,844 through 459,935\nturn off 882,741 through 974,786\nturn off 283,179 through 731,899\ntoggle 197,366 through 682,445\nturn on 106,309 through 120,813\ntoggle 950,387 through 967,782\nturn off 274,603 through 383,759\nturn off 155,665 through 284,787\ntoggle 551,871 through 860,962\nturn off 30,826 through 598,892\ntoggle 76,552 through 977,888\nturn on 938,180 through 994,997\ntoggle 62,381 through 993,656\ntoggle 625,861 through 921,941\nturn on 685,311 through 872,521\nturn on 124,934 through 530,962\nturn on 606,379 through 961,867\nturn off 792,735 through 946,783\nturn on 417,480 through 860,598\ntoggle 178,91 through 481,887\nturn off 23,935 through 833,962\ntoggle 317,14 through 793,425\nturn on 986,89 through 999,613\nturn off 359,201 through 560,554\nturn off 729,494 through 942,626\nturn on 204,143 through 876,610\ntoggle 474,97 through 636,542\nturn off 902,924 through 976,973\nturn off 389,442 through 824,638\nturn off 622,863 through 798,863\nturn on 840,622 through 978,920\ntoggle 567,374 through 925,439\nturn off 643,319 through 935,662\ntoggle 185,42 through 294,810\nturn on 47,124 through 598,880\ntoggle 828,303 through 979,770\nturn off 174,272 through 280,311\nturn off 540,50 through 880,212\nturn on 141,994 through 221,998\nturn on 476,695 through 483,901\nturn on 960,216 through 972,502\ntoggle 752,335 through 957,733\nturn off 419,713 through 537,998\ntoggle 772,846 through 994,888\nturn on 881,159 through 902,312\nturn off 537,651 through 641,816\ntoggle 561,947 through 638,965\nturn on 368,458 through 437,612\nturn on 290,149 through 705,919\nturn on 711,918 through 974,945\ntoggle 916,242 through 926,786\ntoggle 522,272 through 773,314\nturn on 432,897 through 440,954\nturn off 132,169 through 775,380\ntoggle 52,205 through 693,747\ntoggle 926,309 through 976,669\nturn off 838,342 through 938,444\nturn on 144,431 through 260,951\ntoggle 780,318 through 975,495\nturn off 185,412 through 796,541\nturn on 879,548 through 892,860\nturn on 294,132 through 460,338\nturn on 823,500 through 899,529\nturn off 225,603 through 483,920\ntoggle 717,493 through 930,875\ntoggle 534,948 through 599,968\nturn on 522,730 through 968,950\nturn off 102,229 through 674,529"
    for(cmd <- commands.split("\n"))
      {
        LightCommandsParser.process(cmd, fieldWithSimpleLights)
        LightCommandsParser.process(cmd, fieldWithBrightnessLights)
      }


    println(fieldWithBrightnessLights.stateSum)
    assert(fieldWithSimpleLights.lightsOnState(1) == 377891)

  }
}




