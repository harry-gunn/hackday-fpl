import java.io.{BufferedWriter, File, FileOutputStream, FileWriter}

import com.github.agourlay.json2Csv.Json2Csv
import org.json4s.DefaultFormats
import scala.io.Source.fromURL

object DataFetcher extends App {

  implicit val formats = DefaultFormats

  val stuff = fromURL("https://fantasy.premierleague.com/drf/bootstrap-static").getLines().next()
  // FileWriter
  val file = new File("src/main/resources/json")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(stuff)
  bw.close()

  val jsonFile = new File("src/main/resources/json")

  val output = new FileOutputStream("src/main/resources/result-json.csv")
  Json2Csv.convert(jsonFile, output)
}
