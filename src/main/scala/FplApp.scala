import java.io.File

import com.github.tototoshi.csv._

import scala.annotation.tailrec

object FplApp extends App {

  val data = readCsv("src/main/resources/result-json.csv")
  val playerData = getPlayerData(data)
  val teamData = getTeamData(data)

//  getAndPrintPlayers(pointsPerNinety())
//  getAndPrintPlayers(overallDreamTeam())
//  getAndPrintPlayers(playerFromName("Carrillo"))
//  getAndPrintPlayers(mostWeeklyDreamTeam())
//  getAndPrintPlayers(pointsPerNinety())
//  getAndPrintPlayers(pointsPerMil())
//  getAndPrintPlayers(playerByForm())
//  getAndPrintPlayers(topPlayersFromTeam("Southampton"))
//  printList(getTeamsNextFixtureInfo("Southampton"))
//  printList(teamFromName("Southampton"))
//  printList(allNextFixtures)
  getAndPrintPlayers(topFormPlayersPredictedToWin)


  def topFormPlayersPredictedToWin = {
    val winningTeams = teamData.map(x => getTeamsNextFixtureInfo(x("name")).head)
          .filter(_("expected_result") == "win")
          .map(x => teamCodeFromName(x("name"))).toSet

    playerByForm(playerData.filter(x => winningTeams.contains(x("team"))))
  }

  def allNextFixtures = {
    teamData.filter(_("next_event_fixture.is_home") == "true")
      .map(x => Map(x("name") -> {
        teamData.filter(_("id") == x("next_event_fixture.opponent"))
          .map(x => x("name")).head
      }))
  }

  def getTeamsNextFixtureInfo(teamName: String) = {
    val neededKeys = Set( "name",
                          "next_fixture_date",
                          "next_event_fixture.opponent",
                          "next_event_fixture.is_home",
                          "strength_overall_home",
                          "strength_overall_away")
    teamFromName(teamName)
      .map(x => x ++ createDate(x))
      .map(y => y.filterKeys(neededKeys))
      .map(z => buildFixtureInfo(z))
  }

  def buildFixtureInfo(rawData: Map[String, String]): Map[String, String] = {
    val rawOpponent = teamData.filter(_("id") == rawData("next_event_fixture.opponent"))
    val opponentName = rawOpponent.map(x => x("name")).head
    val isHome = rawData("next_event_fixture.is_home").toBoolean
    val homeOrAway = if(isHome) "home" else "away"
    val expectedResult = {
      val teamStrength = rawData({ if(isHome) "strength_overall_home" else "strength_overall_away" }).toInt
      val opponentStrength = rawOpponent.head({
        if(!isHome) "strength_overall_home"
        else "strength_overall_away"
      }).toInt

      teamStrength - opponentStrength match {
        case x if x < -20 => "loss"
        case z if z > 20 => "win"
        case _ => "draw"
      }
    }

    Map("name" -> rawData("name"),
      "opponent" -> opponentName,
      "next_fixture_date" -> rawData("next_fixture_date"),
      "location" -> homeOrAway,
      "expected_result" -> expectedResult)
  }

  def teamFromName(teamName: String) = {
    teamData.filter(_("name") == teamName)
  }

  def teamCodeFromName(teamName: String): String = {
    teamData.filter(_("name") == teamName)
      .map(_("id")).head
  }

  def playerFromName(playerName: String) = {
    playerData.filter(_("web_name") == playerName)
  }

  def playerByForm(playerData: List[Map[String, String]] = playerData) = {
    playerData
      .map(x => (x("web_name"), x("form")))
      .sortBy(_._2).reverse
  }

  def topPlayersFromTeam(team: String) = {
    playerData.filter(_("team") == teamCodeFromName(team))
      .map(x => (x("web_name"), x("total_points").toInt))
      .sortBy(_._2).reverse
  }

  def overallDreamTeam() = {
    playerData.filter(_("in_dreamteam") == "true")
      .map(x => (x("web_name"), x("total_points")))
  }

  def mostWeeklyDreamTeam() = {
    playerData
      .map(x => (x("web_name"), x("dreamteam_count")))
      .sortBy(_._2).reverse
  }

  def pointsPerMil() = {
    val neededKeys = Set("web_name", "total_points", "now_cost")
    playerData.map(x => x.filterKeys(neededKeys))
      .map(x => (x("web_name"), 10 * x("total_points").toDouble / x("now_cost").toDouble))
      .sortBy(_._2).reverse
      .map(x => f"${x._1}, ${x._2}%1.2f")
  }

  def pointsPerNinety() = {
    val neededKeys = Set("web_name", "total_points", "minutes")
    playerData.map(x => x.filterKeys(neededKeys))
      .filter(_("minutes").toInt > 300)
      .filter(_("total_points").toInt > 0)
      .map(x => (x("web_name"), 90 * x("total_points").toDouble / x("minutes").toDouble))
      .sortBy(_._2).reverse
      .map(x => f"${x._1}, ${x._2}%1.2f")
  }

  def playersOverTenMil() = {
    playerData.flatMap(x => if(x("now_cost").toInt > 100) List(x) else None)
      .filter(_ != None)
      .map(x => x("web_name"))
  }

  def createDate(teamData: Map[String, String]): Map[String, String] = {
    val day = teamData("next_event_fixture.day")
    val month = teamData("next_event_fixture.month")
    Map("next_fixture_date" -> s"$day/$month")
  }

  def printList(thing: Any): Unit = {
    thing match {
      case x: List[_] => x.foreach(printList)
      case x: Map[String, String] => x.foreach(printList)
      case x: (String, String) => println(s"${x._1}, ${x._2}")
      case _ => println(thing)
    }
  }

  def getAndPrintPlayers(f: List[_]) = {
    printList(f.take(11))
  }

  def getPlayerData(data: List[Map[String, String]]) = data.map(x => x.filterKeys(_.startsWith("elements"))).map(x => x.map(y => (y._1.split("elements.")(1), y._2)))

  def getTeamData(data: List[Map[String, String]]) = data.map(x => x.filterKeys(_.startsWith("teams"))).map(x => x.map(y => (y._1.split("teams.")(1), y._2))).take(20)

  def readCsv(filename: String) = {
    val reader = CSVReader.open(new File(filename))
    reader.allWithHeaders()
  }
}
