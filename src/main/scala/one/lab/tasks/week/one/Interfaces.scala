package one.lab.tasks.week.one

/**
  * 1. You have to define three [[one.lab.tasks.week.one.Interfaces.Console]] class implementation, let's say Xbox,
  *    PlayStation, Sega
  * 2. You also need to define implementation GameDisk traits for each of console, and some classes of games, see
  *    [[one.lab.tasks.week.one.Interfaces.XboxGameDisk]] and [[one.lab.tasks.week.one.Interfaces.ForzaHorizon]]
  * 3. When creating implementation of Console be sure to properly implement play method,
  *    so that when I try to play Xbox with PS game disk, it will print me that disk format is invalid.
  *    But when I supply appropriate disk it will print s"playing ${disk.game()}"
  */
object Interfaces extends App{

  trait GameDisk {
    val consoleType: String
    val game: String
  }

  trait Console {
    def play(disk: GameDisk): Unit
  }

  // Game Console clasess

  class Xbox extends Console {
    override def play(disk: GameDisk): Unit = {
      if(disk.consoleType.equals("Xbox")) {
        println(s"playing ${disk.game}")
      } else {
        println(s"disk format is invalid")
      }
    }
  }

  class PlayStation extends Console {
    override def play(disk: GameDisk): Unit = {
      if(disk.consoleType.equals("PlayStation")) {
        println(s"playing ${disk.game}")
      } else {
        println(s"disk format is invalid")
      }
    }
  }

  class Sega extends Console {
    override def play(disk: GameDisk): Unit = {
      if(disk.consoleType.equals("Sega")) {
        println(s"playing ${disk.game}")
      } else {
        println(s"disk format is invalid")
      }
    }
  }

  // Console game disks

  trait XboxGameDisk extends GameDisk {
    override val consoleType: String = "Xbox"
  }

  trait PlayStationGameDisk extends GameDisk {
    override val consoleType: String = "PlayStation"
  }

  trait SegaGameDisk extends GameDisk {
    override val consoleType: String = "Sega"
  }

  // Console games

  class GodOfWar extends PlayStationGameDisk {
    override val game: String = "God of War game"
  }

  class DaysGone extends PlayStationGameDisk {
    override val game: String = "Days Gone game"
  }

  class ForzaHorizon extends XboxGameDisk {
    override val game: String = "ForzaHorizon race game"
  }

  class GearsOfWar extends XboxGameDisk {
    override val game: String = "Gears of War third-person shooter game"
  }

  class Tetris extends SegaGameDisk {
    override val game: String = "Tetris falling blocks game"
  }

  //Testing xbox games
  new Xbox().play(new ForzaHorizon())
  new Xbox().play(new GodOfWar())

  //Testing PlayStation games
  new PlayStation().play(new GodOfWar())
  new PlayStation().play(new GearsOfWar())

  //Testing Sega games
  new Sega().play(new Tetris())
  new Sega().play(new DaysGone())



}
