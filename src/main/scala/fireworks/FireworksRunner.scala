package fireworks

import cats.instances.list.*
import cats.instances.unit.*
import doodle.core.Color
import doodle.interact.syntax.*
import doodle.java2d.*
import doodle.syntax.*
import monix.reactive.Observable

@main
def run(): Unit =

  val initFireworks = List.fill(10)(Firework.init())

  val fireworksOverTime =
    Observable.unfold(initFireworks) { fireworks =>
      if fireworks.isEmpty then {
        None
      } else
        val updatedFireworks =
          fireworks.flatMap { firework =>
            Firework.next(firework) match
              case Done => None
              case other => Some(other)
          }
        Some((fireworks, updatedFireworks))
    }

  def launchedPicture(launched: Launched): Picture[Unit] =
    shape
      .circle[Algebra, Drawing](diameter = 5)
      .fillColor(Color.lightGoldenrodYellow)
      .at(launched.position)

  def explodingPicture(exploding: Exploding): Picture[Unit] =
    exploding.particles.value.map { particle =>
      shape
        .circle[Algebra, Drawing](diameter = 5)
        .fillColor(particle.color)
        .at(particle.position)
    }.allOn

  val pictures = fireworksOverTime.map { fireworks =>
    fireworks.map { firework =>
      firework match
        case _: Waiting => shape.empty[Algebra, Drawing]
        case Done => shape.empty[Algebra, Drawing]
        case launched: Launched => launchedPicture(launched)
        case exploding: Exploding => explodingPicture(exploding)
    }.allOn
  }

  val frame = Frame.size(Settings.width.toDouble, Settings.height.toDouble).background(Color.black)

  pictures.animate(frame)
