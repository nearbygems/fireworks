package fireworks

import doodle.core.{Angle, Color, Point}

import scala.util.Random

sealed trait Firework

object Firework:

  def init(): Firework = Waiting.init()

  def next(firework: Firework): Firework = firework match {
    case waiting: Waiting => waiting
    case launched: Launched => launched
    case exploding: Exploding => exploding
    case Done => Done
  }


case class Waiting(countDown: Int, startPosition: Point, numberOfParticles: Int, particlesColor: Color) extends Firework :

  def next: Firework =
    if countDown > 0 then {
      copy(countDown = countDown - 1)
    } else {
      Launched.init(startPosition, numberOfParticles, particlesColor)
    }


object Waiting:

  def init(): Waiting =

    val color             = Settings.colors(Random.nextInt(Settings.colors.length))
    val numberOfParticles = 40

    val position = Point((Random.nextInt(Settings.width / 2) - Settings.width / 4).toDouble,
                         (-Settings.height / 2).toDouble)

    val countDown = Random.nextInt(60)
    Waiting(countDown, position, numberOfParticles, color)


case class Launched(countDown: Int, position: Point, direction: Angle, numberOfParticles: Int, particlesColor: Color) extends Firework :

  def next: Firework =
    if countDown > 0 then {
      copy(countDown = countDown - 1, position = Motion.movePoint(position, direction, Settings.propulsionSpeed))
    } else {
      Exploding.init(numberOfParticles, direction, position, particlesColor)
    }


object Launched:

  def init(position: Point, numberOfParticles: Int, particlesColor: Color): Launched =
    val direction = Angle(math.Pi / 2 + (Random.nextDouble() - 0.5) / 5)
    Launched(countDown = 30, position, direction, numberOfParticles, particlesColor)


case class Exploding(countDown: Int, particles: Particles) extends Firework :

  def next: Firework =
    if (countDown > 0) {
      copy(countDown = countDown - 1, particles = particles.next)
    } else {
      Done
    }


object Exploding:

  def init(numberOfParticles: Int, direction: Angle, position: Point, color: Color): Exploding =
    val particles = List.fill(numberOfParticles)(Particle.init(direction, position, color))
    Exploding(countDown = 30, Particles(particles))


case object Done extends Firework


case class Particle(horizontalSpeed: Double, verticalSpeed: Double, position: Point, color: Color):

  def next: Particle =

    val updatedHorizontalSpeed: Double = Motion.drag(horizontalSpeed)
    val updatedVerticalSpeed  : Double = Motion.drag(verticalSpeed - Settings.gravity)
    val updatedPosition                = Point(position.x + updatedHorizontalSpeed, position.y + updatedVerticalSpeed)
    Particle(horizontalSpeed = updatedHorizontalSpeed, verticalSpeed = updatedVerticalSpeed, updatedPosition, color)


case class Particles(value: List[Particle]):

  def next: Particles = Particles(value.map(particle => particle.next))


object Particle:

  def init(initialDirection: Angle, position: Point, color: Color): Particle =
    val angle    = initialDirection + Angle(Random.nextDouble() * (math.Pi / 4) - math.Pi / 8)
    val velocity = Random.nextDouble() * 10 + 20
    Particle(angle.cos * velocity, angle.sin * velocity, position, color)


object Motion:

  def movePoint(point: Point, direction: Angle, speed: Double): Point =
    Point(point.x + direction.cos * speed, point.y + direction.sin * speed)

  def drag(speed: Double): Double =
    if speed > Settings.friction then {
      math.max(speed - Settings.friction, 0)
    } else if speed < -Settings.friction then {
      math.min(speed + Settings.friction, 0)
    } else {
      0
    }


object Settings:

  val width                = 800
  val height               = 600
  val friction             = 0.2
  val gravity              = 1.5
  val propulsionSpeed      = 8.0
  val colors: Array[Color] = Array(Color.red, Color.yellow, Color.white, Color.blue, Color.violet)
  