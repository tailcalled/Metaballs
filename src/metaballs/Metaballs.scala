package metaballs

import gamefx._
import scala.annotation.tailrec

object Metaballs extends Game {
  
  val width = 6.0
  val height = 4.0
  val r = new java.util.Random
  val balls = Array.fill(8)(new Metaball)
  
  def step() = {
    balls.foreach(_.step(dt))
  }
  def draw() = {
    val res = 7.0
    val ww = (width * res).toInt
    val hh = (height * res).toInt
    val arr = Array.tabulate(ww, hh)((xi, yi) => {
        val x = xi / res
        val y = yi / res
        val d = balls.view.map(_.density(Pt(x, y))).sum
        d
    })
    @tailrec def at(xi: Int, yi: Int): Double =
      if (xi < 0) at(xi + ww, yi)
      else if (yi < 0) at(xi, yi + hh)
      else if (xi >= ww) at(xi - ww, yi)
      else if (yi >= hh) at(xi, yi - hh)
      else arr(xi)(yi)
    def layer(threshold: Double, color: Color) = {
      val buff = buffer.transform(Scale(1/res, 1/res))
      for (x <- 0 until ww) {
        for (y <- 0 until hh) {
          val b = buff.transform(Translate(x, y))
          val c00 = at(x, y) > threshold
          val c01 = at(x, y + 1) > threshold
          val c10 = at(x + 1, y) > threshold
          val c11 = at(x + 1, y + 1) > threshold
          var poly = Vector[Pt]()
          def int(a: Double, b: Double) = {
            val alpha = b - a
            (threshold - a) / alpha
          }
          if (c00) poly :+= Pt(0, 0)
          if (!c00 && c01 || c00 && !c01) poly :+= Pt(0, int(at(x, y), at(x, y + 1)))
          if (c01) poly :+= Pt(0, 1)
          if (!c01 && c11 || c01 && !c11) poly :+= Pt(int(at(x, y + 1), at(x + 1, y + 1)), 1)
          if (c11) poly :+= Pt(1, 1)
          if (!c11 && c10 || c11 && !c10) poly :+= Pt(1, int(at(x + 1, y), at(x + 1, y + 1)))
          if (c10) poly :+= Pt(1, 0)
          if (!c10 && c00 || c10 && !c00) poly :+= Pt(int(at(x, y), at(x + 1, y)), 0)
          if (!poly.isEmpty) b.drawPoly(Identity, poly, color)
        }
      }
    }
    buffer.drawPgram(Scale(width, height), Color(0xFF000033))
    layer(0.5, Color(0xFFFFFFFF))
  }
  
}

class Metaball {
  
  var pos = Pt(Metaballs.r.nextDouble * Metaballs.width, Metaballs.r.nextDouble * Metaballs.height)
  var vel = Pt(Metaballs.r.nextGaussian(), Metaballs.r.nextGaussian()) / 4
  
  def step(dt: Double) = {
    pos += dt * vel
    if (pos.x > Metaballs.width) pos -= Pt(Metaballs.width, 0)
    if (pos.y > Metaballs.height) pos -= Pt(0, Metaballs.height)
    if (pos.x < 0) pos += Pt(Metaballs.width, 0)
    if (pos.y < 0) pos += Pt(0, Metaballs.height)
  }
  def density(at: Pt) = {
    var diff = Pt(10000, 10000)
    for (dx <- -1 to 1; dy <- -1 to 1) {
      val d0 = pos - at + dx * Pt(Metaballs.width, 0) + dy * Pt(0, Metaballs.height)
      if (d0.abs2 < diff.abs2) diff = d0
    }
    if (diff.abs2 > 1) 0
    else {
      val x = (1 - diff.abs2)
      x * x
    }
  }
  
}