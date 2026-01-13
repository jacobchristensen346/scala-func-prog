/* This script defines functions 
which perform a estimation of the
square root of a value based on a
starting guess value */

def mean(j: Double, k: Double): Double =
  (j + k) / 2

def abs(x: Double): Double =
  if x < 0 then (-1 * x) else x

def closeEnough(w: Double, z: Double): Boolean =
  abs(w - z) / mean(w, z) < 0.001

def sqrtIter(guess: Double, x: Double): Double = {
  val meanie = mean(x / guess, guess)
  println(meanie)
  if closeEnough(meanie, guess) then meanie else sqrtIter(meanie, x)
  }
