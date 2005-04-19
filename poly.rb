# poly.rb -- polynomial-related stuff; poly.scm --> poly.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Sat Apr 09 23:55:07 CEST 2005
# Last: Mon Apr 18 04:21:08 CEST 2005

# Commentary:
#
# see poly.scm
#
# module Poly
#  poly_new(len, init, &body)
#  poly_eval(x)
#  poly_reduce
#  poly_add(other)
#  poly_multiply(other)
#  poly_div(other)
#  poly_derivative
#  poly_gcd(other)
#  poly_roots
#
# class Float
#  poly_add(other)
#  poly_multiply(other)
#  poly_div(other)
#
# class Array
#  include Poly
#  add(other)
#  add!(other)
#  scale(scl)
#  scale!(scl)
#
# class Vct
#  include Poly
#  to_vct
#
# poly_reduce(obj)
# poly_add(obj1, obj2)
# poly_multiply(obj1, obj2)
# poly_div(obj1, obj2)
# poly_derivative(obj)
# poly_gdc(obj1, obj2)
# poly_roots(obj)
#
# Code:

require "examp"
require "complex"
require "rational"

module Poly
  def poly_new(len, init = 0.0, &body)
    case self.class
    when Vct
      make_vct!(len, init, &body)
    when Array
      make_array(len, init, &body)
    else
      nil
    end
  end
  
  def poly_eval(x)
    if self.null?
      0.0
    else
      sum = self.last
      (self.length - 2).downto(0) do |i| sum = sum * x + self[i] end
      sum
    end
  end
  
  def poly_reduce
    if self.last.zero?
      ret = self.to_a.dup
      i = 0
      (ret.length - 1).downto(0) do |i|
        if ret[i].nonzero?
          break
        end
      end
      ret[0, i + 1].to_vct
    else
      self
    end
  end
  # [1, 2, 3].poly_reduce             ==> [1, 2, 3]
  # vct(1, 2, 3, 0, 0, 0).poly_reduce ==> #<vct[len=3]: 1.000 2.000 3.000>
  # vct(0, 0, 0, 0, 1, 0).poly_reduce ==> #<vct[len=5]: 0.000 0.000 0.000 0.000 1.000>
  
  def poly_add(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "an array, a vct or a number")
    if number?(other)
      v = self.dup
      v[0] += other
      v
    else
      other = if vct?(self)
                other.to_vct
              else
                other.to_a
              end
      if self.length > other.length
        self.add(other)
      else
        other.add(self)
      end
    end
  end
  # vct(0.1, 0.2, 0.3) + vct(0, 1, 2, 3, 4) ==> #<vct[len=5]: 0.100 1.200 2.300 3.000 4.000>
  # vct(0.1, 0.2, 0.3) + 0.5                ==> #<vct[len=3]: 0.600 0.200 0.300>
  # 0.5 + vct(0.1, 0.2, 0.3)                ==> #<vct[len=3]: 0.600 0.200 0.300>

  def poly_multiply(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "an array, a vct or a number")
    if number?(other)
      self.scale(Float(other))
    else
      len = self.length + other.length
      m = self.class.new(len, 0.0)
      self.each_with_index do |val1, i|
        other.each_with_index do |val2, j|
          m[i + j] += val1 * val2
        end
      end
      m
    end
  end
  # vct(1, 1) * vct(-1, 1)     ==> #<vct[len=4]: -1.000 0.000 1.000 0.000>
  # [-5, 1] * [3, 7, 2]        ==> [-15.0, -32.0, -3.0, 2.0, 0.0]
  # [-30, -4, 2] * vct(0.5, 1) ==> [-15.0, -32.0, -3.0, 2.0, 0.0]
  # vct(-30, -4, 2) * 0.5      ==> #<vct[len=3]: -15.000 -2.000 1.000>
  # 2.0 * [-30, -4, 2]         ==> [-60.0, -8.0, 4.0]

  def poly_div(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "an array, a vct or a number")
    if number?(other)
      [self.poly_multiply(1.0 / other), self.class.new(1, 0.0)]
    else
      if other.length > self.length
        [self.class.new(1, 0.0), other]
      else
        len = [self.length, other.length].max
        r = self.class.new(len)
        r.map_with_index! do |val, i| self[i] end
        q = self.class.new(len, 0.0)
        n = self.length - 1
        nv = other.length - 1
        (n - nv).downto(0) do |i|
          q[i] = r[nv + i] / other[nv]
          (nv + i - 1).downto(i) do |j|
            r[j] -= q[i] * other[j - i]
          end
        end
        nv.upto(n) do |i| r[i] = 0.0 end
        [q, r]
      end
    end
  end
  # vct(-1.0, 0.0, 1.0) / vct(1.0, 1.0)
  #   ==> [#<vct[len=3]: -1.000 1.000 0.000>, #<vct[len=3]: 0.000 0.000 0.000>]
  # vct(-15, -32, -3, 2) / vct(-5, 1)
  #   ==> [#<vct[len=4]: 3.000 7.000 2.000 0.000>, #<vct[len=4]: 0.000 0.000 0.000 0.000>]
  # [-15, -32, -3, 2] / vct(3, 1)
  #   ==> [[-5.0, -9.0, 2.0, 0.0], [0.0, 0.0, 0.0, 0.0]]
  # vct(-15, -32, -3, 2) / vct(0.5, 1)
  #   ==> [#<vct[len=4]: -30.000 -4.000 2.000 0.000>, #<vct[len=4]: 0.000 0.000 0.000 0.000>]
  # [-15, -32, -3, 2] / vct(3, 7, 2)
  #   ==> [[-5.0, 1.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0]]
  # vct(-15, -32, -3, 2) / 2.0
  #   ==> [#<vct[len=4]: -7.500 -16.000 -1.500 1.000>, #<vct[len=1]: 0.000>]

  def poly_derivative
    if complex?(self.first)
      len = self.length
      pl = Array.new(len, 0.0)
      (len - 1).downto(0) do |i| pl[i] = self[i] * i.to_f end
      pl
    else
      len = self.length - 1
      pl = self.class.new(len, 0.0)
      j = len
      (len - 1).downto(0) do |i|
        pl[i] = self[j] * j.to_f
        j -= 1
      end
      pl
    end
  end
  # vct(0.5, 1.0, 2.0, 4.0).poly_derivative ==> #<vct[len=3]: 1.000 4.000 12.000>

  def simplify_complex(a)
    if a.image.abs < 1.0e-7
      if a.real.abs < 1.0e-7
        0.0
      else
        a.real
      end
    else
      if a.real.abs < 1.0e-7
        make_rectangular(0.0, a.image)
      else
        a
      end
    end
  end

  def poly_gcd(other)
    assert_type((array?(other) or vct?(other)), other, 0, "a vct or an array")
    if self.length < other.length
      self.class.new(1, 0.0)
    else
      qr = self.poly_div(other).map do |m| m.poly_reduce end
      if qr[1].length == 1
        if qr[1][0].zero?
          other
        else
          self.class.new(1, 0.0)
        end
      else
        qr[0].poly_gcd(qr[1])
      end
    end
  end
  # vct(2, 1).poly_multiply(vct(-3, 1)).poly_reduce.poly_gcd(vct(2, 1))
  #  ==> #<vct[len=2]: 2.000 1.000>
  # vct(2, 1).poly_multiply(vct(-3, 1)).poly_reduce.poly_gcd(vct(3, 1))
  # FIXME: vct(6)
  #  ==> #<vct[len=1]: 0.000>
  # vct(2, 1).poly_multiply(vct(-3, 1)).poly_reduce.poly_gcd(vct(-3, 1))
  #  ==> #<vct[len=2]: -3.000 1.000>
  # vct(8, 1).poly_multiply(vct(2, 1).poly_multiply(vct(-3, 1))).poly_reduce.poly_gcd(vct(-3, 1))
  #  ==> #<vct[len=2]: -3.000 1.000>
  # vct(8, 1).poly_multiply(vct(2, 1).poly_multiply(vct(-3, 1))).poly_reduce.poly_gcd(vct(8, 1).poly_multiply(vct(-3, 1)).poly_reduce)
  #  ==> #<vct[len=3]: -24.000 5.000 1.000>
  # vct(-1, 0, 1).poly_gcd(vct(2, -2, -1, 1)) ==> #<vct[len=1]: 0.000>
  # vct(2, -2, -1, 1).poly_gcd(vct(-1, 0, 1)) ==> #<vct[len=2]: 1.000 -1.000>
  # vct(2, -2, -1, 1).poly_gcd(vct(-2.5, 1))  ==> #<vct[len=1]: 0.000>

  def poly_roots
    if (deg = self.length - 1).zero?
      []
    else
      if self[0].zero?
        if deg == 1
          [0.0]
        else
          [0.0] + make_array(deg) do |i| self[i + 1] end.poly_roots
        end
      else
        if deg == 1
          [-self[0] / self[1]]
        else
          if deg == 2
            a = self[2]
            b = self[1]
            c = self[0]
            d = sqrt(b * b - 4 * a * c)
            [(-b + d ) / (2 * a), (-b - d) / (2 * a)]
          else
            ones = 0
            1.upto(deg) do |i| if self[i].nonzero? then ones += 1 end end
            if ones == 1
              val1 = -self[0] / self[deg]
              val2 = 1.0 / deg
              if (n = val1 ** val2).nan? then n = Complex(val1) ** Complex(val2) end
              mag = n.abs
              roots = [n]
              incr = TWO_PI / deg
              ang = incr
              1.upto(deg - 1) do |i|
                roots.unshift(simplify_complex(make_polar(mag, ang)))
                ang += incr
              end
              roots
            else
              if ones == 2 and deg.even? and self[deg / 2].nonzero?
                quad_roots = [self[0], self[deg / 2], self[deg]].poly_roots
                roots = []
                n = deg / 2
                quad_roots.each do |qr|
                  if n == 2
                    sq = sqrt(qr)
                    roots.unshift(sq, -sq)
                  else
                    xqr = qr ** (1.0 / n)
                    mag = xqr.abs
                    incr = TWO_PI / n
                    ang = 0.0
                    n.times do
                      roots.unshift(simplify_complex(make_polar(mag, ang)))
                      ang += incr
                    end
                  end
                end
                roots
              else
                if deg == 3
                  a0 = self[0] / self[3]
                  a1 = self[1] / self[3]
                  a2 = self[2] / self[3]
                  q = a1 / 3.0 - (a2 * a2) / 9.0
                  r = (a1 * a2 - 3.0 * a0) / 6.0 - (a2 * a2 * a2) / 27.0
                  q3r2 = q * q * q + r * r
                  sq3r2 = sqrt(q3r2)
                  s1 = (q3r2.zero? ? -(r.abs ** (1.0 / 3.0)) : ((r + sq3r2) ** (1.0 / 3.0)))
                  s2 = (q3r2.zero? ? s1 : ((r - sq3r2) ** (1.0 / 3.0)))
                  z1 = (s1 + s2) - a2 / 3.0
                  z2 = -0.5 * (s1 + s2) +
                       a2 / -3.0 +
                       (s1 - s2) * 0.5 * sqrt(3.0) * make_rectangular(0.0, 1.0)
                  z3 = -0.5 * (s1 + s2) +
                       a2 / -3.0 +
                           (s1 - s2) * -0.5 * sqrt(3.0) * make_rectangular(0.0, 1.0)
                  [z1, z2, z3].map do |val|
                    if complex?(val) and val.image.zero?
                      val.real
                    else
                      val
                    end
                  end
                else
                  if deg == 4
                    a0 = self[0] / self[4]
                    a1 = self[1] / self[4]
                    a2 = self[2] / self[4]
                    a3 = self[3] / self[4]
                    yroot = [4 * a0 * a2 + -(a1 * a1) + -(a3 * a3 * a0),
                             a1 * a3 - 4 * a0,
                             -a2,
                             1.0].poly_roots
                    y1 = if (not complex?(yroot[0]))
                           yroot[0]
                         else
                           if (not complex?(yroot[0]))
                             yroot[1]
                           else
                             yroot[2]
                           end
                         end
                    r = sqrt(0.25 * a3 * a3 + -a2 + y1)
                    d = if r.zero?
                          sqrt(0.75 * a3 * a3 + -2 * a2 + 2 * sqrt(y1 * y2 - 4 * a0))
                        else
                          sqrt(0.75 * a3 * a3 + -2 * a2 + -(r * r) +
                                     (0.25 * (4 * a3 * a2 + -8 * a1 + -(a3 * a3 * a3))) / r)
                        end
                    e = if r.zero?
                          sqrt(0.75 * a3 * a3 + -2 * a2 + -2 * sqrt(y1 * y1 - 4 * a0))
                        else
                          sqrt(0.75 * a3 * a3 + -2 * a2 + -(r * r) +
                                     (-0.25 * (4 * a3 * a2 + -8 * a1 + -(a3 * a3 * a3))) / r)
                        end
                    z1 = -0.25 * a3 + 0.5 * r + 0.5 * d
                    z2 = -0.25 * a3 + 0.5 * r + -0.5 * d
                    z3 = -0.25 * a3 + -0.5 * r + 0.5 * e
                    z4 = -0.25 * a3 + -0.5 * r + -0.5 * e
                    [z1, z2, z3, z4].map do |val|
                      if complex?(val) and val.image.zero?
                        val.real
                      else
                        val
                      end
                    end
                  else
                    q = self.dup
                    pp = self.poly_derivative
                    qp = pp.dup
                    n = deg
                    x = Complex(1.3, 0.314159)
                    v = q.poly_eval(x)
                    m = v.abs * v.abs
                    accuracy = 1.0e-7
                    dx = 0.0
                    until c_g?
                      dx = v / qp.poly_eval(x)
                      break if dx.abs <= accuracy
                      20.times do
                        break if dx.abs <= accuracy
                        y = x - dx
                        v1 = q.poly_eval(y)
                        if (m1 = v1.abs * v1.abs) < m
                          x = y
                          v = v1
                          m = m1
                          break
                        else
                          dx /= 4.0
                        end
                      end
                    end
                    x -= self.poly_eval(x) / pp.poly_eval(x)
                    x -= self.poly_eval(x) / pp.poly_eval(x)
                    if x.image < accuracy
                      x = x.real
                      q = q.poly_div([-x, 1.0])
                      n -= 1
                    else
                      q = q.poly_div([x.abs, 0.0, 1.0])
                      n -= 2
                    end
                    roots = [x]
                    if n > 0 then roots.unshift(*q.first.poly_reduce.poly_roots) end
                    roots
                  end
                end
              end
            end
          end
        end
      end
    end
  end
end

class Float
  def poly_add(other)
    assert_type((array?(other) or vct?(other)), other, 0, "a vct or an array")
    pl = other.dup
    pl[0] += self
    pl
  end

  def poly_multiply(other)
    assert_type((array?(other) or vct?(other)), other, 0, "a vct or an array")
    other.scale(self)
  end

  def poly_div(other)
    assert_type((array?(other) or vct?(other)), other, 0, "a vct or an array")
    [other.class.new(1, 0.0), other]
  end

  def add(other)
    if number?(other)
      self.old_add(other)
    else
      poly_add(other)
    end
  end
  alias old_add +
  alias + add                 

  def multiply(other)
    if number?(other)
      self.old_multiply(other)
    else
      poly_multiply(other)
    end
  end
  alias old_multiply *
  alias * multiply

  def divide(other)
    if number?(other)
      self.old_divide(other)
    else
      poly_div(other)
    end
  end
  alias old_divide /
  alias / divide
end

class Array
  include Poly
  
  def new_add(other)
    case other
    when Vct, Numeric
      poly_add(other)
    else
      self.old_add(other)
    end
  end
  alias old_add +
  alias + new_add                 
  
  def new_multiply(other)
    case other
    when Vct
      poly_multiply(other)
    else
      self.old_multiply(other)
    end
  end
  alias old_multiply *
  alias * new_multiply
  
  def new_divide(other)
    case other
    when Vct
      poly_div(other)
    else
      self.old_divide(other)
    end
  end
  alias / new_divide
  
  def add(other)
    new_ary = self.dup
    [self.length, other.length].min.times do |i| new_ary[i] += other[i] end
    new_ary
  end

  def add!(other)
    [self.length, other.length].min.times do |i| self[i] += other[i] end
    self
  end

  def scale(scl)
    scl = Float(scl)
    make_array(self.length) do |i| self[i] * scl end
  end

  def scale!(scl)
    scl = Float(scl)
    self.map! do |val| val *= scl end
  end
end

class Vct
  include Poly

  def to_vct
    self
  end
  
  alias + poly_add
  alias * poly_multiply
  alias / poly_div
end

def poly_reduce(obj)
  obj.reduce
end

def poly_add(obj1, obj2)
  obj1.poly_add(obj2)
end

def poly_multiply(obj1, obj2)
  obj1.poly_multiply(obj2)
end

def poly_div(obj1, obj2)
  obj1.poly_div(obj2)
end

def poly_derivative(obj)
  obj.derivative
end

def poly_gcd(obj1, obj2)
  obj1.poly_gcd(obj2)
end

def poly_roots(obj)
  obj.poly_roots
end

# poly.rb ends here
