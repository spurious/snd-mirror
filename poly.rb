# poly.rb -- polynomial-related stuff; poly.scm --> poly.rb -*- snd-ruby -*-

# Translator: Michael Scholz <scholz-micha@gmx.de>
# Created: Sat Apr 09 23:55:07 CEST 2005
# Last: Mon May 16 18:14:57 CEST 2005

# Commentary: (see poly.scm)
#
# rational?(obj)
#
# class Poly
#  inspect
#  to_poly
#  reduce
#  +(other)
#  *(other)
#  /(other)
#  derivative
#  gcd(other)
#  roots
#
# class Float
#  +(other)
#  *(other)
#  /(other)
#
# class String
#  to_poly
#
# class Array
#  to_poly
#
# class Vct
#  to_poly
#
# Poly(obj)
# make_poly(len, init, &body)
# poly?(obj)
# poly(*vals)
# poly_reduce(obj)
# poly_add(obj1, obj2)
# poly_multiply(obj1, obj2)
# poly_div(obj1, obj2)
# poly_derivative(obj)
# poly_gcd(obj1, obj2)
# poly_roots(obj)
#
# Code:

require "examp"
require "complex"
require "rational"

class Poly < Vec
  def inspect
    @name = "poly"
    super
  end
  
  def to_poly
    self
  end
  
  def reduce
    if self.last.zero?
      ret = self.dup
      i = 0
      (ret.length - 1).downto(0) do |i|
        if ret[i].nonzero?
          break
        end
      end
      ret[0, i + 1]
    else
      self
    end
  end
  # [1, 2, 3].to_poly.reduce             ==> poly(1.000, 2.000, 3.000)
  # poly(1, 2, 3, 0, 0, 0).reduce        ==> poly(1.000, 2.000, 3.000)
  # vct(0, 0, 0, 0, 1, 0).to_poly.reduce ==> poly(0.000, 0.000, 0.000, 0.000, 1.000)
  
  def poly_add(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "a poly, a vct an array, or a number")
    if number?(other)
      v = self.dup
      v[0] += other
      v
    else
      if self.length > other.length
        self.add(other)
      else
        Poly(other).add(self)
      end
    end
  end
  alias + poly_add
  # poly(0.1, 0.2, 0.3) + poly(0, 1, 2, 3, 4) ==> poly(0.100, 1.200, 2.300, 3.000, 4.000)
  # poly(0.1, 0.2, 0.3) + 0.5                 ==> poly(0.600, 0.200, 0.300)
  # 0.5 + poly(0.1, 0.2, 0.3)                 ==> poly(0.600, 0.200, 0.300)

  def poly_multiply(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "a poly, a vct an array, or a number")
    if number?(other)
      Poly(self.scale(Float(other)))
    else
      len = self.length + other.length
      m = Poly.new(len, 0.0)
      self.each_with_index do |val1, i|
        other.each_with_index do |val2, j|
          m[i + j] += val1 * val2
        end
      end
      m
    end
  end
  alias * poly_multiply
  # poly(1, 1) * poly(-1, 1)        ==> poly(-1.000, 0.000, 1.000, 0.000)
  # poly(-5, 1) * poly(3, 7, 2)     ==> poly(-15.000, -32.000, -3.000, 2.000, 0.000)
  # poly(-30, -4, 2) * poly(0.5, 1) ==> poly(-15.000, -32.000, -3.000, 2.000, 0.000)
  # poly(-30, -4, 2) * 0.5          ==> poly(-15.000, -2.000, 1.000)
  # 2.0 * poly(-30, -4, 2)          ==> poly(-60.000, -8.000, 4.000)

  def poly_div(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "a poly, a vct, an array, or a number")
    if number?(other)
      [self * (1.0 / other), poly(0.0)]
    else
      if other.length > self.length
        [poly(0.0), Poly(other)]
      else
        len = [self.length, other.length].max
        r = Poly.new(len, 0.0)
        q = Poly.new(len, 0.0)
        r.map_with_index! do |val, i| self[i] end
        n = self.length - 1
        nv = other.length - 1
        (n - nv).downto(0) do |i|
          q[i] = r[nv + i] / other[nv]
          (nv + i - 1).downto(i) do |j| r[j] -= q[i] * other[j - i] end
        end
        nv.upto(n) do |i| r[i] = 0.0 end
        [q, r]
      end
    end
  end
  alias / poly_div
  # poly(-1.0, 0.0, 1.0) / poly(1.0, 1.0)
  #                      ==> [poly(-1.000, 1.000, 0.000), poly(0.000, 0.000, 0.000)]
  # poly(-15, -32, -3, 2) / poly(-5, 1)
  #                      ==> [poly(3.000, 7.000, 2.000, 0.000), poly(0.000, 0.000, 0.000, 0.000)]
  # poly(-15, -32, -3, 2) / poly(3, 1)
  #                      ==> [poly(-5.000, -9.000, 2.000, 0.000), poly(0.000, 0.000, 0.000, 0.000)]
  # poly(-15, -32, -3, 2) / poly(0.5, 1)
  #                      ==> [poly(-30.000, -4.000, 2.000, 0.000), poly(0.000, 0.000, 0.000, 0.000)]
  # poly(-15, -32, -3, 2) / poly(3, 7, 2)
  #                      ==> [poly(-5.000, 1.000, 0.000, 0.000), poly(0.000, 0.000, 0.000, 0.000)]
  # poly(-15, -32, -3, 2) / 2.0
  #                      ==> [poly(-7.500, -16.000, -1.500, 1.000), poly(0.000)]

  def derivative
    if complex?(self.first)
      len = self.length
      pl = Array.new(len, 0.0)
      (len - 1).downto(0) do |i| pl[i] = self[i] * i.to_f end
      pl
    else
      len = self.length - 1
      pl = Poly.new(len, 0.0)
      j = len
      (len - 1).downto(0) do |i|
        pl[i] = self[j] * j.to_f
        j -= 1
      end
      pl
    end
  end
  # poly(0.5, 1.0, 2.0, 4.0).derivative ==> poly(1.000, 4.000, 12.000)

  def gcd(other)
    assert_type((array?(other) or vct?(other)), other, 0, "a poly, a vct or an array")
    if self.length < other.length
      poly(0.0)
    else
      qr = self.poly_div(other).map do |m| m.reduce end
      if qr[1].length == 1
        if qr[1][0].zero?
          Poly(other)
        else
          poly(0.0)
        end
      else
        qr[0].gcd(qr[1])
      end
    end
  end
  # (poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(2, 1))               ==> poly(2.000, 1.000)
  # (poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(3, 1))               ==> poly(0.000)
  # (poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(-3, 1))              ==> poly(-3.000, 1.000)
  # (poly(8, 1) * poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(-3, 1)) ==> poly(-3.000, 1.000)
  # (poly(8, 1) * poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(8, 1) * poly(-3, 1)).reduce
  #                                                                 ==> poly(-24.000, 5.000, 1.000)
  # poly(-1, 0, 1).gcd(poly(2, -2, -1, 1))                          ==> poly(0.000)
  # poly(2, -2, -1, 1).gcd(poly(-1, 0, 1))                          ==> poly(1.000, -1.000)
  # poly(2, -2, -1, 1).gcd(poly(-2.5, 1))                           ==> poly(0.000)

  def roots
    if (deg = self.length - 1).zero?
      []
    else
      if self[0].zero?
        if deg == 1
          poly(0.0)
        else
          Poly.new(deg) do |i| self[i + 1] end.roots.unshift(0.0)
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
                quad_roots = poly(self[0], self[deg / 2], self[deg]).roots
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
                    yroot = poly(4 * a0 * a2 + -(a1 * a1) + -(a3 * a3 * a0),
                                 a1 * a3 - 4 * a0,
                                 -a2,
                                 1.0).roots
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
                    pp = self.derivative
                    qp = pp.dup
                    n = deg
                    x = Complex(1.3, 0.314159)
                    v = q.eval(x)
                    m = v.abs * v.abs
                    accuracy = 1.0e-7
                    dx = 0.0
                    until c_g?
                      dx = v / qp.eval(x)
                      break if dx.abs <= accuracy
                      20.times do
                        break if dx.abs <= accuracy
                        y = x - dx
                        v1 = q.eval(y)
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
                    x -= self.eval(x) / pp.eval(x)
                    x -= self.eval(x) / pp.eval(x)
                    if x.image < accuracy
                      x = x.real
                      q = q.poly_div([-x, 1.0])
                      n -= 1
                    else
                      q = q.poly_div([x.abs, 0.0, 1.0])
                      n -= 2
                    end
                    rts = poly(x)
                    if n > 0 then rts.unshift(*q.first.reduce.roots) end
                    rts
                  end
                end
              end
            end
          end
        end
      end
    end
  end

  def eval(x)
    if self.null?
      0.0
    else
      sum = self.last
      (self.length - 2).downto(0) do |i| sum = sum * x + self[i] end
      sum
    end
  end

  private
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
end

class Float
  unless defined? 0.0.poly_plus
    alias fp_plus +
    def poly_plus(other)
      case other
      when Poly
        other[0] += self
        other
      else
        self.fp_plus(other)
      end
    end
    alias + poly_plus
  end

  unless defined? 0.0.poly_times
    alias fp_times *
    def poly_times(other)
      case other
      when Poly
        Poly(other.scale(self))
      else
        self.fp_times(other)
      end
    end
    alias * poly_times
  end

  unless defined? 0.0.poly_div
    alias fp_div /
    def poly_div(other)
      case other
      when Poly
        [poly(0.0), other]
      else
        self.fp_div(other)
      end
    end
    alias / poly_div
  end
end

class String
  def to_poly
    if self.scan(/^poly\([-+,.)\d\s]+/).null?
      nil
    else
      eval(self)
    end
  end
end

class Array
  def to_poly
    poly(*self)
  end
end

class Vct
  def to_poly
    poly(*self.to_a)
  end
end

def Poly(obj)
  if obj.nil? then obj = [] end
  assert_type(obj.respond_to?(:to_poly), obj, 0,
              "an object containing method 'to_poly' (Vct, String, Array and subclasses)")
  obj.to_poly
end

def make_poly(len, init = 0.0, &body)
  Poly.new(len, init, &body)
end

def poly?(obj)
  obj.instance_of?(Poly)
end

def poly(*vals)
  Poly.new(vals.length) do |i|
    if integer?(val = vals[i])
      Float(val)
    else
      val
    end
  end
end

def poly_reduce(obj)
  assert_type(obj.respond_to?(:to_poly), obj, 0, "an object containing method 'to_poly'")
  Poly(obj).reduce
end

def poly_add(obj1, obj2)
  if number?(obj1)
    assert_type(obj2.respond_to?(:to_poly), obj2, 1, "an object containing method 'to_poly'")
    Float(obj1) + Poly(obj2)
  else
    assert_type(obj1.respond_to?(:to_poly), obj1, 0, "an object containing method 'to_poly'")
    Poly(obj1) + obj2
  end
end

def poly_multiply(obj1, obj2)
  if number?(obj1)
    assert_type(obj2.respond_to?(:to_poly), obj2, 1, "an object containing method 'to_poly'")
    Float(obj1) * Poly(obj2)
  else
    assert_type(obj1.respond_to?(:to_poly), obj1, 0, "an object containing method 'to_poly'")
    Poly(obj1) * obj2
  end
end

def poly_div(obj1, obj2)
  if number?(obj1)
    assert_type(obj2.respond_to?(:to_poly), obj2, 1, "an object containing method 'to_poly'")
    Float(obj1) / Poly(obj2)
  else
    assert_type(obj1.respond_to?(:to_poly), obj1, 0, "an object containing method 'to_poly'")
    Poly(obj1) / obj2
  end
end

def poly_derivative(obj)
  assert_type(obj.respond_to?(:to_poly), obj, 0, "an object containing method 'to_poly'")
  Poly(obj).derivative
end

def poly_gcd(obj1, obj2)
  assert_type(obj.respond_to?(:to_poly), obj, 0, "an object containing method 'to_poly'")
  Poly(obj1).gcd(obj2)
end

def poly_roots(obj)
  assert_type(obj.respond_to?(:to_poly), obj, 0, "an object containing method 'to_poly'")
  Poly(obj).roots
end

# poly.rb ends here
