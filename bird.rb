# bird songs
# translated (semi-automatically) from a Sambox note list to bird.clm, then bird.scm, then bird.rb

$out_file = false
$out_data = false
$with_editable_mixes = false

def normalize_partials(lst1)
  lst = lst1.flatten # i.e. copy -- there doesn't seem to be a copy method for arrays?
  sum = 0.0
  len = (lst.length) / 2
  0.upto(len - 1) do |i|
    sum = sum + lst[(i * 2) + 1]
  end
  0.upto(len - 1) do |i|
    lst[(i * 2) + 1] = lst[(i * 2) + 1] / sum
  end
  lst
end

def bigbird(start, dur, frequency, freqskew, amplitude, freq_envelope, amp_envelope, partials)
  gls_env = make_env(freq_envelope, hz2radians(freqskew), dur)
  os = make_oscil(frequency)
  coeffs = partials2polynomial(normalize_partials(partials))
  amp_env = make_env(amp_envelope, amplitude, dur)	
  beg = (srate() * start).round
  len = (srate() * dur).round
  local_data  = make_vct len
  vct_map!(local_data,
	   Proc.new { ||
		      env(amp_env) *
		      polynomial(coeffs,
			         oscil(os, env(gls_env)))
		    })
  vct_add!($out_data, local_data, beg)
end

def bird(start, dur, frequency, freqskew, amplitude, freq_envelope, amp_envelope)
  gls_env = make_env(freq_envelope, hz2radians(freqskew), dur)
  os = make_oscil(frequency)
  amp_env = make_env(amp_envelope, amplitude, dur)
  beg = (srate() * start).round
  len = (srate() * dur).round
  local_data  = make_vct len
  vct_map!(local_data,
	   Proc.new { ||
		      env(amp_env) * oscil(os, env(gls_env))
		    })
  vct_add!($out_data, local_data, beg)
end

def one_bird(beg, maxdur, func, birdname)
  $out_data = make_vct((srate() * maxdur).round)
  func.call()
  mix_vct($out_data, (beg*srate()).round, $out_file, 0, $with_editable_mixes)
  birdname
end

$main_amp = [.00, .00, .25, 1.00, .60, .70, .75, 1.00, 1.00, .0]
$bird_tap = [.00, .00, .01, 1.00, .99, 1.00, 1.00, .0]
$bird_amp = [.00, .00, .25, 1.00, .75, 1.00, 1.00, .0]

def orchard_oriole(beg)
  oriup = [.00, .00, 1.00, 1.0]
  oridwn = [.00, 1.00, 1.00, .0]
  oriupdwna = [.00, .00, .60, 1.00, 1.00, .60]
  oriupdwnb = [.00, .50, .30, 1.00, 1.00, .0]
  oribiga = [.00, .90, .15, 1.00, .40, .30, .60, .60, .85, .00, 1.00, .0]
  orimid = [.00, 1.00, .05, .50, .10, 1.00, .25, .00, .85, .50, 1.00, .0]
  oridwnup = [.00, .30, .25, .00, 1.00, 1.0]
  oriamp = [.00, .00, .10, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.38, .03, 3700, 100, .05, oridwn, $main_amp)
   bird(.41, .05, 2500, 1000, .1, oriup, $main_amp)
   bigbird(.5, .1, 2000, 800, .2, oriupdwna, $main_amp, [1, 1, 2, .02, 3, .05])
   bird(.65, .03, 3900, 1200, .1, oridwn, $main_amp)
   bigbird(.7, .21, 2000, 1200, .15, oribiga, $main_amp, [1, 1, 2, .05])
   bird(1.0, .05, 4200, 1000, .1, oridwn, $main_amp)
   bigbird(1.1, .1, 2000, 1000, .25, orimid, $main_amp, [1, 1, 2, .05])
   bigbird(1.3, .1, 2000, 1000, .25, orimid, $main_amp, [1, 1, 2, .05])
   bird(1.48, .1, 2300, 3200, .1, oriupdwnb, oriamp)
   bird(1.65, .03, 1800, 300, .05, oriup, $main_amp)
   bird(1.7, .03, 2200, 100, .04, oridwn, $main_amp)
   bird(1.8, .07, 2500, 2000, .15, oriupdwnb, oriamp)
   bigbird(1.92, .2, 2400, 1200, .25, oridwnup, $main_amp, [1, 1, 2, .04])
   bird(2.2, .02, 2200, 3000, .04, oriup, $main_amp)
   bird(2.28, .02, 2200, 3000, .04, oriup, $main_amp)
   bigbird(2.4, .17, 2000, 1000, .2, oriupdwna, oriamp, [1, 1, 2, .04])
	},
  report_in_minibuffer("orchard_oriole"))
end


def cassins_kingbird(beg)
  kingfirst = [.00, .30, .45, 1.00, .90, .10, 1.00, .0]
  kingsecond = [.00, .00, .02, .50, .04, .00, .06, .55, .08, .05, .10, .60, .12, .05, .14, .65, .16, .10, .18, .70, .20, .10, .22, .75, .24, .15, .26, .80, .28, .20, .30, .85, .32, .25, .34, .90, .36, .30, .38, .95, .40, .40, .42, 1.00, .44, .50, .46, 1.00, .48, .45, .50, 1.00, .52, .50, .54, 1.00, .56, .40, .58, .95, .60, .40, .62, .90, .64, .40, .66, .85, .68, .35, .70, .80, .72, .30, .74, .75, .76, .25, .78, .70, .80, .20, .82, .65, .84, .10, .86, .60, .88, .00, .90, .55, .92, .00, .94, .50, .96, .00, 1.00, .40]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bigbird(.03, .04, 1700, 1200, .15, kingfirst, $main_amp, [1, 1, 2, .5, 3, 0, 4, .2])
   bigbird(.12, .18, 1700, 900, .25, kingsecond, $main_amp, [1, 1, 2, .01, 3, 0, 4, .1])
	},
  report_in_minibuffer("cassins_kingbird"))
end


def chipping_sparrow(beg)
  chip_up = [.00, .80, .15, 1.00, .75, .30, 1.00, .0]
  
  one_bird(beg, 1.1,
  Proc.new {||
   bird(0, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.06, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.12, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.18, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.24, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.30, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.36, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.42, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.48, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.54, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.60, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.66, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.72, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.78, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.84, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.90, .05, 4000, 2400, .2, chip_up, $main_amp)
   bird(.96, .05, 4000, 2400, .2, chip_up, $main_amp)
	},
  report_in_minibuffer("chipping_sparrow"))
end


def bobwhite(beg)
  bobup1 = [.00, .00, .40, 1.00, 1.00, 1.0]
  bobup2 = [.00, .00, .65, .50, 1.00, 1.0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bigbird(.4, .2, 1800, 200, .1, bobup1, $main_amp, [1, 1, 2, .02])
   bigbird(1, .20, 1800, 1200, .2, bobup2, $main_amp, [1, 1, 2, .02])
	},
  report_in_minibuffer("bobwhite"))
end


def western_meadowlark(beg)
  no_skw = [.00, .00, 1.00, .0]
  down_skw = [.00, 1.00, .40, .40, 1.00, .0]
  fas_down = [.00, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bigbird(.800, .1, 2010.000, 0.000, .100, no_skw, $main_amp, [1, 1, 2, .04])
   bigbird(1.100, .15, 3000.000, 100.000, .110, down_skw, $main_amp, [1, 1, 2, .04])
   bigbird(1.300, .25, 2000.000, 150.000, .200, down_skw, $main_amp, [1, 1, 2, .04])
   bigbird(1.650, .15, 3010.000, 250.000, .110, down_skw, $main_amp, [1, 1, 2, .04])
   bigbird(1.850, .10, 2200.000, 150.000, .110, down_skw, $main_amp, [1, 1, 2, .04])
   bigbird(2.000, .10, 3200.000, 1400.000, .110, fas_down, $main_amp, [1, 1, 2, .04])
   bigbird(2.200, .05, 2000.000, 200.000, .110, fas_down, $main_amp, [1, 1, 2, .04])
   bigbird(2.300, .10, 1600.000, 0.000, .110, fas_down, $main_amp, [1, 1, 2, .04])
	},
  report_in_minibuffer("western_meadowlark"))
end


def scissor_tailed_flycatcher(beg)
  scissor = [.00, .00, .40, 1.00, .60, 1.00, 1.00, .0]
  one_bird(beg, 1.0,
  Proc.new {||
   bigbird(0, .05, 1800, 1800, .2, scissor, $main_amp, [1, .5, 2, 1, 3, .5, 4, .1, 5, .01])
	},
  report_in_minibuffer("scissor_tailed_flycatcher"))
end


def great_horned_owl(beg)
  owlup = [.00, .00, .30, 1.00, 1.00, 1.0]
  owldown = [.00, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bigbird(.3, .1, 300, 0, .1, $main_amp, $main_amp, [1, 1, 3, .02, 7, .01])
   bigbird(.6, .4, 293, 6, .1, owldown, $main_amp, [1, 1, 3, .02, 7, .01])
   bigbird(1.75, .35, 293, 7, .1, owlup, $main_amp, [1, 1, 3, .02, 7, .01])
   bigbird(2.5, .2, 300, 0, .1, owlup, $main_amp, [1, 1, 3, .02, 7, .01])
	},
  report_in_minibuffer("great_horned_owl"))
end


def black_throated_gray_warbler(beg)
  grayone = [.00, .50, .02, .60, .04, .45, .06, .62, .08, .40, .10, .65, .12, .35, .14, .70, .18, .30, .20, .70, .22, .30, .24, .70, .25, .20, .30, .80, .35, .10, .40, .90, .45, .00, .50, 1.00, .55, .00, .60, 1.00, .65, .00, .70, 1.00, .75, .00, .80, 1.00, .85, .00, .90, 1.00, .95, .00, 1.00, .50]
  graytwo = [.00, .00, .01, .40, .02, .00, .03, .40, .04, .00, .05, .40, .06, .00, .07, .40, .08, .00, .09, .40, .10, .00, .25, .80, .40, .30, .55, 1.00, .70, .00, .85, .80, 1.00, .40]
  graythree = [.00, 1.00, .01, .60, .02, 1.00, .03, .60, .04, 1.00, .05, .60, .06, 1.00, .07, .60, .08, 1.00, .09, .60, .10, 1.00, .11, .60, .12, 1.00, .13, .60, .14, 1.00, .15, .60, .16, 1.00, .17, .60, .18, 1.00, .19, .60, .20, 1.00, .21, .55, .22, 1.00, .23, .50, .24, 1.00, .25, .50, .26, 1.00, .27, .50, .28, 1.00, .29, .50, .30, 1.00, .31, .50, .32, 1.00, .33, .50, .34, 1.00, .35, .50, .36, 1.00, .37, .50, .38, 1.00, .39, .50, .40, 1.00, .41, .50, .42, 1.00, .43, .50, .44, 1.00, .45, .50, .46, 1.00, .47, .50, .48, 1.00, .49, .50, .50, 1.00, .51, .50, .52, 1.00, .53, .50, .54, 1.00, .55, .50, .56, 1.00, .57, .50, .58, 1.00, .59, .50, .60, 1.00, 1.00, .0]
  grayfour = [.00, .00, 1.00, 1.0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(0, .12, 3700, 600, .05, grayone, $main_amp)
   bird(.18, .08, 3000, 800, .07, graytwo, $main_amp)
   bird(.28, .12, 3700, 600, .12, grayone, $main_amp)
   bird(.44, .08, 3000, 800, .15, graytwo, $main_amp)
   bird(.54, .12, 3700, 600, .20, grayone, $main_amp)
   bird(.72, .08, 3000, 800, .25, graytwo, $main_amp)
   bird(.82, .12, 3700, 600, .25, grayone, $main_amp)
   bird(.96, .2, 3000, 2000, .2, graythree, $main_amp)
   bird(1.2, .02, 4500, 500, .05, grayfour, $main_amp)
   bird(1.25, .02, 4200, 800, .05, grayfour, $main_amp)
   bird(1.3, .02, 4000, 900, .05, grayfour, $main_amp)
	},
  report_in_minibuffer("black_throated_gray_warbler"))
end


def yellow_warbler(beg)
  yellow_up = [.00, .00, .60, 1.00, 1.00, .50]
  yellow_swirl = [.00, 1.00, .05, 1.00, .60, .00, .80, .30, 1.00, .10]
  yellow_down = [.00, 1.00, 1.00, .0]
  yellow_last = [.00, .00, .30, .20, .80, .70, 1.00, 1.0]
  swirl_amp = [.00, .00, .90, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(0, .05, 5600, 400, .05, yellow_up, $main_amp)
   bird(.23, .12, 5000, 1500, .15, yellow_swirl, swirl_amp)
   bird(.45, .13, 5000, 1700, .17, yellow_swirl, swirl_amp)
   bird(.62, .16, 5000, 2000, .20, yellow_swirl, swirl_amp)
   bird(.85, .15, 5000, 2000, .20, yellow_swirl, swirl_amp)
   bird(1.05, .075, 3700, 1000, .20, yellow_down, $main_amp)
   bird(1.15, .075, 3700, 800, .15, yellow_down, $main_amp)
   bird(1.25, .075, 3700, 800, .15, yellow_down, $main_amp)
   bird(1.4, .2, 3700, 2000, .2, yellow_last, swirl_amp)
	},
  report_in_minibuffer("yellow_warbler"))
end


def black_necked_stilt(beg)
  upamp = [.00, .00, .90, 1.00, 1.00, .0]
  rampup = [.00, .00, .50, 1.00, 1.00, .20]
  
  one_bird(beg, 1.0,
  Proc.new {||
   bigbird(0, .1, 900, 100, .2, rampup, upamp, [1, .5, 2, 1, 3, .75, 4, .5, 5, .1])
   bigbird(.30, .1, 900, 200, .2, rampup, upamp, [1, .5, 2, 1, 3, .75, 4, .5, 5, .1])
   bigbird(.60, .1, 900, 250, .2, rampup, upamp, [1, .5, 2, 1, 3, .75, 4, .5, 5, .1])
	},
  report_in_minibuffer("black_necked_stilt"))
end



def chestnut_sided_warbler(beg)
  ycurve = [.00, 1.00, .30, .50, .60, 1.00, .80, .20, 1.00, .0]
  vcurve = [.00, .20, .50, 1.00, 1.00, .0]
  wcurve = [.00, .50, .15, .00, .45, .10, .60, 1.00, .70, .90, 1.00, .90]
  upcurve = [.00, .00, .95, 1.00, 1.00, 1.0]
  downcurve = [.00, 1.00, .25, .30, .60, .15, 1.00, .0]
  louder = [.00, .00, .90, 1.00, 1.00, .0]
  wamp = [.00, .00, .10, 1.00, .40, .10, .50, .90, .60, .10, .70, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bigbird(.1, .1, 4050, 1200, .05, ycurve, $main_amp, [1, 1, 2, .1])
   bigbird(.25, .03, 3900, 300, .075, vcurve, $main_amp, [1, 1, 2, .1])
   bigbird(.3, .1, 4050, 1200, .15, ycurve, louder, [1, 1, 2, .1])
   bigbird(.42, .03, 3800, 500, .1, vcurve, $main_amp, [1, 1, 2, .1])
   bigbird(.5, .1, 4000, 1200, .2, ycurve, $bird_tap, [1, 1, 2, .1])
   bigbird(.65, .03, 3800, 500, .15, vcurve, $main_amp, [1, 1, 2, .1])
   bigbird(.72, .1, 4000, 1200, .2, ycurve, $bird_tap, [1, 1, 2, .1])
   bigbird(.85, .03, 3800, 500, .15, vcurve, $main_amp, [1, 1, 2, .1])
   bigbird(.91, .1, 4000, 1200, .2, ycurve, $bird_tap, [1, 1, 2, .1])
   bigbird(1.05, .12, 3800, 2200, .15, wcurve, wamp, [1, 1, 2, .1])
   bigbird(1.20, .12, 3800, 2200, .15, wcurve, wamp, [1, 1, 2, .1])
   bigbird(1.35, .12, 2500, 2200, .25, upcurve, louder, [1, 1, 2, .1])
   bigbird(1.50, .12, 2500, 4000, .15, downcurve, $main_amp, [1, 1, 2, .1])
	},
  report_in_minibuffer("chestnut_sided_warbler"))
end


def grasshopper_sparrow(beg)
  grassone = [.00, .50, .02, .80, .04, .30, .06, .80, .07, .10, .08, .90, .10, .00, .11, .90, .12, .00, .13, .90, .14, .10, .15, 1.00, .16, .10, .17, 1.00, .18, .10, .19, 1.00, .20, .10, .21, 1.00, .22, .10, .23, 1.00, .24, .10, .25, 1.00, .26, .10, .27, 1.00, .28, .10, .29, 1.00, .30, .10, .31, 1.00, .32, .10, .33, 1.00, .34, .10, .35, 1.00, .36, .10, .37, 1.00, .38, .10, .39, 1.00, .40, .10, .41, 1.00, .42, .10, .43, 1.00, .44, .10, .45, 1.00, .46, .10, .47, 1.00, .48, .10, .49, 1.00, .50, .10, .51, 1.00, .52, .10, .53, 1.00, .54, .10, .55, 1.00, .56, .10, .57, 1.00, .58, .10, .59, 1.00, .60, .10, .61, 1.00, .62, .10, .63, 1.00, .64, .10, .65, 1.00, .66, .10, .67, 1.00, .68, .10, .69, 1.00, .70, .10, .71, 1.00, .72, .10, .73, 1.00, .74, .10, .75, 1.00, .76, .10, .77, 1.00, .78, .10, .79, 1.00, .80, .10, .81, 1.00, .82, .10, .83, 1.00, .84, .10, .85, 1.00, .86, .10, .87, 1.00, .88, .10, .89, 1.00, .90, .10, .91, 1.00, .92, .10, .93, 1.00, .94, .10, .95, 1.00, .96, .10, .97, 1.00, .98, .10, 1.00, 1.0]
  grasstwo = [.00, .00, .10, 1.00, .20, .00, .30, 1.00, .40, .00, .50, 1.00, .60, .00, .70, 1.00, .80, .00, .90, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.49, .01, 8000, 100, .1, grasstwo, $main_amp)
   bird(.60, .01, 5700, 300, .1, grasstwo, $main_amp)
   bird(.92, .01, 3900, 100, .1, grasstwo, $main_amp)
   bird(1.00, 1.4, 6000, 2500, .2, grassone, $main_amp)
	},
  report_in_minibuffer("grasshopper_sparrow"))
end


def swamp_sparrow(beg)
  swamp_up = [.00, .00, .60, .70, 1.00, 1.0]
  swamp_down = [.00, 1.00, .50, .50, .60, .60, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(0, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.035, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.08, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.1, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.135, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.18, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.2, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.235, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.28, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.3, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.335, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.38, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.4, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.435, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.48, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.5, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.535, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.58, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.6, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.635, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.68, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.7, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.735, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.78, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.8, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.835, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.88, .025, 3700, 0, .1, $main_amp, $main_amp)

   bird(.9, .02, 3900, 200, .3, swamp_up, $main_amp)
   bird(.935, .035, 3200, 3000, .1, swamp_down, $main_amp)
   bird(.98, .025, 3700, 0, .1, $main_amp, $main_amp)
	},
  report_in_minibuffer("swamp_sparrow"))
end


def golden_crowned_sparrow(beg)
  goldone = [.00, 1.00, .25, .20, 1.00, .0]
  goldtwo = [.00, .90, .05, 1.00, .10, .40, 1.00, .0]
  goldtrill = [.00, .50, .10, .00, .20, 1.00, .30, .00, .40, 1.00, .50, .00, .60, 1.00, .70, .00, .80, 1.00, .90, .00, 1.00, .50]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.6, .5, 4300, 1000, .15, goldone, $main_amp)
   bird(1.3, .45, 3300, 200, .15, goldone, $main_amp)
   bird(1.75, .4, 3800, 100, .15, goldtwo, $main_amp)
   bird(2.2, .3, 3800, 100, .1, goldtrill, $main_amp)
	},
  report_in_minibuffer("golden_crowned_sparrow"))
end


def indigo_bunting(beg)
  buntdwn = [.00, 1.00, 1.00, .0]
  buntv = [.00, .00, .50, 1.00, 1.00, .0]
  bunty = [.00, 1.00, .50, .00, 1.00, .90]
  buntn = [.00, .80, .30, 1.00, .70, .20, 1.00, .0]
  buntx = [.00, 1.00, .10, .50, .25, .90, 1.00, .0]
  buntup = [.00, .00, 1.00, 1.0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.4, .08, 3000, 700, .25, buntdwn, $main_amp)
   bird(.52, .02, 6200, 1000, .05, buntdwn, $main_amp)
   bird(.55, .15, 3500, 2300, .1, buntv, $main_amp)
   bird(.74, .02, 6200, 1800, .05, buntx, $main_amp)
   bird(.80, .15, 3400, 2300, .1, buntv, $main_amp)
   bird(1.00, .1, 3400, 800, .2, buntv, $main_amp)
   bird(1.13, .03, 4100, 2000, .05, buntdwn, $main_amp)
   bird(1.25, .08, 3400, 800, .2, buntv, $main_amp)
   bird(1.40, .03, 4100, 2000, .05, buntdwn, $main_amp)
   bird(1.5, .07, 3700, 300, .1, buntdwn, $main_amp)
   bird(1.6, .1, 4100, 2200, .15, bunty, $main_amp)
   bird(1.72, .05, 3700, 300, .1, buntdwn, $main_amp)
   bird(1.81, .1, 4100, 2200, .15, bunty, $main_amp)
   bird(1.94, .07, 5200, 1800, .2, buntn, $main_amp)
   bird(2.05, .08, 3000, 1500, .15, buntup, $main_amp)
   bird(2.20, .07, 5200, 1800, .2, buntn, $main_amp)
   bird(2.33, .08, 3000, 1500, .15, buntup, $main_amp)
   bird(2.43, .07, 5200, 1800, .1, buntn, $main_amp)
   bird(2.51, .08, 3000, 1500, .10, buntup, $main_amp)
	},
  report_in_minibuffer("indigo_bunting"))
end


def hooded_warbler(beg)
  hoodup = [.00, .00, 1.00, 1.0]
  hooddown = [.00, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.6, .03, 3900, 1600, .05, hooddown, $main_amp)
   bird(.64, .03, 3900, 1700, .05, hooddown, $main_amp)
   bird(.8, .03, 3900, 2000, .10, hooddown, $main_amp)
   bird(.84, .03, 3900, 2000, .10, hooddown, $main_amp)
   bird(.93, .03, 3900, 2100, .15, hooddown, $main_amp)
   bird(.97, .03, 3900, 2100, .15, hooddown, $main_amp)
   bird(1.05, .03, 3900, 2100, .05, hooddown, $main_amp)
   bird(1.09, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.17, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.21, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.39, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.43, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.51, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.55, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.63, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.67, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.75, .03, 3900, 2100, .2, hooddown, $main_amp)
   bird(1.80, .03, 3900, 2100, .2, hooddown, $main_amp)

   bird(1.90, .04, 3000, 1000, .15, hoodup, $main_amp)
   bird(1.98, .04, 3000, 1000, .15, hoodup, $main_amp)
   bird(2.05, .04, 3000, 1000, .15, hoodup, $main_amp)
   bird(2.13, .04, 3000, 1000, .15, hoodup, $main_amp)
   bird(2.21, .04, 3000, 1000, .15, hoodup, $main_amp)
   bird(2.29, .04, 3000, 1000, .15, hoodup, $main_amp)
   bird(2.37, .04, 3000, 1000, .15, hoodup, $main_amp)
   bird(2.45, .04, 3000, 1000, .15, hoodup, $main_amp)
	},
  report_in_minibuffer("hooded_warbler"))
end



def american_widgeon(beg)
  widgeon = [.00, .00, .50, 1.00, 1.00, .0]
  
  one_bird(beg, 1.0,
  Proc.new {||
   bigbird(.3, .07, 1900, 300, .15, widgeon, widgeon, [1, 1, 2, .02])
   bigbird(.4, .11, 1700, 1400, .25, widgeon, widgeon, [1, .7, 2, 1, 3, .02])
   bigbird(.55, .07, 1900, 300, .15, widgeon, widgeon, [1, 1, 2, .02])
	},
  report_in_minibuffer("american_widgeon"))
end


def louisiana_waterthrush(beg)
  water_one = [.00, .80, .35, .40, .45, .90, .50, 1.00, .75, 1.00, 1.00, .10]
  water_two = [.00, 1.00, .40, .00, .60, .10, 1.00, .80]
  water_three = [.00, 1.00, .95, .00, 1.00, .0]
  water_four = [.00, .00, 1.00, 1.0]
  water_five = [.00, 1.00, 1.00, .0]
  water_amp = [.00, .00, .35, 1.00, .50, .20, .90, 1.00, 1.00, .0]
  water_damp = [.00, .00, .90, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(0, .17, 4100, 2000, .2, water_one, water_amp)
   bird(.32, .18, 4050, 2050, .3, water_one, water_amp)
   bird(.64, .20, 4000, 1900, .25, water_one, water_amp)
   bird(.9, .2, 3900, 2000, .3, water_two, $bird_tap)
   bird(1.25, .12, 3000, 3000, .25, water_three, water_damp)
   bird(1.4, .1, 2700, 1500, .2, water_four, water_damp)
   bird(1.58, .02, 5200, 1000, .1, water_five, $main_amp)
   bird(1.65, .02, 5200, 1000, .1, water_five, $main_amp)
   bird(1.7, .035, 3200, 1000, .1, water_four, water_damp)
	},
  report_in_minibuffer("louisiana_waterthrush"))
end


def robin(beg)
  r_one = [.00, .10, .08, .70, .30, .00, .35, 1.00, .40, .30, 1.00, .30]
  r_two = [.00, .00, .10, 1.00, .20, .70, .35, .70, .65, .30, .70, .50, .80, .00, .90, .20, 1.00, .0]
  r_three = [.00, .20, .25, 1.00, .60, .70, .90, .00, 1.00, .10]
  r_four = [.00, 1.00, 1.00, .0]
  r_five = [.00, .50, .10, .00, .20, 1.00, .30, .00, .40, 1.00, .50, .00, .60, 1.00, .70, .50, 1.00, .20]
  r_six = [.00, .00, .12, .70, .30, .00, .70, 1.00, 1.00, .50]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bigbird(.45, .06, 2000, 800, .15, r_six, $main_amp, [1, 1, 2, .1])
   bigbird(.56, .10, 2000, 900, .15, r_one, $main_amp, [1, 1, 2, .1])
   bigbird(1.04, .24, 2000, 2000, .25, r_two, $main_amp, [1, 1, 2, .1])
   bigbird(1.63, .13, 1900, 1600, .20, r_three, $main_amp, [1, 1, 2, .1])
   bigbird(1.80, .11, 2200, 1200, .25, r_four, $main_amp, [1, 1, 2, .1])
   bigbird(2.31, .21, 1950, 2000, .15, r_five, $main_amp, [1, 1, 2, .1])
	},
  report_in_minibuffer("robin"))
end


def solitary_vireo(beg) 
  bigskew = [.00, .20, .03, .30, .06, .10, .10, .50, .13, .40, .16, .80, .19, .50, .22, .90, .25, .60, .28, 1.00, .31, .60, .34, 1.00, .37, .50, .41, .90, .45, .40, .49, .80, .51, .40, .54, .75, .57, .35, .60, .70, .63, .30, .66, .60, .69, .25, .72, .50, .75, .20, .78, .30, .82, .10, .85, .30, .88, .05, .91, .30, .94, .00, .95, .30, .99, .00, 1.00, .10]
  one_bird(beg, 1.0,
  Proc.new {||
   bird(0, .4, 1800, 1200, .2, bigskew, $main_amp)
	},
  report_in_minibuffer("solitary_vireo"))
end


def pigeon_hawk(beg)
  hupdown = [.00, .00, .30, 1.00, .70, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bigbird(0, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(.12, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(.13, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(.25, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(.26, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(.38, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(.39, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(.51, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(.52, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(.64, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(.65, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(.77, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(.78, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(.90, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(.91, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(1.03, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(1.04, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(1.16, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(1.17, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(1.29, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(1.30, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(1.42, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(1.43, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(1.55, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(1.56, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(1.68, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(1.69, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
   bigbird(1.81, .01, 2050, 0, .1, $main_amp, $main_amp, [1, .5, 2, 1])
   bigbird(1.82, .1, 1900, 200, .2, hupdown, $main_amp, [1, .7, 2, 1])
	},
  report_in_minibuffer("pigeon_hawk"))
end


def cerulean_warbler(beg)
  w_down = [.00, 1.00, 1.00, .0]
  trill = [.00, .80, .10, 1.00, .25, .50, .40, 1.00, .55, .50, .70, 1.00, 1.00, .0]
  w_up = [.00, .00, 1.00, 1.0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(.27, .05, 3000, 1000, .05, w_down, $main_amp)
   bird(.33, .05, 3000, 800, .075, w_up, $main_amp)
   bird(.41, .01, 3200, 700, .07, w_down, $main_amp)
   bird(.42, .01, 3200, 700, .08, w_down, $main_amp)
   bird(.43, .06, 3200, 700, .09, w_down, $main_amp)
   bird(.51, .06, 3200, 500, .1, w_up, $main_amp)
   bird(.6, .10, 3000, 1200, .2, trill, $main_amp)
   bird(.72, .05, 3000, 800, .2, w_up, $main_amp)
   bird(.8, .10, 3000, 1200, .2, trill, $main_amp)
   bird(.92, .05, 3000, 800, .2, w_up, $main_amp)
   bird(1.00, .01, 3900, 600, .1, w_up, $main_amp)
   bird(1.01, .01, 3910, 800, .1, w_up, $main_amp)
   bird(1.02, .01, 3940, 500, .1, w_up, $main_amp)
   bird(1.03, .01, 4000, 500, .1, w_up, $main_amp)
   bird(1.04, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.05, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.06, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.07, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.08, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.09, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.10, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.11, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.12, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.13, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.14, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.15, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.16, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.17, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.18, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.19, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.20, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.21, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.22, .01, 3900, 1000, .1, w_up, $main_amp)
   bird(1.23, .01, 3900, 1200, .1, w_up, $main_amp)
   bird(1.24, .01, 3900, 1200, .1, w_up, $main_amp)
   bird(1.25, .01, 3900, 1200, .1, w_up, $main_amp)
   bird(1.26, .01, 3900, 1200, .1, w_up, $main_amp)
   bird(1.27, .01, 3900, 1400, .1, w_up, $main_amp)
   bird(1.28, .01, 3900, 1400, .1, w_up, $main_amp)
   bird(1.29, .01, 3900, 1400, .1, w_up, $main_amp)
   bird(1.30, .01, 3900, 1400, .1, w_up, $main_amp)
	},
  report_in_minibuffer("cerulean_warbler"))
end


def nashville_warbler(beg)
  nash_blip = [.00, .60, .35, 1.00, 1.00, .0]
  nash_down = [.00, .90, .05, 1.00, .10, .90, .65, .50, 1.00, .0]
  nash_up = [.00, .00, .15, .20, .25, .05, .90, .95, 1.00, 1.0]
  nash_amp = [.00, .00, .80, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(.15, .025, 3900, 300, .3, nash_blip, $main_amp)
   bird(.24, .16, 4200, 3800, .15, nash_down, nash_amp)
   bird(.42, .025, 3900, 300, .3, nash_blip, $main_amp)
   bird(.55, .14, 4300, 3700, .15, nash_down, nash_amp)
   bird(.75, .03, 3950, 350, .3, nash_blip, $main_amp)
   bird(.81, .17, 4200, 3900, .175, nash_down, $main_amp)
   bird(1.0, .02, 3800, 400, .25, nash_blip, $main_amp)
   bird(1.11, .14, 4200, 3800, .165, nash_down, nash_amp)
   bird(1.3, .03, 3750, 300, .2, nash_blip, $main_amp)
   bird(1.4, .11, 4200, 3700, .1, nash_down, $main_amp)
   bird(1.57, .1, 3800, 2200, .1, nash_up, $main_amp)
   bird(1.7, .1, 3800, 2150, .125, nash_up, $main_amp)
   bird(1.85, .075, 3900, 1800, .1, nash_up, nash_amp)
	},
  report_in_minibuffer("nashville_warbler"))
end


def eastern_phoebe(beg)
  phoebe_one = [.00, .00, .30, .30, .35, .50, .55, .40, .70, .80, .75, .70, .80, 1.00, .95, .90, 1.00, .0]
  phoebe_two = [.00, .00, .50, 1.00, 1.00, .0]
  phoebe_three = [.00, .00, .10, .40, .80, 1.00, 1.00, .10]
  phoebe_four = [.00, 1.00, .50, .70, 1.00, .0]
  phoebe_amp = [.00, .00, .10, 1.00, 1.00, .0]
  
  one_bird(beg, 1.0,
  Proc.new {||
   bird(0, .225, 3000, 1300, .3, phoebe_one, $main_amp)
   bird(.35, .12, 3000, 500, .1, phoebe_two, phoebe_amp)
   bird(.4, .10, 3000, 1500, .2, phoebe_three, phoebe_amp)
   bird(.55, .05, 3000, 1400, .2, phoebe_four, phoebe_amp)
	},
  report_in_minibuffer("eastern_phoebe"))
end


def painted_bunting(beg)
  b_one = [.00, .00, 1.00, 1.0]
  b_two = [.00, .00, .90, 1.00, 1.00, .0]
  b_three = [.00, 1.00, 1.00, .0]
  b_four = [.00, .00, .50, 1.00, 1.00, .0]
  b_five = [.00, .70, .15, .00, .40, 1.00, .80, 1.00, 1.00, .50]
  b_six = [.00, .00, .10, .50, .15, .00, .40, 1.00, .90, 1.00, 1.00, .0]
  b_seven = [.00, 1.00, .25, .40, .75, .50, 1.00, .0]
  b_eight = [.00, .30, .40, .40, .50, 1.00, .60, .20, 1.00, .0]
  b_nine = [.00, .00, .05, 1.00, .30, 1.00, .50, .30, .90, 1.00, 1.00, .0]
  b_ten = [.00, .40, .25, .00, .35, 1.00, .50, .00, .65, 1.00, .75, .00, .85, 1.00, 1.00, .0]
  b_eleven = [.00, 1.00, 1.00, .0]
  b_twelve = [.00, .00, .50, 1.00, 1.00, .50]
  b_thirteen = [.00, .00, .05, 1.00, .30, .20, .60, .20, .90, 1.00, 1.00, .0]
  b_fourteen = [.00, .30, .30, 1.00, .60, .30, 1.00, .0]
  b_fifteen = [.00, .00, .10, .50, .50, .50, .90, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(.05, .10, 3100, 900, .05, b_one, b_two)
   bird(.21, .07, 4100, 700, .15, b_three, $main_amp)
   bird(.36, .12, 3700, 1000, .20, b_four, $main_amp)
   bird(.52, .08, 2300, 1600, .15, b_five, b_six)
   bird(.68, .1, 4000, 1000, .25, b_one, $bird_tap)
   bird(.8, .12, 2300, 1700, .2, b_seven, $main_amp)
   bird(.96, .15, 3800, 2200, .3, b_eight, b_nine)
   bird(1.18, .1, 2300, 1600, .15, b_ten, $main_amp)
   bird(1.3, .02, 3200, 1000, .1, b_eleven, $main_amp)
   bird(1.33, .02, 3200, 1000, .1, b_eleven, $main_amp)
   bird(1.36, .02, 3200, 1000, .1, b_eleven, $main_amp)
   bird(1.40, .03, 4000, 2000, .12, b_twelve, b_thirteen)
   bird(1.47, .1, 2300, 1700, .2, b_fourteen, b_fifteen)
	},
  report_in_minibuffer("painted_bunting"))
end


def western_flycatcher(beg)
  f_one = [.00, .00, .10, 1.00, .20, .40, .95, .10, 1.00, .0]
  a_one = [.00, .00, .10, .20, .20, .10, .30, 1.00, .90, 1.00, 1.00, .0]
  f_two = [.00, .50, .25, 1.00, .50, .00, .60, .00, .95, .30, 1.00, .60]
  a_two = [.00, .00, .10, 1.00, .20, 1.00, .50, .10, .60, .10, .90, 1.00, 1.00, .0]
  
  one_bird(beg, 1.0,
  Proc.new {||
   bigbird(0, .2, 2000, 2200, .2, f_one, a_one, [1, 1, 2, .02, 3, .1, 4, .01])
   bigbird(.3, .2, 2000, 1100, .2, f_two, a_two, [1, 1, 2, .02, 3, .1, 4, .01])
	},
  report_in_minibuffer("western_flycatcher"))
end


def bachmans_sparrow(beg)
  sopening = [.00, 1.00, .10, .50, .90, .50, 1.00, .0]
  sup = [.00, .10, .35, .00, 1.00, 1.0]
  sdwn = [.00, 1.00, .40, .50, 1.00, .0]
  supn = [.00, .00, 1.00, 1.0]
  slast = [.00, 1.00, .25, .00, .75, .40, 1.00, .50]
  
  one_bird(beg, 5.0,
  Proc.new {||
   bird(0, .51, 4900, 200, .3, sopening, $main_amp)
   bird(.52, .015, 3800, 200, .1, sup, $main_amp)
   bird(.52, .015, 3750, 250, .1, sup, $main_amp)
   bird(.54, .015, 3600, 300, .1, sup, $main_amp)
   bird(.56, .015, 3500, 250, .1, sup, $main_amp)
   bird(.58, .015, 3400, 200, .1, sup, $main_amp)
   bird(.60, .015, 3200, 200, .1, sup, $main_amp)
   bird(.62, .015, 3800, 100, .1, sup, $main_amp)

   bird(.65, .07, 3000, 750, .2, sup, $main_amp)
   bird(.73, .03, 5000, 1000, .1, sdwn, $main_amp)
   bird(.80, .07, 3000, 750, .2, sup, $main_amp)
   bird(.88, .03, 5000, 1000, .1, sdwn, $main_amp)
   bird(.95, .07, 3000, 750, .2, sup, $main_amp)
   bird(1.03, .03, 5000, 1000, .1, sdwn, $main_amp)
   bird(1.10, .07, 3000, 750, .2, sup, $main_amp)
   bird(1.18, .03, 5000, 1000, .1, sdwn, $main_amp)
   bird(1.25, .07, 3000, 750, .2, sup, $main_amp)
   bird(1.33, .03, 5000, 1000, .1, sdwn, $main_amp)
   bird(1.40, .07, 3000, 750, .2, sup, $main_amp)
   bird(1.48, .03, 5000, 1000, .1, sdwn, $main_amp)
   bird(1.55, .07, 3000, 750, .2, sup, $main_amp)
   bird(1.63, .03, 5000, 1000, .1, sdwn, $main_amp)

   bird(2.8, .06, 4000, 1700, .1, supn, $main_amp)
   bird(2.87, .01, 5200, 0, .2, supn, $main_amp)
   bird(2.9, .06, 4000, 1700, .1, supn, $main_amp)
   bird(2.97, .01, 5200, 0, .2, supn, $main_amp)
   bird(3.0, .06, 4000, 1700, .1, supn, $main_amp)
   bird(3.07, .01, 5200, 0, .2, supn, $main_amp)
   bird(3.1, .06, 4000, 1700, .1, supn, $main_amp)
   bird(3.17, .01, 5200, 0, .2, supn, $main_amp)
   bird(3.2, .06, 4000, 1700, .1, supn, $main_amp)
   bird(3.27, .01, 5200, 0, .2, supn, $main_amp)

   bird(3.4, .15, 3000, 1000, .2, slast, $main_amp)
   bird(3.6, .15, 3000, 1000, .2, slast, $main_amp)
   bird(3.8, .15, 3000, 1000, .2, slast, $main_amp)
   bird(4.0, .15, 3000, 1000, .2, slast, $main_amp)
   bird(4.2, .15, 3000, 1000, .2, slast, $main_amp)
   bird(4.4, .15, 3000, 1000, .2, slast, $main_amp)
	},
  report_in_minibuffer("bachmans_sparrow"))
end


def cedar_waxwing(beg)
  cedar = [.00, .00, .25, .70, .70, 1.00, .90, 1.00, 1.00, .20]
  cedamp = [.00, .00, .20, 1.00, .40, 1.00, 1.00, .0]

  one_bird(beg, 1.0,
  Proc.new {||
   bird(0, .50, 6000, 800, .2, cedar, cedamp)
	},
  report_in_minibuffer("cedar_waxwing"))
end


def bairds_sparrow(beg)
  bairdend = [.00, .00, .25, 1.00, .50, .00, .75, 1.00, 1.00, .0]
  bairdstart = [.00, .50, .05, 1.00, .10, .00, .15, 1.00, .20, .00, .25, 1.00, .30, .00, .35, 1.00, .40, .00, .45, 1.00, .50, .00, .55, 1.00, .60, .00, .65, 1.00, .70, .00, .75, 1.00, .80, .00, .85, 1.00, .90, .00, .95, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bird(0, .09, 6500, 1500, .2, bairdstart, $main_amp)
   bird(.22, .01, 5900, 100, .2, bairdend, $main_amp)
   bird(.25, .09, 6000, 1000, .2, bairdstart, $main_amp)
   bird(.45, .01, 4200, 100, .2, bairdend, $main_amp)
   bird(.50, .08, 4200, 600, .2, bairdstart, $main_amp)
   bird(.59, .01, 4400, 100, .2, bairdend, $main_amp)
   bird(.60, .01, 4400, 100, .2, bairdend, $main_amp)
   bird(.68, .07, 5400, 700, .2, bairdstart, $main_amp)

   bird(.75, .01, 4200, 100, .2, bairdend, $main_amp)
   bird(.79, .01, 4400, 100, .2, bairdend, $main_amp)
   bird(.83, .01, 4200, 100, .19, bairdend, $main_amp)
   bird(.87, .01, 4400, 100, .19, bairdend, $main_amp)
   bird(.91, .01, 4200, 100, .18, bairdend, $main_amp)
   bird(.95, .01, 4400, 100, .18, bairdend, $main_amp)
   bird(.99, .01, 4200, 100, .17, bairdend, $main_amp)
   bird(1.03, .01, 4400, 100, .17, bairdend, $main_amp)
   bird(1.07, .01, 4200, 100, .16, bairdend, $main_amp)
   bird(1.11, .01, 4400, 100, .16, bairdend, $main_amp)
   bird(1.15, .01, 4200, 100, .15, bairdend, $main_amp)
   bird(1.19, .01, 4400, 100, .15, bairdend, $main_amp)
   bird(1.23, .01, 4200, 100, .14, bairdend, $main_amp)
   bird(1.27, .01, 4400, 100, .14, bairdend, $main_amp)
   bird(1.31, .01, 4200, 100, .13, bairdend, $main_amp)
   bird(1.35, .01, 4400, 100, .13, bairdend, $main_amp)
   bird(1.39, .01, 4200, 100, .12, bairdend, $main_amp)
   bird(1.43, .01, 4400, 100, .12, bairdend, $main_amp)
   bird(1.47, .01, 4200, 100, .11, bairdend, $main_amp)
   bird(1.51, .01, 4400, 100, .11, bairdend, $main_amp)
   bird(1.55, .01, 4200, 100, .10, bairdend, $main_amp)
   bird(1.59, .01, 4400, 100, .10, bairdend, $main_amp)
   bird(1.63, .01, 4200, 100, .09, bairdend, $main_amp)
   bird(1.67, .01, 4400, 100, .09, bairdend, $main_amp)
   bird(1.71, .01, 4200, 100, .08, bairdend, $main_amp)
   bird(1.75, .01, 4400, 100, .08, bairdend, $main_amp)
   bird(1.79, .01, 4200, 100, .07, bairdend, $main_amp)
   bird(1.83, .01, 4400, 100, .07, bairdend, $main_amp)
   bird(1.87, .01, 4200, 100, .06, bairdend, $main_amp)
   bird(1.92, .01, 4400, 100, .06, bairdend, $main_amp)
   bird(1.97, .01, 4200, 100, .05, bairdend, $main_amp)
	},
  report_in_minibuffer("bairds_sparrow"))
end


def kentucky_warbler(beg)
  kenstart = [.00, .30, .50, 1.00, 1.00, .0]
  kendwn = [.00, .90, .10, 1.00, 1.00, .0]
  kenup = [.00, .00, 1.00, 1.0]
  kentrill = [.00, 1.00, .25, .00, .50, .00, .75, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bigbird(.6, .02, 3800, 200, .05, kenstart, $main_amp, [1, 1, 2, .03])
   bigbird(.65, .03, 4300, 200, .15, kenup, $main_amp, [1, 1, 2, .1])
   bigbird(.73, .02, 3200, 100, .1, kendwn, $main_amp, [1, 1, 2, .1])

   bigbird(.75, .05, 3000, 800, .15, kenstart, $main_amp, [1, 1, 2, .01])
   bigbird(.82, .06, 3100, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(.90, .06, 3200, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(.98, .05, 4600, 100, .2, kentrill, $main_amp, [1, 1, 2, .1])

   bigbird(1.10, .05, 2900, 800, .15, kenstart, $main_amp, [1, 1, 2, .01])
   bigbird(1.17, .06, 3000, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(1.25, .06, 3100, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(1.33, .05, 4600, 100, .2, kentrill, $main_amp, [1, 1, 2, .1])

   bigbird(1.43, .05, 2800, 800, .15, kenstart, $main_amp, [1, 1, 2, .01])
   bigbird(1.50, .05, 2700, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(1.57, .06, 2800, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(1.64, .05, 4600, 100, .2, kentrill, $main_amp, [1, 1, 2, .1])

   bigbird(1.75, .05, 2700, 800, .15, kenstart, $main_amp, [1, 1, 2, .01])
   bigbird(1.81, .05, 2600, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(1.88, .06, 2600, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(1.97, .05, 4600, 100, .2, kentrill, $main_amp, [1, 1, 2, .1])

   bigbird(2.05, .05, 2700, 800, .15, kenstart, $main_amp, [1, 1, 2, .01])
   bigbird(2.12, .06, 2600, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(2.20, .05, 4600, 100, .2, kentrill, $main_amp, [1, 1, 2, .1])

   bigbird(2.30, .05, 2800, 800, .15, kenstart, $main_amp, [1, 1, 2, .01])
   bigbird(2.37, .06, 2700, 1200, .1, kendwn, $main_amp, [1, 1, 2, .01])
   bigbird(2.45, .05, 4700, 100, .25, kentrill, $main_amp, [1, 1, 2, .1])
	},
  report_in_minibuffer("kentucky_warbler"))
end


def rufous_sided_towhee(beg)
  towhee_one = [.00, .10, .02, .05, .04, .15, .06, .05, .08, .20, .10, .04, .12, .25, .14, .03, .16, .30, .18, .02, .20, .35, .22, .01, .24, .40, .26, .00, .28, .45, .30, .00, .32, .50, .34, .00, .36, .50, .80, 1.00, 1.00, .0]
  towhee_two = [.00, .00, 1.00, 1.0]
  towhee_three = [.00, 1.00, 1.00, .0]
  
  one_bird(beg, 2.0,
  Proc.new {||
   bigbird(.25, .13, 1400, 1100, .2, towhee_one, $main_amp, [1, .03, 2, 1, 3, .03])
   bigbird(.45, .13, 1400, 1100, .2, towhee_one, $main_amp, [1, .03, 2, 1, 3, .03])
   bigbird(.60, .13, 1400, 1100, .2, towhee_one, $main_amp, [1, .03, 2, 1, 3, .03])
   bigbird(.75, .10, 1400, 1100, .2, towhee_one, $main_amp, [1, .03, 2, 1, 3, .03])

   bird(.88, .01, 5100, 2000, .1, towhee_two, $main_amp)
   bird(.895, .01, 5100, 1600, .1, towhee_two, $main_amp)
   bird(.91, .01, 5100, 1000, .1, towhee_two, $main_amp)
   bird(.93, .01, 3000, 1200, .1, towhee_three, $main_amp)

   bird(.945, .01, 5100, 2000, .09, towhee_two, $main_amp)
   bird(.96, .01, 5100, 1600, .09, towhee_two, $main_amp)
   bird(.975, .01, 5100, 1000, .09, towhee_two, $main_amp)
   bird(.995, .01, 3000, 1200, .09, towhee_three, $main_amp)

   bird(1.01, .01, 5100, 2000, .1, towhee_two, $main_amp)
   bird(1.025, .01, 5100, 1600, .1, towhee_two, $main_amp)
   bird(1.04, .01, 5100, 1000, .1, towhee_two, $main_amp)
   bird(1.06, .01, 3000, 1200, .1, towhee_three, $main_amp)

   bird(1.075, .01, 5100, 2000, .09, towhee_two, $main_amp)
   bird(1.09, .01, 5100, 1600, .09, towhee_two, $main_amp)
   bird(1.105, .01, 5100, 1000, .09, towhee_two, $main_amp)
   bird(1.125, .01, 3000, 1200, .09, towhee_three, $main_amp)

   bird(1.14, .01, 5100, 2000, .08, towhee_two, $main_amp)
   bird(1.155, .01, 5100, 1600, .08, towhee_two, $main_amp)
   bird(1.17, .01, 5100, 1000, .08, towhee_two, $main_amp)
   bird(1.19, .01, 3000, 1200, .08, towhee_three, $main_amp)

   bird(1.205, .01, 5100, 2000, .08, towhee_two, $main_amp)
   bird(1.220, .01, 5100, 1600, .08, towhee_two, $main_amp)
   bird(1.235, .01, 5100, 1000, .08, towhee_two, $main_amp)
   bird(1.255, .01, 3000, 1200, .08, towhee_three, $main_amp)

   bird(1.27, .01, 5100, 2000, .07, towhee_two, $main_amp)
   bird(1.285, .01, 5100, 1600, .07, towhee_two, $main_amp)
   bird(1.30, .01, 5100, 1000, .07, towhee_two, $main_amp)
   bird(1.32, .01, 3000, 1200, .07, towhee_three, $main_amp)

   bird(1.335, .01, 5100, 2000, .06, towhee_two, $main_amp)
   bird(1.350, .01, 5100, 1600, .06, towhee_two, $main_amp)
   bird(1.365, .01, 5100, 1000, .06, towhee_two, $main_amp)
   bird(1.385, .01, 3000, 1200, .06, towhee_three, $main_amp)

   bird(1.400, .01, 5100, 2000, .05, towhee_two, $main_amp)
   bird(1.415, .01, 5100, 1600, .05, towhee_two, $main_amp)
   bird(1.430, .01, 5100, 1000, .05, towhee_two, $main_amp)
   bird(1.45, .01, 3000, 1200, .05, towhee_three, $main_amp)

   bird(1.465, .01, 5100, 2000, .03, towhee_two, $main_amp)
   bird(1.480, .01, 5100, 1600, .03, towhee_two, $main_amp)
   bird(1.495, .01, 5100, 1000, .03, towhee_two, $main_amp)
   bird(1.515, .01, 3000, 1200, .03, towhee_three, $main_amp)
	},
  report_in_minibuffer("rufous_sided_towhee"))
end


def prothonotary_warbler(beg)
  pro_one = [.00, .10, .20, .00, 1.00, 1.0]
  pro_two = [.00, .00, 1.00, 1.0]
  pro_amp = [.00, .00, .20, 1.00, .40, .50, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.76, .08, 3000, 3000, .05, pro_one, pro_amp)
   bird(.85, .05, 4000, 2500, .06, pro_two, $bird_amp)

   bird(1.02, .09, 3000, 3000, .10, pro_one, pro_amp)
   bird(1.12, .05, 4000, 2500, .10, pro_two, $bird_amp)

   bird(1.26, .08, 3000, 3000, .15, pro_one, pro_amp)
   bird(1.35, .05, 4000, 2500, .16, pro_two, $bird_amp)

   bird(1.54, .08, 3000, 3000, .20, pro_one, pro_amp)
   bird(1.63, .05, 4000, 2500, .19, pro_two, $bird_amp)

   bird(1.80, .08, 3000, 3000, .20, pro_one, pro_amp)
   bird(1.89, .05, 4000, 2500, .16, pro_two, $bird_amp)

   bird(2.03, .08, 3000, 3000, .15, pro_one, pro_amp)
   bird(2.12, .05, 4000, 2500, .10, pro_two, $bird_amp)

   bird(2.30, .08, 3000, 3000, .10, pro_one, pro_amp)
   bird(2.39, .05, 4000, 2500, .06, pro_two, $bird_amp)
	},
  report_in_minibuffer("prothonotary_warbler"))
end


def audubons_warbler(beg)
  w_up = [.00, .00, 1.00, 1.0]
  w_down = [.00, 1.00, 1.00, .0]
  w_end = [.00, .00, .15, 1.00, .45, .90, .50, .00, .55, 1.00, .90, .90, 1.00, .10]
  w_updown = [.00, .10, .50, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.75, .04, 2400, 200, .05, w_down, $bird_amp)
   bird(.83, .03, 3200, 200, .1, w_up, $bird_amp)
   bird(.90, .04, 2500, 300, .15, w_up, $bird_amp)
   bird(.97, .04, 2300, 600, .15, w_down, $bird_amp)
   bird(1.02, .03, 3500, 400, .20, w_up, $bird_amp)
   bird(1.06, .04, 2300, 1200, .10, w_up, $bird_amp)
   bird(1.13, .05, 2300, 1200, .15, w_down, $bird_amp)
   bird(1.22, .02, 3200, 800, .25, w_up, $bird_amp)
   bird(1.25, .08, 2400, 600, .20, w_updown, $bird_amp)
   bird(1.35, .02, 2200, 400, .10, w_up, $bird_amp)
   bird(1.38, .07, 2400, 1400, .15, w_down, $bird_amp)
   bird(1.47, .03, 3000, 800, .20, w_up, $bird_amp)
   bird(1.50, .03, 2500, 400, .10, w_updown, $bird_amp)
   bird(1.55, .01, 2300, 100, .05, w_up, $bird_amp)
   bird(1.56, .06, 2200, 1400, .15, w_down, $bird_amp)
   bird(1.65, .03, 3100, 800, .10, w_up, $bird_amp)
   bird(1.70, .07, 2800, 800, .15, w_updown, $bird_amp)
   bird(1.79, .06, 2400, 1000, .10, w_down, $bird_amp)
   bird(1.86, .14, 3100, 900, .25, w_end, $bird_amp)
   bird(2.02, .12, 3200, 800, .20, w_end, $bird_amp)
	},
  report_in_minibuffer("audubons_warbler"))
end


def lark_bunting(beg)
  b_down = [.00, 1.00, 1.00, .0]
  b_up = [.00, .00, 1.00, 1.0]
  b_trill_one = [.00, .00, .06, .80, .12, .00, .18, .85, .24, .05, .36, .90, .42, .10, .48, .95, .54, .20, .60, 1.00, .66, .20, .72, 1.00, .78, .20, .84, 1.00, .90, .20, 1.00, 1.0]
  b_trill_two = [.00, .00, .05, .80, .10, .00, .15, .85, .20, .00, .25, .90, .30, .00, .35, .95, .40, .00, .45, 1.00, .50, .00, .55, 1.00, .60, .00, .65, 1.00, .70, .00, .75, 1.00, .80, .00, .85, 1.00, .90, .00, .95, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.1, .03, 1800, 100, .1, b_up, $bird_amp)
   bird(.2, .12, 3700, 400, .2, b_up, $bird_amp)

   bird(.4, .03, 4100, 500, .15, b_down, $bird_amp)
   bird(.45, .05, 2000, 400, .20, b_down, $bird_amp)
   bird(.51, .03, 1800, 100, .1, b_up, $bird_amp)

   bird(.6, .03, 4100, 500, .15, b_down, $bird_amp)
   bird(.65, .05, 2000, 400, .20, b_down, $bird_amp)
   bird(.71, .03, 1800, 100, .1, b_up, $bird_amp)

   bird(.8, .03, 4100, 500, .15, b_down, $bird_amp)
   bird(.85, .05, 2000, 400, .20, b_down, $bird_amp)
   bird(.91, .03, 1800, 100, .1, b_up, $bird_amp)

   bird(1.0, .03, 4100, 500, .15, b_down, $bird_amp)
   bird(1.05, .05, 2000, 400, .20, b_down, $bird_amp)
   bird(1.11, .03, 1800, 100, .1, b_up, $bird_amp)

   bird(1.2, .03, 4100, 500, .15, b_down, $bird_amp)
   bird(1.25, .05, 2000, 400, .20, b_down, $bird_amp)
   bird(1.31, .03, 1800, 100, .1, b_up, $bird_amp)

   bird(1.4, .03, 4100, 500, .15, b_down, $bird_amp)
   bird(1.45, .05, 2000, 400, .20, b_down, $bird_amp)
   bird(1.51, .03, 1800, 100, .1, b_up, $bird_amp)

   bird(1.6, .03, 4100, 500, .15, b_down, $bird_amp)
   bird(1.65, .05, 2000, 400, .20, b_down, $bird_amp)
   bird(1.71, .03, 1800, 100, .1, b_up, $bird_amp)

   bird(1.77, .23, 6000, 600, .15, b_trill_one, $bird_amp)
   bird(2.005, .28, 6000, 600, .15, b_trill_two, $bird_amp)
	},
  report_in_minibuffer("lark_bunting"))
end


def eastern_bluebird(beg)
  blue_one = [.00, .00, 1.00, 1.0]
  blue_two = [.00, 1.00, 1.00, .0]
  blue_three = [.00, .60, .10, 1.00, .20, .00, .25, 1.00, .30, .00, .35, 1.00, .40, .00, .45, 1.00, .50, .00, .75, 1.00, 1.00, .0]
  blue_four = [.00, .00, .50, 1.00, 1.00, .0]
  blue_five = [.00, .50, .10, 1.00, .20, .00, .35, 1.00, .50, .00, .65, 1.00, .80, .00, .95, 1.00, 1.00, .50]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.75, .02, 2000, 1600, .1, blue_one, $bird_amp)
   bird(.80, .02, 2000, 1600, .1, blue_one, $bird_amp)
   bird(.86, .02, 2000, 1600, .1, blue_one, $bird_amp)
   bird(1.00, .13, 2000, 1400, .2, blue_two, $bird_amp)
   bird(1.20, .24, 2000, 800, .2, blue_three, $bird_amp)
   bird(1.68, .03, 2200, 400, .1, blue_one, $bird_amp)
   bird(1.72, .10, 1950, 100, .15, blue_four, $bird_amp)
   bird(1.96, .15, 2000, 600, .20, blue_five, $bird_amp)
	},
  report_in_minibuffer("eastern_bluebird"))
end


def chuck_wills_widow(beg)
  wid_down = [.00, 1.00, 1.00, .0]
  wid_one = [.00, .00, .10, .10, .25, 1.00, .50, .30, .80, .70, 1.00, .0]
  wid_two = [.00, .20, .30, 1.00, .50, .30, .60, .70, .90, .10, 1.00, .0]
  
  one_bird(beg, 1.0,
  Proc.new {||
   bird(.05, .03, 1000, 800, .1, wid_down, $bird_amp)
   bird(.32, .20, 1000, 1000, .2, wid_one, $bird_amp)
   bird(.56, .29, 900, 1100, .2, wid_two, $bird_amp)
	},
  report_in_minibuffer("chuck_wills_widow"))
end


def blue_gray_gnatcatcher(beg)	
  gskw1 = [.00, .00, .15, 1.00, .75, .80, .90, 1.00, 1.00, .70]
  gskw2 = [.00, .00, .25, 1.00, .75, .70, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bigbird(.5, .20, 4000, 1000, .2, gskw1, $bird_amp, [1, .4, 2, 1, 3, .1])
   bigbird(.8, .13, 4000, 800, .2, gskw2, $bird_amp, [1, .4, 2, 1, 3, .2])

   bigbird(1.4, .25, 4000, 800, .2, gskw2, $bird_amp, [1, .4, 2, 1, 3, .3])
   bigbird(1.80, .17, 4000, 900, .2, gskw1, $bird_amp, [1, .4, 2, 1, 3, .3])
   bigbird(2.00, .17, 4000, 700, .2, gskw1, $bird_amp, [1, .4, 2, 1, 3, .3])
   bigbird(2.20, .17, 4000, 800, .2, gskw2, $bird_amp, [1, .4, 2, 1, 3, .3])
	},
  report_in_minibuffer("blue_gray_gnatcatcher"	))
end


def black_throated_sparrow(beg)
  black_up = [.00, .00, 1.00, 1.0]
  black_down = [.00, 1.00, 1.00, .0]
  black_down_amp = [.00, .00, .75, 1.00, 1.00, .0]
  black_trill = [.00, .00, .03, .70, .06, .00, .09, .75, .12, .00, .15, .80, .18, .05, .21, .85, .24, .10, .27, .90, .30, .10, .33, 1.00, .36, .10, .39, 1.00, .42, .10, .45, 1.00, .48, .10, .51, 1.00, .54, .10, .57, 1.00, .60, .10, .63, 1.00, .66, .10, .69, 1.00, .72, .10, .75, 1.00, .78, .10, .81, 1.00, .84, .10, .87, 1.00, .90, .00, .93, .95, .96, .00, 1.00, .90]
  black_up_down = [.00, .00, .50, 1.00, 1.00, .20]
  black_amp = [.00, .00, .50, 1.00, 1.00, .0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.8, .02, 2200, 1000, .1, black_down, $bird_amp)
   bird(.83, .01, 3000, 200, .05, black_up, $bird_amp)
   bird(.96, .02, 5800, 500, .05, black_up, $bird_amp)
   bird(1.00, .02, 4000, 200, .05, black_up, $bird_amp)
   bird(1.04, .10, 2100, 1700, .15, black_down, black_down_amp)
   bird(1.15, .05, 5700, 400, .25, black_up, $bird_amp)
   bird(1.25, .25, 2000, 900, .2, black_trill, $bird_amp)
   bird(1.52, .05, 5600, 400, .15, black_up_down, $bird_amp)

   bird(1.6, .04, 3900, 1100, .15, black_up, $bird_amp)
   bird(1.66, .01, 1900, 100, .10, black_up, black_amp)

   bird(1.69, .01, 3600, 300, .10, black_up, black_amp)
   bird(1.71, .03, 3900, 1000, .15, black_up, black_amp)
   bird(1.74, .02, 5000, 100, .20, black_up, black_amp)
   bird(1.76, .01, 1900, 100, .10, black_up, black_amp)

   bird(1.78, .01, 3600, 300, .10, black_up, black_amp)
   bird(1.80, .03, 3900, 1000, .15, black_up, black_amp)
   bird(1.83, .02, 5000, 100, .20, black_up, black_amp)
   bird(1.85, .01, 1900, 100, .10, black_up, black_amp)

   bird(1.87, .01, 3600, 300, .10, black_up, black_amp)
   bird(1.89, .03, 3900, 1000, .15, black_up, black_amp)
   bird(1.92, .02, 5000, 100, .20, black_up, black_amp)
   bird(1.94, .01, 1900, 100, .10, black_up, black_amp)

   bird(1.96, .01, 3600, 300, .10, black_up, black_amp)
   bird(1.98, .03, 3900, 1000, .15, black_up, black_amp)
   bird(2.01, .02, 5000, 100, .20, black_up, black_amp)
   bird(2.03, .01, 1900, 100, .10, black_up, black_amp)

   bird(2.05, .01, 3600, 300, .10, black_up, black_amp)
   bird(2.07, .03, 3900, 1000, .15, black_up, black_amp)
   bird(2.10, .02, 5000, 100, .20, black_up, black_amp)
   bird(2.13, .01, 1900, 100, .10, black_up, black_amp)

   bird(2.16, .03, 3800, 300, .1, black_up, $bird_amp)
	},
  report_in_minibuffer("black_throated_sparrow"))
end



def black_chinned_sparrow(beg)
  chin_up = [.00, .00, 1.00, 1.0]
  chin_up2 = [.00, .00, .30, .20, 1.00, 1.0]
  
  one_bird(beg, 3.0,
  Proc.new {||
   bird(.6, .2, 4200, 100, .1, chin_up, $bird_amp)
   bird(1.0, .09, 3800, 2000, .1, chin_up2, $bird_amp)
   bird(1.25, .08, 3900, 1700, .12, chin_up2, $bird_amp)
   bird(1.40, .08, 3600, 2300, .13, chin_up, $bird_amp)
   bird(1.50, .11, 3100, 2800, .14, chin_up, $bird_amp)
   bird(1.65, .07, 2900, 2700, .15, chin_up, $bird_amp)
   bird(1.74, .07, 2900, 2700, .15, chin_up, $bird_amp)
   bird(1.82, .07, 3000, 2300, .13, chin_up, $bird_amp)
   bird(1.89, .07, 3200, 2000, .10, chin_up, $bird_amp)
   bird(1.97, .05, 3200, 1500, .10, chin_up, $bird_amp)
   bird(2.04, .04, 3400, 1000, .07, chin_up, $bird_amp)
   bird(2.10, .03, 3600, 700, .05, chin_up, $bird_amp)
   bird(2.15, .03, 3800, 300, .05, chin_up, $bird_amp)
   bird(2.19, .02, 3900, 100, .03, chin_up, $bird_amp)
   bird(2.22, .01, 3900, 100, .01, chin_up, $bird_amp)
   bird(2.24, .01, 3900, 100, .01, chin_up, $bird_amp)
	},
  report_in_minibuffer("black_chinned_sparrow"))
end


def various_gull_cries_from_end_of_colony_5(beg)
  gullstart = [0, 0, 10, 1, 20, .5000, 40, .6000, 60, .5000, 100, 0]
  gullmiddle = [0, 0, 10, 1, 30, .5000, 80, .5000, 100, 0]
  gullend = [0, 0, 5, 1, 10, .5000, 90, .4000, 100, 0]
  unknown = [1, .1, 2, 1, 3, .1, 4, .01, 5, .09, 6, .01, 7, .01]

  one_bird(beg, 10.0,
  Proc.new {||
   bigbird(.250, .80, 1180, 1180, .08, gullend, $bird_amp, unknown)
   bigbird(1.500, .90, 1180, 1180, .07, gullend, $bird_amp, unknown)
   bigbird(2.750, 1.00, 1050, 1050, .08, gullend, $bird_amp, unknown)
   bigbird(4.800, .05, 1180, 1180, .06, gullstart, $bird_amp, unknown)
   bigbird(4.950, .10, 1180, 1180, .08, gullstart, $bird_amp, unknown)
   bigbird(5.150, .10, 1180, 1180, .09, gullstart, $bird_amp, unknown)
   bigbird(5.350, .10, 1180, 1180, .1, gullmiddle, $bird_amp, unknown)
   bigbird(5.450, .40, 1050, 1050, .1, gullend, $bird_amp, unknown)
   bigbird(6.250, .80, 1050, 1050, .1, gullend, $bird_amp, unknown)
   bigbird(7.450, 1.80, 1050, 1050, .1, gullend, $bird_amp, unknown)
	},
  report_in_minibuffer("gulls"))
end


def make_birds
#  atime = Time.new
  $out_file = new_sound("test.snd")
  set_squelch_update(true, $out_file, 0)
  orchard_oriole(0)
  cassins_kingbird(3)
  chipping_sparrow(6)
  bobwhite(9)
  western_meadowlark(12)
  scissor_tailed_flycatcher(15)
  great_horned_owl(18)
  black_throated_gray_warbler(21)
  yellow_warbler(24)
  black_necked_stilt(27)
  chestnut_sided_warbler(30)
  grasshopper_sparrow(33)
  swamp_sparrow(36)
  golden_crowned_sparrow(39)
  indigo_bunting(42)
  hooded_warbler(45)
  american_widgeon(48)
  louisiana_waterthrush(51)
  robin(54)
  solitary_vireo(57)
  pigeon_hawk(61)
  cerulean_warbler(64)
  nashville_warbler(67)
  eastern_phoebe(70)
  painted_bunting(73)
  western_flycatcher(76)
  bachmans_sparrow(79)
  cedar_waxwing(82)
  bairds_sparrow(85)
  kentucky_warbler(88)
  rufous_sided_towhee(91)
  prothonotary_warbler(94)
  audubons_warbler(97)
  lark_bunting(100)
  eastern_bluebird(103)
  chuck_wills_widow(106)
  blue_gray_gnatcatcher(109)
  black_throated_sparrow(112)
  black_chinned_sparrow(115)
  set_squelch_update(false, $out_file, 0)
  various_gull_cries_from_end_of_colony_5(118)
  $out_data = false
#  btime = Time.new
#  $stderr.write sprintf("time: %f\n", btime - atime)
end



