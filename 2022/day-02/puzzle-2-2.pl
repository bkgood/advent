#!/usr/bin/env perl -an

# A rock B paper C scissors
# X      Y       Z

# 1/2/3 pts for choosing r/p/s
# + 0 points for losing, 3 for a draw and 6 for victory

use v5.36.0;

state %inst = (
    # lose
    X => { qw/ A Z B X C Y / },
    # draw
    Y => { qw/ A X B Y C Z / },
    # win
    Z => { qw/ A Y B Z C X / },
);

state $score = 0;

my ($theirs, $should) = @F;

my $mine = $inst{$should}{$theirs};

$score += 1 + (ord($mine) - ord('X'));
$score += [ 0, 3, 6 ]->[ord($should) - ord('X')];

END { say $score }

# IT IS NOT 11724
# IT IS 11980
