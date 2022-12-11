#!/usr/bin/env perl -n

use v5.30.0;
use warnings;
use Data::Dumper;

state $clock = 0;
state @x = (1);

sub consume_cycles {
    my ($cycles) = @_;

    for (1 .. $cycles) {
        my $draw_pos = $clock % 40;
        my $sprite_center = $x[$clock];
        if ($draw_pos >= $sprite_center - 1 && $draw_pos <= $sprite_center + 1) {
            print '#';
        } else {
            print '.';
        }

        if ($clock % 40 == 39) {
            print "\n";
        }

        $clock++;

        $x[$clock] //= $x[$clock - 1];
    }
}

if (m/noop/) {
    consume_cycles(1);
} elsif (my ($add) = m/addx (-?[0-9]+)/) {
    consume_cycles(2);

    $x[$clock] = $x[$clock - 1] + $add;

}

END {
    my $sum = 0;

    for (my $cycle = 20; $cycle <= 220; $cycle += 40) {
        $sum += $cycle * $x[$cycle-1];
    }

    say "pt1: ", $sum;
}
