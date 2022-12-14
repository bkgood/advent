#!/usr/bin/env perl -n

use v5.36.0;
use warnings;

state @scan;
state $min_x;

sub new_x {
    my ($new) = @_;

    if (!defined $min_x || $new < $min_x) {
        $min_x = $new;
    }
}

chomp;

my @points = map { [ split m/,/, $_ ] } split m/ -> /, $_;

while (my $point = shift @points) {
    $scan[$point->[1]][$point->[0]] = '#';
    new_x($point->[0]);

    if (my ($next) = @points) {
        if ($point->[0] == $next->[0]) {
            # going down
            $point->[1] > $next->[1] and ($point, $next) = ($next, $point);
            for my $j ($point->[1] .. $next->[1]) {
                $scan[$j][$point->[0]] = '#';
            }
        } else {
            # doing left
            $next->[0] > $point->[0] and ($point, $next) = ($next, $point);

            for my $i ($next->[0] .. $point->[0]) {
                $scan[$point->[1]][$i] = '#';
                new_x($i);
            }
        }
    }
}

sub max { my $x = shift; $x = $_ > $x ? $_ : $x while $_ = shift; $x }

sub show {
    my $max_x = $min_x;
    for my $row (@scan) {
        $max_x = max($max_x, $row->$#*);
    }

    for my $j (0..$#scan) {
        printf '% 3d ', $j;

        for my $i ($min_x..$max_x) {
            if (my $x = $scan[$j][$i]) {
                print $x;
            } else {
                print '.';
            }
        }

        print "\n";
    }
}

# return true if sand stops in some decent place, false if it falls into the abyss
sub spawn_sand {
    my @pos = (500, 0);

    while ($pos[1] < $#scan) {
        my @possible = (
            [ $pos[0], $pos[1] + 1 ],
            [ $pos[0] - 1, $pos[1] + 1 ],
            [ $pos[0] + 1, $pos[1] + 1 ],
        );

        my $stopped = 1;
        for my $pos (@possible) {
            if (!$scan[$pos->[1]][$pos->[0]]) {
                @pos = @$pos;
                $stopped = 0;
                last;
            }
        }

        if ($stopped) {
            $scan[$pos[1]][$pos[0]] = 'o';
            return 1;
        }
    }

    say 'abyss!';
    return 0;
}

END {
    my $i = 0;

    while (spawn_sand) { $i++; }

    say "pt1: $i";
}
