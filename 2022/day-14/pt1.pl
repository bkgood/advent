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

# return stop coords if sand stops in some decent place, false if it falls into the abyss
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
            return \@pos;
        }
    }

    say 'abyss!';
    return 0;
}

sub clear {
    for my $row (@scan) {
        $row->@* = map { $_ && $_ eq '#' ? '#' : undef } $row->@*;
    }
}

sub add_floor {
    my ($floor_y) = @_;

    my $max_x = $min_x;
    for my $row (@scan) {
        $max_x = max($max_x, $row->$#*);
    }

    $scan[$floor_y] = [];
    $scan[$floor_y][$_] = '#' for $min_x .. $max_x;
}

sub grow_floor {
    my ($floor_y) = @_;

    my $floor = $scan[$floor_y];

    my $begin = 0;
    $begin++ while !$floor->[$begin];
    my $end = $begin;
    $end++ while $floor->[$end];

    my $add = $end - $begin;

    $floor->[$_] = '#' for ($begin-$add)..$begin;
    $floor->[$_] = '#' for $end..($end + $add);
}

END {
    my $i = 0;

    show;
    while (spawn_sand) { $i++; }
    show;

    say "pt1: $i";
    clear;

    my $floor_y = $#scan + 2;
    add_floor($floor_y);
    #show();

    if (1) {
        my $spawned = 0;
        while (1) {
            my $stop_pos = spawn_sand();

            if (!$stop_pos) {
                grow_floor($floor_y);
                redo;
            }

            $spawned++;

            if ($stop_pos->[0] == 500 && $stop_pos->[1] == 0) {
                say "pt2: $spawned";
                last;
            }

            #say $spawned;
            # this is very slow.
            #show();
        }
    }
}
