#!/usr/bin/env perl -n

use v5.36;
use warnings;

use Data::Dumper;

state @sensors;

chomp;

my @coords = m/Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)/;

my %row = (
    pos => [ @coords[0..1] ],
    nearest => [ @coords[2..3] ],
);

$row{radius} = dist(@row{qw/ pos nearest /});

push @sensors, \%row;

sub dist {
    my ($a, $b) = @_;

    return abs($a->[0] - $b->[0]) + abs($a->[1] - $b->[1]);
}

END {
    my $max = 4_000_000;
    #my $max = 20;
    my $mult = 4_000_000;

    SENSOR: for my $sensor (@sensors) {
        my ($pos, $radius) = $sensor->@{qw/ pos radius /};

        my @points;

        my @left = ($pos->[0] - $radius - 1, $pos->[1]);
        my @top = ($pos->[0], $pos->[1] + $radius + 1);
        my @right = ($pos->[0] + $radius + 1, $pos->[1]);
        my @bottom = ($pos->[0], $pos->[1] - $radius - 1);

        my @point = @left;

        # calling this after every line of points, to strike some balance between paying
        # too-many-subroutine-call penalties vs storing all the points in memory.
        my $check = sub {
            for my $check_sensor (@sensors) {
                $check_sensor == $sensor and next;
                @points or last;

                my ($pos, $radius) = $check_sensor->@{qw/ pos radius /};
                @points = grep { dist($pos, $_) > $radius } @points;
            }

            if (@points) {
                if (@points > 1) {
                    local $" = ',';

                    die "pt2: too many: ", join ', ', map { "@$_" } @points;
                }

                my $point = shift @points;

                say "pt2: ", $point->[0] * $mult + $point->[1];

                return 1;
            }

            return 0;
        };

        while ("@point" ne "@top") {
            $point[0] >= 0 && $point[1] >= 0 && $point[0] <= $max && $point[1] <= $max
                and push @points, [ @point ];
            $point[0]++;
            $point[1]++;
        }

        $check->() and last SENSOR;

        while ("@point" ne "@right") {
            $point[0] >= 0 && $point[1] >= 0 && $point[0] <= $max && $point[1] <= $max
                and push @points, [ @point ];
            $point[0]++;
            $point[1]--;
        }

        $check->() and last SENSOR;

        while ("@point" ne "@bottom") {
            $point[0] >= 0 && $point[1] >= 0 && $point[0] <= $max && $point[1] <= $max
                and push @points, [ @point ];
            $point[0]--;
            $point[1]--;
        }

        $check->() and last SENSOR;

        while ("@point" ne "@left") {
            $point[0] >= 0 && $point[1] >= 0 && $point[0] <= $max && $point[1] <= $max
                and push @points, [ @point ];
            $point[0]--;
            $point[1]++;
        }

        $check->() and last SENSOR;
    }
}

END {
    my $y = 2000000;

    my %beacons;
    my %clear;

    for my $sensor (@sensors) {
        my ($nearest) = $sensor->{nearest};

        if ($nearest->[1] == $y) {
            $beacons{$nearest->[1]}++;
        }
    }

    for my $sensor (@sensors) {
        my ($pos, $radius) = $sensor->@{qw/ pos radius /};

        if (abs($pos->[1] - $y) > $radius) {
            next;
        }

        for my $i (($pos->[0] - $radius) .. ($pos->[0] + $radius)) {
            if ($beacons{$i} || $clear{$i}) {
                next;
            }

            if (dist($pos, [ $i, $y ]) > $radius) {
                next;
            }

            $clear{$i}++;
        }
    }

    say "pt1: ", 0+keys %clear;
}
