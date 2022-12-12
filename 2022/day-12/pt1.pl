#!/usr/bin/env perl -n

use v5.36.0;
use warnings;

use Data::Dumper;

state @map;
state @start;
state @end;

chomp;

my @row = split m//, $_;
push @map, \@row;

if (my ($x) = grep { $row[$_] eq 'S' } 0 .. $#row) {
    $start[0] = $x;
    $start[1] = $#map;
}

if (my ($x) = grep { $row[$_] eq 'E' } 0 .. $#row) {
    $end[0] = $x;
    $end[1] = $#map;
}

sub show_map {
    local $" = '';
    say "$_->@*" for reverse @map;
    local $" = ',';
    say "start=@start end=@end";
}

sub find_route {
    my ($start) = @_;

    my %seen;

    local $" = ',';

    my @q = ([$start, 0]);
    $seen{"@$start"}++;

    while (@q) {
        my ($node, $depth) = shift(@q)->@*;

        if ("@$node" eq "@end") {
            return $depth;
        }

        my @neighbors = grep {
            $_->[0] >= 0 && $_->[1] >= 0 && $_->[0] < $map[0]->@* && $_->[1] < @map
        } (
            [ $node->[0], $node->[1] - 1 ],
            [ $node->[0] - 1, $node->[1] ],
            [ $node->[0], $node->[1] + 1 ],
            [ $node->[0] + 1, $node->[1] ],
        );

        my @neighbor_heights = map { ord $map[$_->[1]][$_->[0]] } @neighbors;

        my $max_height = ord($map[$node->[1]][$node->[0]]) + 1;

        my @legal = grep {
            !$seen{"@$_"}
        } map {
            $neighbors[$_]
        } grep {
            $neighbor_heights[$_] <= $max_height
        } 0 .. $#neighbors;

        $seen{"@$_"}++ for @legal;

        push @q, [ $_, $depth + 1 ] for @legal;
    }

    return -1;
}

END {
    $start[1] = $#map - $start[1];
    $end[1] = $#map - $end[1];
    @map = reverse @map;

    $map[$start[1]][$start[0]] = 'a';
    $map[$end[1]][$end[0]] = 'z';

    show_map;
    my $steps = find_route([ @start ]);
    say "pt1: $steps";

    my $shortest;

    for my $j (0 .. $#map) {
        my $row = $map[$j];

        for my $i (0 .. $#$row) {
            $row->[$i] eq 'a' or next;

            (my $steps = find_route([ $i, $j ])) < 0 and next;

            if (!defined($shortest) || $steps < $shortest) {
                $shortest = $steps;
            }
        }
    }

    say "pt2: $shortest";
}
