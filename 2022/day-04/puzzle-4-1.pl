#!/usr/bin/env perl -n

use v5.36;

state $dupes = 0;

chomp;

my ($first, $second) = split m/,/, $_;

$_ = [ split m/-/, $_ ] for $first, $second;

my @diffs = map { abs($_->[0] - $_->[1]) } $first, $second;

if ($diffs[1] > $diffs[0]) {
    my $tmp = $first;
    $first = $second;
    $second = $tmp;
}

# first is always the larger

if ($first->[0] <= $second->[0] && $first->[1] >= $second->[1]) {
    $dupes++;
}

END { say $dupes }
