#!/usr/bin/env perl -n

use v5.36;

state $dupes = 0;

chomp;

my ($first, $second) = sort { $a->[0] <=> $b->[0] } map { [ split m/-/, $_ ] } split m/,/, $_;

if ($first->[0] == $second->[0] || $first->[1] >= $second->[0]) {
    $dupes++;
}

END { say $dupes }
