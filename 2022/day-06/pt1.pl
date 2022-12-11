#!/usr/bin/env perl -n

use v5.30;
use warnings;

use Data::Dumper;

state $marker_len = 14;

chomp;

my $i = 0;
while ($marker_len == length(my $quad = substr $_, $i, $marker_len)) {
    my %dupes;
    $dupes{$_}++ for split m//, $quad;

    if (!grep { $_ > 1 } values %dupes) {
        say $i + $marker_len;
        last;
    }

    $i++;
}
