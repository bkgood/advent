#!/usr/bin/env perl -n

use v5.36;

sub score { my ($x) = @_; return $x le 'Z' ? 27 + ord($x) - ord('A') : 1 + ord($x) - ord('a') }

state $sum = 0;

chomp;
my $sack = $_;

my $left_pocket = substr $sack, 0, length($sack) / 2;
my $right_pocket = substr $sack, length($sack) / 2;

my %left_contents; @left_contents{split m//, $left_pocket} = ();

my $dupe;
for my $right_item (split m//, $right_pocket) {
    if (exists $left_contents{$right_item}) {
        $dupe = $right_item;
    }
}

$sum += score($dupe);

END { say $sum }
