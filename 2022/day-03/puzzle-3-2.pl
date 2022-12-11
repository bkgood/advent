#!/usr/bin/env perl -n

use v5.36;

sub score { my ($x) = @_; return $x le 'Z' ? 27 + ord($x) - ord('A') : 1 + ord($x) - ord('a') }

state $sum = 0;
state @group;

chomp;
my $sack = $_;

push @group, $sack;

if (@group < 3) {
    next;
}

my %isect;
@isect{split m//, shift @group} = ();

while (my $bag = shift @group) {
    my %in_bag;
    @in_bag{split m//, $bag} = ();

    delete @isect{map { exists $in_bag{$_} ? () : $_ } keys %isect};
}

my ($common, @rest) = keys %isect;

@rest and die;

$sum += score($common);

END { say $sum }
