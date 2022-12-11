#!/usr/bin/env perl -n

use v5.30.0;
use warnings;

state @head = (0, 0);
state %tail_visited;

# O => overlapping
# L/R => tail is left/right of head
# A/B => tail is above/below head
# AL => tail is above+left of head
state $tail_pos = 'O';

# head moves -> tail was -> tail becomes
state $tail_trans = {
    R => {
        O => 'L',
        L => 'L',
        R => 'O',
        A => 'AL',
        B => 'BL',
        AL => 'L',
        AR => 'A',
        BL => 'L',
        BR => 'B',
    },
    U => {
        O => 'B',
        L => 'BL',
        R => 'BR',
        A => 'O',
        B => 'B',
        AL => 'L',
        AR => 'R',
        BL => 'B',
        BR => 'B',
    },
    L => {
        O => 'R',
        L => 'O',
        R => 'R',
        A => 'AR',
        B => 'BR',
        AL => 'A',
        AR => 'R',
        BL => 'B',
        BR => 'R',
    },
    D => {
        O => 'A',
        L => 'AL',
        R => 'AR',
        A => 'A',
        B => 'O',
        AL => 'A',
        AR => 'A',
        BL => 'L',
        BR => 'R',
    },
};

state %tail_pos_xform = (
    O => [ 0, 0, 'N' ],
    L => [ -1, 0, 'L' ],
    R => [ 1, 0, 'R', ],
    A => [ 0, 1, 'U', ],
    B => [ 0, -1, 'D', ],
    AL => [ -1, 1 ],
    AR => [ 1, 1 ],
    BL => [ -1, -1 ],
    BR => [ 1, -1 ],
);

chomp;

sub move {
    my ($dir, $coords) = @_;

    if ($dir eq 'R') {
        $coords->[0]++;
    } elsif ($dir eq 'U') {
        $coords->[1]++;
    } elsif ($dir eq 'L') {
        $coords->[0]--;
    } elsif ($dir eq 'D') {
        $coords->[1]--;
    } else { die "wat: $dir"; }
}

my ($dir, $n) = split m/\s+/, $_;

for (1 .. $n) {
    move($dir, \@head);

    $tail_pos = $tail_trans->{$dir}{$tail_pos};

    my $tail_pos_xform = $tail_pos_xform{$tail_pos};
    my @tail_pos = ( $head[0] + $tail_pos_xform->[0], $head[1] + $tail_pos_xform->[1] );

    $tail_visited{join ',',@tail_pos}++;
}

END {
    say 0+keys %tail_visited;
}
