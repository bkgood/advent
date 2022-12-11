#!/usr/bin/env perl -n

use v5.30.0;
use warnings;

use Time::HiRes qw/ sleep /;

state @rope = map { +{ abs => [ 0, 0 ], visited => {} } } 1 .. 10;

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

sub translate {
    my ($coords, $t) = @_;

    return [ map { $coords->[$_] + $t->[$_] } 0 .. 1 ];
}

sub adjacent {
    my ($a, $b) = @_;

    my $y = abs($b->[1] - $a->[1]);
    my $x = abs($b->[0] - $a->[0]);

    return $x <= 1 && $y <= 1;
}

sub same_row_or_col {
    my ($a, $b) = @_;

    my $y = abs($b->[1] - $a->[1]);
    my $x = abs($b->[0] - $a->[0]);

    return $x == 0 || $y == 0;
}

my ($dir, $n) = split m/\s+/, $_;

state @diag_moves = (
    [ 1, 1 ],
    [ 1, -1 ],
    [ -1, 1 ],
    [ -1, -1 ],
);

sub render {
    my @bounds = ( [ 0, 40 ], [ 0, 40 ] );

    for my $knot (@rope) {
        for my $visited (keys $knot->{visited}->%*) {
            my ($x, $y) = $visited =~ m/\A(-?[0-9]+),(-?[0-9]+)\z/ or die "wat: $visited";

            if ($x < $bounds[0][0]) { $bounds[0][0] = $x; }
            if ($x > $bounds[0][1]) { $bounds[0][1] = $x; }
            if ($y < $bounds[1][0]) { $bounds[1][0] = $y; }
            if ($y > $bounds[1][1]) { $bounds[1][1] = $y; }
        }
    }

    say '';
    local $"=',';
    for my $y (reverse $bounds[1][0] .. $bounds[1][1]) {
        for my $x ($bounds[0][0] .. $bounds[0][1]) {
            my $printed;

            for my $i (0 .. $#rope) {
                my $coords = $rope[$i]{abs};
                if ($coords->[0] == $x && $coords->[1] == $y) {
                    print $i == 0 ? 'H' : $i == $#rope ? 's' : $i - 1;
                    $printed = 1;
                    last;
                }
            }

            #            my $coords = $rope[-1]{abs};
            #
            #            if ($coords->[0] == $x && $coords->[1] == $y) {
            #                print 's';
            #            } elsif ($rope[-1]{visited}{"$x,$y"}) {
            #                print '#';
            #            } else {
            #                print '.';
            #            }
            #            $printed=1;

            if (!$printed) { print '.'; }
        }

        print "\n";
    }

    sleep 0.1;
}

for (1 .. $n) {
    my ($head, @tail) = @rope;

    local $" = ',';
    move($dir, $head->{abs});

    while (@tail) {
        my ($next) = @tail;

        while (!adjacent($head->{abs}, $next->{abs})) {
            if (same_row_or_col($head->{abs}, $next->{abs})) {
                move($dir, $next->{abs});
            } else {
                for my $move (@diag_moves) {
                    if (adjacent($head->{abs}, my $moved = translate($next->{abs}, $move))) {
                        $next->{abs} = $moved;
                        last;
                    }
                }
            }
        }

        $head = shift @tail;
    }

    $_->{visited}{join ',', $_->{abs}->@*}++ for @rope;

    #    render;
}

END {
    say 0+keys $rope[1]{visited}->%*;
    say 0+keys $rope[-1]{visited}->%*;
}
