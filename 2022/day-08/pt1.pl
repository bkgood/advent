#!/usr/bin/env perl -n

use v5.30.0;
use warnings;

state @forest;

chomp;

my @row;
push @forest, \@row;

for my $h (split m//, $_) {
    push @row, $h;
}

sub col { [ map { $forest[$_][$_[0]] } 0 .. $#forest ] }

sub vis_score {
    my ($t_i, $t_j) = @_;

    if ($t_i == 0 || $t_j == 0 || $t_i == $#forest || $t_j == $forest[0]->$#*) {
        return 0; # edges always have one zero side so the product is zero
    }

    my $row = $forest[$t_i];
    my $t_h = $row->[$t_j];
    my @scores;

    push @scores, 0;
    for my $j ($t_j + 1 .. $row->$#*) {
        $scores[-1]++;

        if ($row->[$j] >= $t_h) {
            last;
        }
    }

    push @scores, 0;
    for my $j (reverse 0 .. $t_j - 1) {
        $scores[-1]++;

        if ($row->[$j] >= $t_h) {
            last;
        }
    }

    my $col = col($t_j);

    push @scores, 0;
    for my $i ($t_i + 1 .. $col->$#*) {
        $scores[-1]++;

        if ($col->[$i] >= $t_h) {
            last;
        }
    }

    push @scores, 0;
    for my $i (reverse 0 .. $t_i - 1) {
        $scores[-1]++;

        if ($col->[$i] >= $t_h) {
            last;
        }
    }

    my $out = 1;
    $out *= shift @scores while @scores;
    return $out;
}

END {
    my $best = 0;
    for my $i (0 .. $#forest) {
        for my $j (0 .. $forest[0]->$#*) {
            if ($best < (my $score = vis_score($i, $j))) {
                $best = $score;
            }
        }
    }

    say "pt2: $best";
}

END {
    my %visible;
    for my $i (0 .. $#forest) {
        my $row = $forest[$i];

        my @visible;
        my $h = -1;
        my $rh = -1;

        for my $j (0 .. $#$row) {
            if ($row->[$j] > $h) {
                $h = $row->[$j];

                $visible{$i, $j}++;
            }

            $j++;
            if ($row->[-$j] > $rh) {
                $rh = $row->[-$j];

                $visible{$i, $#$row - $j + 1}++;
            }
        }
    }

    my @ft = map { col($_) } 0 .. $forest[0]->$#*;

    for my $j (0 .. $#ft) {
        my $col = $ft[$j];

        my @visible;
        my $h = -1;
        my $rh = -1;

        for my $i (0 .. $#$col) {
            if ($col->[$i] > $h) {
                $h = $col->[$i];

                $visible{$i, $j}++;
            }

            $i++;
            if ($col->[-$i] > $rh) {
                $rh = $col->[-$i];

                $visible{$#$col - $i + 1, $j}++;
            }
        }
    }

    say "pt1: ", 0+keys %visible;
}
