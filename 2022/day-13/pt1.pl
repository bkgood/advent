#!/usr/bin/env perl -n

use v5.36.0;
use warnings;

use Data::Dumper;
use JSON::PP;

use constant DEBUG => 0;

state $seen_pairs = 0;
state $sum_ok_indicies = 0;
state @messages;

chomp;

$_ or next;

push @messages, JSON::PP::decode_json($_);

if ((@messages % 2) == 0) {
    $seen_pairs++;

    if (ok(@messages[-2, -1])) {
        DEBUG and say "$seen_pairs ok!";
        $sum_ok_indicies += $seen_pairs;
    }
}

sub ok {
    my ($first, $second) = @_;

    for my $i (0 .. $first->$#*) {
        my $one = $first->[$i];
        my $two = $second->[$i];

        if (!defined $two) {
            DEBUG and say "$seen_pairs: reached end of second";
            return 0;
        }

        if (ref $one || ref $two) {
            !ref $one and $one = [ $one ];
            !ref $two and $two = [ $two ];

            my $ok = ok($one, $two);

            if (!defined $ok) {
                DEBUG and say "$seen_pairs: list comparison uninteresting";
                next;
            } elsif ($ok) {
                DEBUG and say "$seen_pairs: list comparison ok";
                return $ok;
            } else {
                DEBUG and say "$seen_pairs: list comparison failed";
                return 0;
            }
        } elsif ($one < $two) {
            DEBUG and say "$seen_pairs: first ($one) is less than second ($two)";
            return 1;
        } elsif ($one > $two) {
            DEBUG and say "$seen_pairs: first ($one) is greater than second ($two)";
            return 0;
        }
    }

    if (@$second > @$first) {
        DEBUG and say "$seen_pairs: ran out of first before second";
        return 1;
    }

    return undef;
}

sub pair_cmp {
    my ($one, $two) = @_;

    my $ok = ok(@_);

    if (!defined $ok) {
        return 0;
    } elsif (!$ok) {
        return 1;
    } elsif ($ok) {
        return -1;
    }
}

END {
    my @dividors = ([[2]], [[6]]);
    my @sorted = sort { pair_cmp($a, $b) } @messages, @dividors;

    my $prod = 1;

    for my $i (1 .. @sorted) {
        for my $dividor (@dividors) {
            if ($sorted[$i - 1] == $dividor) {
                $prod *= $i;
            }
        }
    }

    say "pt2: $prod";
}

END {
    say "pt1: $sum_ok_indicies";
}
