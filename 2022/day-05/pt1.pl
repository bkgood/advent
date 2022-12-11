#!/usr/bin/env perl -n

use v5.30.0;
use warnings;

use constant CRATEMOVER_9001 => 1;

state $constructed;
state @stacks;
state @lines;

chomp;

if (my ($cnt, $from, $to) = m/move ([0-9]+) from ([0-9]+) to ([0-9]+)/) {
    if (CRATEMOVER_9001) {
        push $stacks[$to]->@*, splice $stacks[$from]->@*, -$cnt;
    } else {
        for (1 .. $cnt) {
            push $stacks[$to]->@*, pop $stacks[$from]->@*;
        }
    }

    next;
}

END {
    for my $stack (@stacks) {
        print $stack->[-1] // '';
    }

    say '';
}

if ($constructed) {
    die "already constructed the stacks but didn't find an instruction?";
}

if (!length $_) {
    # we got all the stack info, construct it

    my $labels = pop @lines;
    my $i = 1;
    my @cols = (0);
    while (-1 != (my $idx = index $labels, $i++, $cols[-1] + 1)) {
        push @cols, $idx;
    }

    while (defined(my $line = pop @lines)) {
        for my $i (0 .. $#cols) {
            my $val = substr $line, $cols[$i], 1;

            if ($val =~ m/\A[A-Z]\z/) {
                push $stacks[$i]->@*, $val;
            }
        }
    }

    $constructed = 1;
    next;
}

push @lines, $_;
