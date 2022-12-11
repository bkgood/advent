#!/usr/bin/env perl -n

use v5.36.0;
use warnings;

use constant PT2 => 1;

state @monkies;
state $divisor_prod = 1;

chomp;

sub worry {
    my ($m, $worry) = @_;

    my $op = $m->{op};
    my $arg = $m->{arg};
    $arg eq 'old' and $arg = $worry;

    if ($op eq '+') {
        return $worry + $arg;
    } elsif ($op eq '*') {
        return $worry * $arg;
    } else {
        die "don't understand op=$op";
    }
}

if (m/\AMonkey/) {
    push @monkies, {};
} elsif (my ($items) = m/Starting items: (.*)/) {
    $monkies[-1]{items_decaying} = [ split m/,\s*/, $items ];
    $monkies[-1]{items} = [ $monkies[-1]{items_decaying}->@* ];
} elsif (my ($op, $arg) = m/Operation: new = old ([+*]) ([0-9]+|old)/) {
    $monkies[-1]{op} = $op;
    $monkies[-1]{arg} = $arg;
} elsif (my ($divisor) = m/Test: divisible by ([0-9]+)/) {
    $monkies[-1]{divisor} = $divisor;
    $divisor_prod *= $divisor;
} elsif (my ($cond, $target) = m/If (true|false): throw to monkey ([0-9]+)/) {
    $monkies[-1]{targets}[$cond eq 'true'] = $target;
} else {
    $_ and say STDERR "don't understand: $_";
}

sub round {
    my ($decaying) = @_;

    my $k = 'items';
    $k .= '_decaying' if $decaying;

    my $inspected = 'inspected';
    $inspected .= '_decaying' if $decaying;

    for my $monkey (@monkies) {
        my $items = $monkey->{$k};
        while (my $item = shift $items->@*) {
            $monkey->{$inspected}++;

            $item = worry($monkey, $item);

            if ($decaying) {
                $item = int($item / 3.0);
            } else {
                $item = $item % $divisor_prod;
            }

            my $target = $monkey->{targets}[$item % $monkey->{divisor} == 0];

            push $monkies[$target]{$k}->@*, $item;
        }
    }
}

sub show {
    my ($decaying) = @_;

    my $k = 'items';
    $k .= '_decaying' if $decaying;

    for my $i (0 .. $#monkies) {
        my $monkey = $monkies[$i];

        local $" = ', ';

        say "Monkey $i: $monkey->{$k}->@*";
    }
}

sub business {
    my ($decaying) = @_;

    my $k = 'inspected';
    $k .= '_decaying' if $decaying;

    my @ordered = sort { $a->{$k} <=> $b->{$k} } @monkies;
    return $ordered[-1]{$k} * $ordered[-2]{$k};
}

END {
    for (1 .. 20) {
        round(1);
        #show(1);
    }

    for (1 .. 10000) {
        round();
    }

    say "pt1: ", business(1);
    say "pt2: ", business();
}
