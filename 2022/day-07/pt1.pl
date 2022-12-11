#!/usr/bin/env perl -n

use v5.30.0;
use warnings;

state %size_for_path;
state @stack;

chomp;

if (m|\bcd /|) {
    if (!@stack) {
        push @stack, { name => '/', path => '/' };
    } else {
        pop @stack while @stack > 1;
    }
} elsif (m|\bcd [.][.]|) {
    pop @stack if @stack > 1;
} elsif (my ($dir) = m|\bcd ([^./]+)|) {
    my $entry = $stack[-1]{dirs}{$dir} ||= {
        name => $dir,
        path => $stack[-1]->{path} . "$dir/",
    };

    push @stack, $entry;
} elsif (my ($size, $file) = m/\A([0-9]+) ([^ ]+)/) {
    $stack[-1]{files}{$file} = $size;
    $_->{size} += $size for @stack;
    $size_for_path{$_->{path}} += $size for @stack;
}

END {
    my $total_used = $stack[0]{size};
    my $free = 70000000 - $total_used;
    my $needed = 30000000 - $free;

    my $delete_size;

    for my $path_size (grep { $_ >= $needed } values %size_for_path) {
        if (!defined $delete_size || $delete_size > $path_size) {
            $delete_size = $path_size;
        }
    }

    say "pt 2: ", $delete_size;
}

END {
    my $out;
    $out += $_ for grep { $_ <= 100000 } values %size_for_path;
    say "pt 1: ", $out;
}
