#!/usr/bin/env perl -an

# A rock B paper C scissors
# X      Y       Z

# 1/2/3 pts for choosing r/p/s
# + 0 points for losing, 3 for a draw and 6 for victory

use v5.36.0;

state @victory = (1, 2, 0);

state $score = 0;

$F[0] = ord($F[0]) - ord('A');
$F[1] = ord($F[1]) - ord('X');

$score += 1 + $F[1];

if ($F[0] != $F[1]) {
    if ($victory[$F[0]] == $F[1]) {
        $score += 6;
    } # else lose
} else {
    $score += 3;
}

END { say $score }
