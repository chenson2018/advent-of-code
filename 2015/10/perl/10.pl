#!/usr/bin/env perl

# shift a single set of repeated digits e.g. 3333 -> 43

sub repeat
  {
  my $str = shift;
  my $len = length ($str);
  my $in  = substr($str, 0, 1);
  return "$len$in";
  }

# advance an entire string
# note the regex mapped to an array, then I take the even indicies

sub advance
  {
  my $str = shift;
  my @arr = split /(?<=(.))(?!\1|$)/, $str;
  my @splits = map {$arr[$_-1]} grep {$_ & 1} 1..($#arr+1);
  my @next = map { repeat($_) } @splits;
  return (join '', @next);
  }

sub main
  {
  my $num = "3113322113";
  for (1..40)
    {
    $num = advance($num);
    }

  my $p1_ans = length($num);
  print "Part 1 answer: $p1_ans\n";

  for (1..10)
    {
    $num = advance($num);
    }

  my $p2_ans = length($num);
  print "Part 2 answer: $p2_ans\n";
  }

main();
