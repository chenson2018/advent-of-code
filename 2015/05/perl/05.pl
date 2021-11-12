#!/usr/bin/env perl

use List::Util qw/sum/;

sub p1_valid
  {
  return 0 if ($_ =~ /(ab|cd|pq|xy)/);
  my $duplicate = $_ =~ /(.)\1/;
  my $vowels = $_ =~ /((a|e|i|o|u).*?){3,}/;
  return ($duplicate + $vowels == 2)
  }

sub p2_valid
  {
  my $duplicate = $_ =~ /(..).*?\1/;
  my $sandwich  = $_ =~ /(.).\1/;
  return ($duplicate + $sandwich == 2)
  }

sub main
  {
  open my $handle, '<', "../input.txt";
  chomp(my @input = <$handle>);
  close $handle;
  $p1 = sum( map{ p1_valid($_) } @input );
  $p2 = sum( map{ p2_valid($_) } @input );
  print "Part 1 answer: $p1\n";
  print "Part 2 answer: $p2\n";
  }

main();
