#!/usr/bin/env perl
use experimental 'smartmatch';

sub range_policy
   {
   my $input_ref = shift;
   my @lines = @{ $input_ref };
   my $match = 0;

   foreach (@lines)
      {
      my $pattern = '(\d+)\-(\d+) ([a-z]): ([a-z]+)'; 
      my ($low, $high, $letter, $passwd) = $_ =~ /$pattern/;
      my $count = () = $passwd =~ /\Q$letter/g;
      
      $match++ 
         if ($count ~~ [$low..$high]);
      }
   return $match;

   }

sub position_policy
   {
   my $input_ref = shift;
   my @lines = @{ $input_ref };
   my $match = 0;

   foreach (@lines)
      {
      my $pattern = '(\d+)\-(\d+) ([a-z]): ([a-z]+)'; 
      my ($low, $high, $letter, $passwd) = $_ =~ /$pattern/;

      my $index_1 = substr($passwd, $low  - 1, 1);
      my $index_2 = substr($passwd, $high - 1, 1);

      my $check = ($letter eq $index_1) + ($letter eq $index_2);

      $match++ 
         if ($check == 1);
      }
   return $match;

   }


sub main
   {
   open my $handle, '<', "../input.txt";
   chomp(my @input = <$handle>);
   close $handle;

   my $p1 = range_policy(\@input);
   my $p2 = position_policy(\@input);

   print "Part 1 answer: $p1" . "\n";
   print "Part 2 answer: $p2" . "\n";
   }

main();
