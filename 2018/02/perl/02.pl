#!/usr/bin/env perl
use Algorithm::Combinatorics qw(combinations);

sub checksum
   {
   my $input_ref = shift;
   my @input = @{ $input_ref };

   #adapted from https://stackoverflow.com/a/42622549
   #matches strings with two or three repeated 
   my $pattern2 = '(?:^|(.)(?!\1))([a-z])\2(?!\2)';
   my $pattern3 = '(?:^|(.)(?!\1))([a-z])\2\2(?!\2)';

   my $two;
   my $three;
   foreach(@input)
      {
      #sort string into alphabetic order
      my $sort = (join '', sort { $a cmp $b } split(//, $_));
      $two   += $sort =~ /$pattern2/;
      $three += $sort =~ /$pattern3/;
      }
   return ($two*$three)

   }

sub common
   {
   my $input_ref = shift;

   my $iter = combinations($input_ref, 2);
   my $s1, $s2;
   while (my $combo = $iter->next) 
      {
      ($s1, $s2) = @$combo;
      #xor string difference
      my $match = ($s1 ^ $s2) =~ tr/\0//c;
      last if ($match == 1);
      }

   #find/remove whichever character is the mismatch
   my @x = split '', $s1;
   my @y = split '', $s2;

   my $result = join '',
                map { $x[$_] eq $y[$_] ? $y[$_] : "" }
                0 .. $#y;

   return $result;
   }

sub main
   {
   open my $handle, '<', "../input.txt";
   chomp(my @input = <$handle>);
   close $handle;

   my $p1 = checksum (\@input);
   my $p2 = common   (\@input);

   print "Part 1 answer: $p1" . "\n";
   print "Part 2 answer: $p2" . "\n";
   }

main();
