#!/usr/bin/env perl
use experimental 'smartmatch';
use List::Util qw/sum/;

sub range_policy
   {
   #regex to extract variables
   my $pattern = '(\d+)\-(\d+) ([a-z]): ([a-z]+)'; 
   my ($low, $high, $letter, $passwd) = $_ =~ /$pattern/;
   my $count = () = $passwd =~ /\Q$letter/g;

   1 if ($count ~~ [$low..$high]);

   }

sub position_policy
   {
   #regex to extract variables
   my $pattern = '(\d+)\-(\d+) ([a-z]): ([a-z]+)'; 
   my ($low, $high, $letter, $passwd) = $_ =~ /$pattern/;

   #adjusted for indexing at 1
   my $index_1 = substr($passwd, $low  - 1, 1);
   my $index_2 = substr($passwd, $high - 1, 1);

   #exclusive or check
   1 if (($letter eq $index_1)^($letter eq $index_2));
   }


sub main
   {
   open my $handle, '<', "../input.txt";
   chomp(my @input = <$handle>);
   close $handle;

   my $p1 = sum( map{ range_policy   ($_) } @input );
   my $p2 = sum( map{ position_policy($_) } @input );

   print "Part 1 answer: $p1" . "\n";
   print "Part 2 answer: $p2" . "\n";
   }

main();
