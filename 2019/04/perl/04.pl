#!/usr/bin/env perl
use List::Util qw/sum/;

#global regex
$non_decrease = '^(?=\d{6}$)1*2*3*4*5*6*7*8*9*$';
$repeat       = '(\d)\1';

sub p1_valid
   {
   1 if ( $_ =~ /$non_decrease/ and $_ =~ /$repeat/ );
   }

sub p2_valid
   {
   #first check non decreasing
   my $cond1  = $_ =~ /$non_decrease/;

   #remove repeats of more than length 2
   $_ =~ s/(\d)\1{2,}//g;

   #now check for repeats
   my $cond2 = $_ =~ /$repeat/;

   1 if ( $cond1 and $cond2 );
   }

sub main
   {
   my @input = (264360..746325);

   my @p1 = map { p1_valid($_) } @input;
   my @p2 = map { p2_valid($_) } @input;

   print "Part 1 answer: " . (sum @p1) . "\n";
   print "Part 2 answer: " . (sum @p2) . "\n";

   }

main();
