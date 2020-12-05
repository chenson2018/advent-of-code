#!/usr/bin/env perl
use List::Util qw/sum/;

#using positive to match each field
sub p1_valid
   {
   1 if ($_ =~  /(?=.*byr)
                 (?=.*iyr)
                 (?=.*eyr)
                 (?=.*hgt)
                 (?=.*hcl)
                 (?=.*ecl)
                 (?=.*pid)/x);
   }

sub p2_valid
   {
   my $pattern  = '(?=.*byr:(19[2-9]\d|200[0-2]) )'                  .
                  '(?=.*iyr:(201\d|2020) )'                           .
                  '(?=.*eyr:(202\d|2030) )'                            .
                  '(?=.*hgt:((1[5-8]\d|19[0-3])cm|(59|6\d|7[0-6])in) )' .
                  '(?=.*hcl:#[0-9a-f]{6} )'                              .
                  '(?=.*ecl:(amb|blu|brn|gry|grn|hzl|oth) )'              . 
                  '(?=.*pid:(\d{9})(?!\d))';
   1 if ($_ =~ /$pattern/);
   }

sub main
   {
   #change line seperator
   local $/ = "\n\n";
   open my $handle, '<', "../input.txt";
   #not chomping to leave newlines for last entries
   my @input = <$handle>;
   close $handle;

   #now make single lines with space seperators
   map { s/\n/ /g } @input;

   $p1 = sum( map{ p1_valid($_) } @input );
   $p2 = sum( map{ p2_valid($_) } @input );

   print "Part 1 answer: $p1" . "\n";
   print "Part 2 answer: $p2" . "\n";
   }

main();
