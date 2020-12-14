#!/usr/bin/env perl

#unique array elements
sub uniq {
    my %seen;
    grep !$seen{$_}++, @_;
}

#hash from text input
sub rules
   {
   my $input_ref = shift;
   my @input = @{ $input_ref };

   my %rules;

   foreach(@input)
      {
      s/[\s]?bags?[,.\s]//g;
      my ($out, $in) = split 'contain ', $_;

      #get inner bags as array
      my @in = split /\s*(?=\d)\s*/, $in;

      $rules{$out} = \@in;
      }

   return %rules;
   }

#get all keys that contain value
sub contained_by
   {
   my %rules  = %{ shift() };
   my @search = @{ shift() } ;

   my @res;
   
   foreach my $bag (@search)
      {
      my @match = grep { "@{ $rules{$_} }" =~ /$bag/ } keys(%rules);
      map { push(@res, $_) } @match; 
      }

   return @res;
   }

sub p1
   {
   my %rules  = %{ shift() };
   my @search = @{ shift() } ;
   my @p1;

   while ( scalar @{ $p1[-1] } > 0  or scalar @p1 == 0) 
      {
      my @match = contained_by(\%rules, \@search);
      push (@p1, \@match);
      @search = @match;
      }

   my @merge = map { @{ $_ } } @p1;
   my $p1    = scalar uniq @merge;

   return $p1;   
   }

sub main
   {
   open my $handle, '<', "../input.txt";
   chomp(my @input = <$handle>);
   close $handle;

   #create rules hash
   my %rules = rules(\@input);

   #part 1
   my @search = ("shiny gold");
   my $p1     = p1(\%rules, \@search);

   print "Part 1 answer: $p1" . "\n";
   }

main();
