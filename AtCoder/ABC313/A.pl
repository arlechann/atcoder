use strict;
use warnings;

use List::Util 'max';

my $n = <>;
my @p = split(' ', <>);
my $first_p = shift @p;
my $max_p = max @p;

my $result = $first_p > $max_p ? 0 : $max_p - $first_p + 1;
print "$result\n";
