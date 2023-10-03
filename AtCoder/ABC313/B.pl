use strict;
use warnings;

my ($n, $m) = split(' ', <>);
my @a = (1..$m);
my @b = (1..$m);
for (my $i = 0; $i < $m; $i++) {
	($a[$i], $b[$i]) = split(' ', <>);
	$a[$i]--;
	$b[$i]--;
}

my @indegrees = map { 0 } (1..$n);
for (my $i = 0; $i < $m; $i++) {
	$indegrees[$b[$i]]++;
}

my $count_top = grep { $_ == 0 } @indegrees;
if ($count_top != 1) {
	print "-1\n";
	exit;
}

my $result;
for (my $i = 0; $i < $n; $i++) {
	if ($indegrees[$i] == 0) {
		$result = $i;
		last;
	}
}

print(($result + 1) . "\n");
