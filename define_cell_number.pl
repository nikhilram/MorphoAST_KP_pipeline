#!/usr/bin/perl -w

#use warnings;
#use strict;

use Data::Dumper;
$Data::Dumper::Maxrecurse = 0;
use List::Util qw(sum);
use List::Util qw(min);
use List::Util qw(max);


@files = @ARGV;

$a = $files[0];
$cell_number = 1;
$previous_position = 0;

open (S1, $a);
while ($line=<S1>){
chomp $line;

if ($line =~ m/IMAGE/){
	$header=$line;
} else {

	my @chunks;
    @chunks = split /,/, $line;	
    $position = $chunks[7];
    if ($position > $previous_position){
    	$compiled{$cell_number}{$position}{_details} = $line;
 		if ($position - $previous_position == 1){
 			@out = map { $chunks[$_] - $previous[$_] } 0 .. $#chunks;
 			$differences = join ',',@out;
 			$compiled{$cell_number}{$position}{_rates} = $differences;
 		} 	
       	$previous_position = $position;
       	@previous = @chunks;
       		       		} elsif ($position < $previous_position) {
       		       		$cell_number++;
						$compiled{$cell_number}{$position}{_details} = $line;
						$previous_position = $position;
						@previous = @chunks;
						} 
}
}
print "Cell_number,Time_Point,Description,$header\n";
@cells = sort {$a <=> $b} (keys %compiled);
for $n (@cells){
	@positions = sort {$a <=> $b} keys %{$compiled{$n}};
	$max_p = max(@positions);
	$min_p = min(@positions);
	$size = @positions;
	if (($max_p - $min_p) + 1 == $size){
		%p = map { $_ => 1 } @positions;
		if (exists $p{'1'} || exists $p{'2'} || exists $p{'3'}) {
			@keep_positions = @positions;
			$size_p = @keep_positions;
			$max_kp = max(@keep_positions);
			$min_kp = min(@keep_positions);			
			for ($i=$min_kp; $i<=16; $i++){
				if ($i <= $max_kp){
					$details = $compiled{$n}{$i}{_details};
		 			print "$n,$i,Raw_data,$details\n";
				} else {
		 			print "$n,$i,Raw_data,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n";				

				}
			
			}
		}
	}
}
