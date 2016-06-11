#!/usr/bin/perl

# Checks SPR item files for formatting errors
# USAGE: perl checkformat.perl <EXP_NAME>
# EXAMPLE: perl checkformat.perl antiloc08
################

use utf8;
use open ':utf8';
# $number_files = 16;

$proj = $ARGV[0];

$out = "../experiments/$proj/format_errors.txt";

open(OUT,">$out");

@files = <../experiments/$proj/items>;

#$scn--;
foreach $path(@files){
	$path =~ m/^.+\/([^\/]+)$/;
	$file = $1;
	#next if $file !~ /experimental_items/ && $file !~ /practice\.txt/;
	open(IN,"<$path");
	#print "$file\n";
	$linecount=0;
	$nextline="#";
	while(<IN>){
		$linecount++;
		#print "$linecount\n";
		$line = $_;
		if($line =~ /^#/){
			if($nextline ne "#"){
				print "$file\:$linecount: Wrong line!\n";
				print OUT "$file\:$linecount: Wrong line!\n";
			}
			if($line !~ /^#\s[^\s]+\s\d\d?\s[a-z]\n$/){
				print "$file\:$linecount: Error in item definition!\n";
				print OUT "$file\:$linecount: Error in item definition!\n";
			}
			$nextline="sent";
		}elsif($line =~ /^\?/){
			if($nextline ne "?"){
				print "$file\:$linecount: Wrong line!\n";
				print OUT "$file\:$linecount: Wrong line!\n";
			}
			if($line !~ /^\?\s[^\?]+\?\s[NY]\n$/){
				print "$file\:$linecount: Error in question!\n";
				print OUT "$file\:$linecount: Error in question!\n";
			}
			$nextline="#";
		}else{
			if($nextline eq "#" && length($line)>1){
				print "$file\:$linecount: Wrong line!\n";
				print OUT "$file\:$linecount: Wrong line!\n";
			}
			if(length($line) >= 218){
				print "$file\:$linecount: Sentence too long!\n";
				print OUT "$file\:$linecount: Sentence too long!\n";
			}
			$nextline="?" if $nextline eq "sent";
		}
		
		if ($line =~ /.+\/\s+\n$/){
			print "$file\:$linecount: Space at end of line!\n";
			print OUT "$file\:$linecount: Space at end of line!\n";
		}
		if($line =~ m/\s{2}/){
			print "$file\:$linecount: Double space character found!\n";
			print OUT "$file\:$linecount: Double space character found!\n";
		}
	}
	close(IN);
}
close(OUT);
print "Error log in $out\n";
