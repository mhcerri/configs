#!/usr/bin/env perl
#
# Handy script to generate a list of crypt-hook mutt settings based on the
# GPG groups.
#
# Usage (in .muttrc):
#
#   source 'mutt-gpg-groups.pl|'
#
use strict;
open(GROUPS, "gpg --with-colons --list-config group |") or die;
while (<GROUPS>) {
		if (/^cfg:group:([^:]*):(.*)/){
		my $group = $1;
		my @addrs = split(";", $2);
		for my $addr (@addrs) {
			open(KEY, "gpg --list-key \"$addr\" |") or die;
			while (<KEY>) {
				if (/^pub\s+.*\/([[:xdigit:]]+)/) {
					printf("crypt-hook %s 0x%s # %s\n",
					$group, $1, $addr);
					last;
				}
			}
			close(KEY);
		}
	}
}
close(GROUPS);
