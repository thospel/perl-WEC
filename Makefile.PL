#!/usr/bin/perl -w
# $Id: Makefile.PL 2693 2008-01-02 09:54:19Z hospelt $
# Closure cleanup bugs make it need at least 5.8.1
# Fix for bug #30027 makes it need at least 5.8.4
use 5.008004;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile
    (NAME	=> 'WEC',
     VERSION	=> '0.01',
     PREREQ_PM	=> { 
         "Exporter::Tidy"	=> 0.05,
         "Test::More"		=> 0.11,	# For the tests only
     },
     ($] >= 5.005 ?	## Add these new keywords supported since 5.005
      (AUTHOR	=> 'Ton Hospel <WEC@ton.iguana.be>') : ()),
     $^O eq "MSWin32" ? (
         PM_FILTER	=> '$(PERL) -p -e1',
     ) : (),
     clean		=> {
         FILES => '$(DISTNAME).ppd ppm cover_db',
     },
);

# START MY
# END MY
