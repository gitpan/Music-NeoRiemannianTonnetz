use 5.010000;

use strict;
use warnings;
use ExtUtils::MakeMaker;

my %params = (
  NAME         => 'Music::NeoRiemannianTonnetz',
  ABSTRACT     => 'performs Neo-Riemann operations on set classes',
  VERSION_FROM => 'lib/Music/NeoRiemannianTonnetz.pm',
  PREREQ_PM    => {
    Carp                => 0,
    'List::Util'        => 0,
    'Scalar::Util'      => 0,
    'Test::More'        => 0,
    'Try::Tiny'         => 0,
  },
  ( $] >= 5.005
    ? ( AUTHOR => 'Jeremy Mates <jmates@cpan.org>' )
    : ()
  ),
);

if ( $ExtUtils::MakeMaker::VERSION =~ /^\d\.\d\d$/ and $ExtUtils::MakeMaker::VERSION > 6.30 ) {
  $params{LICENSE} = 'perl';
}

if ( $ExtUtils::MakeMaker::VERSION ge '6.46' ) {
  $params{META_MERGE} = {
    resources => {
      homepage   => 'http://github.com/thrig/Music-NeoRiemannianTonnetz',
      license    => 'http://dev.perl.org/licenses/',
      repository => 'http://github.com/thrig/Music-NeoRiemannianTonnetz',
    }
  };
}

WriteMakefile(%params);
