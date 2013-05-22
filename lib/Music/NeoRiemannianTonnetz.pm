# -*- Perl -*-
#
# Performs Neo-Riemann operations on triads:
# https://en.wikipedia.org/wiki/Neo-Riemannian_theory

package Music::NeoRiemannianTonnetz;

use 5.010000;
use strict;
use warnings;

use Carp qw/croak/;
use List::Util qw/min/;
use Scalar::Util qw/reftype/;
use Try::Tiny;

our $VERSION = '0.10';

my $DEG_IN_SCALE = 12;

# for transform table, 'x'. "SEE ALSO" section in docs has links for [refs]
my %TRANSFORMATIONS = (
  P => \&_x_parallel,          # Parallel [WP]
  R => \&_x_relative,          # Relative [WP]
  L => \&_x_leittonwechsel,    # Leittonwechsel [WP]
  N => 'RLP',                  # Nebenverwandt [WP]
  S => 'LPR',                  # Slide [WP]
  H => 'LPL',                  # [WP]
);

########################################################################
#
# SUBROUTINES

sub _x_leittonwechsel {
  my ( $pset_str, $pset2orig ) = @_;
  my @new_set;

  if ( $pset_str eq '0,3,7' ) {
    # minor - 5th up by semitone
    for my $i ( keys %$pset2orig ) {
      if ( $i == 7 ) {
        push @new_set, map { $_ + 1 } @{ $pset2orig->{$i} };
      } else {
        push @new_set, @{ $pset2orig->{$i} };
      }
    }
  } elsif ( $pset_str eq '0,4,7' ) {
    # major - root moves down semitone
    for my $i ( keys %$pset2orig ) {
      if ( $i == 0 ) {
        push @new_set, map { $_ - 1 } @{ $pset2orig->{$i} };
      } else {
        push @new_set, @{ $pset2orig->{$i} };
      }
    }
  } else {
    # XXX call "handle unknown" sub or passthrough would be a good
    # alternatives
    croak "unknown pitch set [$pset_str]";
  }

  @new_set = sort { $a <=> $b } @new_set;
  return \@new_set;
}

sub _x_parallel {
  my ( $pset_str, $pset2orig ) = @_;
  my @new_set;

  if ( $pset_str eq '0,3,7' ) {
    # minor - Major the 3rd
    for my $i ( keys %$pset2orig ) {
      if ( $i == 3 ) {
        push @new_set, map { $_ + 1 } @{ $pset2orig->{$i} };
      } else {
        push @new_set, @{ $pset2orig->{$i} };
      }
    }
  } elsif ( $pset_str eq '0,4,7' ) {
    # Major - major the 3rd
    for my $i ( keys %$pset2orig ) {
      if ( $i == 4 ) {
        push @new_set, map { $_ - 1 } @{ $pset2orig->{$i} };
      } else {
        push @new_set, @{ $pset2orig->{$i} };
      }
    }
  } else {
    # XXX call "handle unknown" sub or passthrough would be a good
    # alternatives
    croak "unknown pitch set [$pset_str]";
  }

  @new_set = sort { $a <=> $b } @new_set;
  return \@new_set;
}

sub _x_relative {
  my ( $pset_str, $pset2orig ) = @_;
  my @new_set;

  if ( $pset_str eq '0,3,7' ) {
    # minor - move root down a tone
    for my $i ( keys %$pset2orig ) {
      if ( $i == 0 ) {
        push @new_set, map { $_ - 2 } @{ $pset2orig->{$i} };
      } else {
        push @new_set, @{ $pset2orig->{$i} };
      }
    }
  } elsif ( $pset_str eq '0,4,7' ) {
    # major - move 5th up a tone
    for my $i ( keys %$pset2orig ) {
      if ( $i == 7 ) {
        push @new_set, map { $_ + 2 } @{ $pset2orig->{$i} };
      } else {
        push @new_set, @{ $pset2orig->{$i} };
      }
    }
  } else {
    # XXX call "handle unknown" sub or passthrough would be a good
    # alternatives
    croak "unknown pitch set [$pset_str]";
  }

  @new_set = sort { $a <=> $b } @new_set;
  return \@new_set;
}

# used as fall-through if set if find something do not know what to do
# with in transform table
sub get_default_token {
  my ($self) = @_;
  if ( !exists $self->{default_token} ) {
    croak 'default_token has not been set';
  }
  return $self->{default_token};
}

# Transform table, 'x'
sub get_x_table { $_[0]->{x} }

sub new {
  my ( $class, %param ) = @_;
  my $self = { x => \%TRANSFORMATIONS };

  if ( exists $param{default_token} ) {
    $self->{default_token} = $param{default_token};
  }

  # should not need to alter, but who knows
  $self->{_DEG_IN_SCALE} = int( $param{DEG_IN_SCALE} // $DEG_IN_SCALE );
  if ( $self->{_DEG_IN_SCALE} < 2 ) {
    croak 'degrees in scale must be greater than one';
  }

  if ( exists $param{x} ) {
    croak 'x must be hash reference' unless ref $param{x} eq 'HASH';
    $self->{x} = $param{x};
  }

  bless $self, $class;

  return $self;
}

# Based on normal_form of Music::AtonalUtil but always transposes to
# zero (cannot use prime_form, as that goes one step too far and
# conflates [0,4,7] with [0,3,7] which here must be distinct).
sub normalize {
  my $self = shift;
  my $pset = ref $_[0] eq 'ARRAY' ? $_[0] : [@_];

  croak 'pitch set must contain something' if !@$pset;

  my %origmap;
  for my $p (@$pset) {
    push @{ $origmap{ $p % $self->{_DEG_IN_SCALE} } }, $p;
  }
  if ( keys %origmap == 1 ) {
    return keys %origmap, \%origmap;
  }
  my @nset = sort { $a <=> $b } keys %origmap;

  my @equivs;
  for my $i ( 0 .. $#nset ) {
    for my $j ( 0 .. $#nset ) {
      $equivs[$i][$j] = $nset[ ( $i + $j ) % @nset ];
    }
  }
  my @order = reverse 1 .. $#nset;

  my @normal;
  for my $i (@order) {
    my $min_span = $self->{_DEG_IN_SCALE};
    my @min_span_idx;

    for my $eidx ( 0 .. $#equivs ) {
      my $span =
        ( $equivs[$eidx][$i] - $equivs[$eidx][0] ) % $self->{_DEG_IN_SCALE};
      if ( $span < $min_span ) {
        $min_span     = $span;
        @min_span_idx = $eidx;
      } elsif ( $span == $min_span ) {
        push @min_span_idx, $eidx;
      }
    }

    if ( @min_span_idx == 1 ) {
      @normal = @{ $equivs[ $min_span_idx[0] ] };
      last;
    } else {
      @equivs = @equivs[@min_span_idx];
    }
  }

  if ( !@normal ) {
    # nothing unique, pick lowest starting pitch, which is first index
    # by virtue of the numeric sort performed above.
    @normal = @{ $equivs[0] };
  }

  # but must map <b dis fis> (and anything else not <c e g>) so b is 0,
  # dis 4, etc. and also update the original pitch mapping - this is
  # the major addition to the otherwise stock normal_form code.
  if ( $normal[0] != 0 ) {
    my $trans = $self->{_DEG_IN_SCALE} - $normal[0];
    my %newmap;
    for my $i (@normal) {
      my $prev = $i;
      $i = ( $i + $trans ) % $self->{_DEG_IN_SCALE};
      $newmap{$i} = $origmap{$prev};
    }
    %origmap = %newmap;
  }

  return join( ',', @normal ), \%origmap;
}

sub set_default_token {
  my ( $self, $token ) = @_;
  if ( !defined $token ) {
    delete $self->{default_token};
  } else {
    $self->{default_token} = $token;
  }
  return $self;
}

sub set_x_table {
  my ( $self, $table ) = @_;
  croak 'transformation table must be hash reference'
    unless ref $table eq 'HASH';
  $self->{x} = $table;
  return $self;
}

# Turns string of tokens (e.g. 'RLP') into a list of tasks (CODE refs,
# or more strings, which are recursed on until CODE refs or error).
# Returns array reference of such tasks. Called by transform() if user
# has not already done this and passes transform() a string of tokens.
sub taskify_tokens {
  my ( $self, $tokens, $tasks ) = @_;
  $tasks //= [];
  $tokens = [ $tokens =~ m/([A-Z][a-z0-9]*)/g ] if !defined reftype $tokens;

  # XXX optimize input? - runs of P can be reduced, as those just toggle
  # the third - even number of P a no-op, odd number of P can be
  # replaced with 'P'. Other optimizations are likely possible.

  for my $t (@$tokens) {
    if ( exists $self->{x}{$t} ) {
      if ( ref $self->{x}{$t} eq 'CODE' ) {
        push @$tasks, $self->{x}{$t};
      } elsif ( !defined reftype $self->{x}{$t}
        or ref $self->{x}{$t} eq 'ARRAY' ) {
        $self->taskify_tokens( $self->{x}{$t}, $tasks );
      } else {
        croak 'unknown token in transformation table';
      }
    } else {
      if ( exists $self->{default_token} ) {
        if (!defined reftype $self->{default_token}
          or ref $self->{default_token} eq 'ARRAY' ) {
          $self->taskify_tokens( $self->{default_token}, $tasks );
        } elsif ( ref $self->{default_token} eq 'CODE' ) {
          push @$tasks, $self->{default_token};
        } else {
          croak 'unknown default_token';
        }
      } else {
        croak "unimplemented transformation token '$t'";
      }
    }
  }

  # TODO include the name of the operation in the list, so that is
  # available? would need return list of hash refs or something
  return $tasks;
}

sub techno { shift; (qw/tonn tz/) x ( 8 * ( shift || 1 ) ) }

sub transform {
  my $self   = shift;
  my $tokens = shift;
  croak 'tokens must be defined' unless defined $tokens;
  my $pset = ref $_[0] eq 'ARRAY' ? $_[0] : [@_];
  croak 'pitch set must contain something' if !@$pset;

  # Assume list of tasks (code refs to call) if array ref, otherwise try
  # to generate such a list.
  my $tasks;
  if ( ref $tokens eq 'ARRAY' ) {
    $tasks = $tokens;
  } else {
    try { $tasks = $self->taskify_tokens($tokens) } catch { croak $_ };
  }

  my $new_pset = [@$pset];
  for my $task (@$tasks) {
    $new_pset = $task->( $self->normalize($new_pset) );
  }

  return $new_pset;
}

1;
__END__

=head1 NAME

Music::NeoRiemannianTonnetz - performs Riemann operations on triads

=head1 SYNOPSIS

  use Music::NeoRiemannianTonnetz;
  my $nrt = Music::NeoRiemannianTonnetz->new;

  # "parallel" changes Major to minor
  $nrt->transform('P', [60, 64, 67]);   # [60, 63, 67]

  my $tasks = $nrt->taskify_tokens('LPR');
  my $new_pitch_set = $nrt->transform($tasks, [0,3,7]);

=head1 DESCRIPTION

Offers a means to perform Neo-Riemannian operations (e.g. C<LPR>) on
major and minor triads (and only those triads, at the moment). In
theory, should be extensible to support other transformations on other
pitch sets (e.g. 7ths, as detailed in the literature), but that would
require more work and reading.

This is a very new module, use with caution, things may change, etc.

=head2 TRANSFORMATIONS

Available operations (sometimes called "tokens" in this module) for the
B<transform> method include:

  P  Parallel
  R  Relative
  L  Leittonwechsel
  N  Nebenverwandt (RLP)
  S  Slide (LPR)
  H  "hexatonic pole exchange" (LPL)

=head1 METHODS

The code may C<croak> if something goes awry; catch these errors with
e.g. L<Try::Tiny>. The B<new> method is doubtless a good one to begin
with, and then B<transform> to just experiment around with the
transformations.

=over 4

=item B<get_default_token>

Returns the default token, or otherwise throws an error if that has
not been set.

=item B<get_x_table>

Returns the transformation table, a hash reference.

=item B<new> I<parameter_pairs ...>

Constructor.

=over 4

=item B<default_token>

A token to use should the B<taskify_tokens> method be unable to convert
an element of the tasks lists to a CODE reference via the transformation
table. The default token should either be a CODE reference or a string
of (hopefully extant!) token names, e.g. C<RLP>.

=item B<DEG_IN_SCALE>

A 12-tone system is assumed, though may be changed, though I have no
idea what that would do.

  Music::NeoRiemannianTonnetz->new(DEG_IN_SCALE => 17);

=item B<x>

Hash reference to set a custom transformation table. Read the source to
figure out what this needs to be.

=back

=item B<normalize> I<pitch_set>

Normalizes the given pitch set (a list or array reference of pitch
numbers, which in turn should be integers) via code that is something
like B<normal_form> of L<Music::AtonalUtil> but slightly different.
Returns a string of the normalized pitch set (such as C<0,4,7> for a
Major triad), and a hash reference that maps the normalized pitch set
pitch numbers to the original pitches of the input I<pitch_set>.

This method is used internally by the B<transform> method.

=item B<set_default_token> I<token>

Sets the default token. See the B<default_token> parameter docs to the
B<new> method for details. Returns the object, so can be chained.

=item B<set_x_table> I<hashref>

Sets the transformation table. See source, etc. Returns the object, so
can be chained.

=item B<taskify_tokens> I<tokens>, [ I<tasks> ]

Converts tokens (a string such as C<RLP> (three tokens, C<R>, C<L>, and
C<P>), or an array reference of such) to a list of tasks (CODE
references) returned as an array reference, assuming all went well with
the taskification.

=item B<techno> [ I<measurecount> ]

Generates techno beats (returned as a list). The optional
I<measurecount> should be a positive integer, and doubtless a
power of two.

=item B<transform> I<tokens>, I<pitch_set>

Transforms the given I<pitch_set> (a list or array reference of pitch
numbers, ideally integers) via the given I<tokens>. If I<tokens> is
not an array reference, it is fed through the B<taskify_tokens>
method first. Returns the new pitch set (as an array reference) if
all goes well.

The resulting pitch set will be ordered from lowest pitch to highest;
Neo-Riemannian theory appears to care little about chord inversions, so
operations will often convert root position chords between 1st or 2nd
inversions, depending on the starting chord and operations in question.

=back

=head1 BUGS

Newer versions of this module may be available from CPAN. If the bug is
in the latest version, check:

L<http://github.com/thrig/Music-NeoRiemannianTonnetz>

C<techno> is not a bug, though may bug some.

=head1 SEE ALSO

=over 4

=item *

[WP] https://en.wikipedia.org/wiki/Neo-Riemannian_theory as an
introduction.

=item *

[Cohn 1998] "Introduction to Neo-Riemannian Theory: A Survey and a
Historical Perspective" by Richard Cohn. Journal of Music Theory, Vol.
42, No. 2, Neo-Riemannian Theory (Autumn, 1998), pp. 167-180.

And also the entire Journal of Music Theory Vol. 42, No. 2, Autumn, 1998
publication: L<http://www.jstor.org/stable/i235025>

=item *

Various other music modules by the author, for different views on music
theory: L<Music::AtonalUtil>, L<Music::Canon>,
L<Music::Chord::Positions>, among others.

=back

=head1 AUTHOR

Jeremy Mates, E<lt>jmates@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013 by Jeremy Mates

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself, either Perl version 5.16 or, at
your option, any later version of Perl 5 you may have available.

=cut
