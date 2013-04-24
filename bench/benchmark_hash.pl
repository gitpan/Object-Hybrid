#!/usr/bin/perl

use Benchmark;

use lib './blib/lib', '../blib/lib';

use strict qw(vars subs);

use Object::Hybrid; # 'HASTE'; # see the effect
my $plain     =                     {a => 1};
my $blessed   =               bless {a => 1}, 'Foo';
my $hybrid    = Object::Hybrid->new({a => 1}, mutable_class => undef);
my $tied      = {};
use          Tie::Hash; 
tie %$tied, 'Tie::StdHash'; 
$tied->{a} = 1;
my   $hybrid_tied = {};
tie %$hybrid_tied, 'Tie::StdHash'; 
$hybrid_tied->{a} = 1;
Object::Hybrid->new($hybrid_tied); # in this case mutable_class => 1 has no effect
{
	package Foo; 
	use overload '*{}' => sub{}; # this makes very tiny slowdown of blessed
}

my $iterations = 100000;
timethese( $iterations, {

	hybrid                  => sub{ $hybrid->{a} },
	hybrid_method           => sub{ $hybrid->FETCH('a') },
	hybrid_method_fast      => sub{ $hybrid->fast->FETCH('a') },
	hybrid_can              => sub{ $hybrid->can('FETCH') },
	hybrid_can_switch       => sub{ (tied(%$hybrid)||$hybrid)->can('FETCH') },
	hybrid_can_fast         => sub{ $hybrid->fast->can('FETCH') },
	hybrid_tied             => sub{ $hybrid_tied->{a} },
	hybrid_tied_method      => sub{ $hybrid_tied->FETCH('a') },
	hybrid_tied_method_fast => sub{ $hybrid_tied->fast->FETCH('a') },
	hybrid_tied_can         => sub{ $hybrid_tied->can('FETCH') },
	hybrid_tied_can_switch  => sub{ (tied(%$hybrid_tied)||$hybrid_tied)->can('FETCH') },
	hybrid_tied_can_fast    => sub{ $hybrid_tied->fast->can('FETCH') },
	primitive               => sub{ $plain->{a} },
	primitive_blessed       => sub{ $blessed->{a} },
	primitive_tied          => sub{ $tied->{a} },
	primitive_switch        => sub{ tied(%$plain) ? tied(%$plain)->FETCH('a') : $plain->{a} },
	primitive_tied_switch   => sub{ tied(%$tied)  ? tied(%$tied )->FETCH('a') : $tied->{a} },
	primitive_tied_method   => sub{                 tied(%$tied )->FETCH('a') },
	
});
