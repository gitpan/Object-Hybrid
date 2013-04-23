package  Object::Hybrid::Class; 
use Class::Tag qw(tagger_class);
use      Object::Hybrid::Class qw(is mutable_class);

1;

=head1 DESCRIPTION

This document describes requirements for custom hybrid classes. It may be useful only for developers, not users of Hybrid::Object, as currently Hybrid::Object by default automatically constructs hybrid class from any given class and use it to promote() primitives, so that there is no need to be concerned with the following requirements. However, this document is still useful to understand Object::Hybrid architecture.

NOTE: this document may be outdated in some places, but still outlines architecture correctly.

=head1 Hybrid classes

Primitives can be promote()d by blessing it into custom hybrid class specified as argument. There are few advantages of using custom hybrid classes, in particular - opportunity for operator overloading. 

Custom hybrid class is required to subclass Object::Hybrid::CLASS (the default hybrid class) and is to meet certain other requirements, but all those requirements are met automatically by subclassing Object::Hybrid::CLASS, which makes construction of custom hybrid classes easier. For example, custom subclass of Object::Hybrid::CLASS may just define overloading of some operators, etc.  

=begin comment

Custom hybrid class may in theory target only specific type of primitives, as well as either tied() or not tied(), but then it is the responsibility of caller to use it conditionally only for promote()ing primitives of corresponding type. In contrast, default Object::Hybrid::CLASS is universal hybrid class that works fine for all supported types of primitives.

=end comment

Although subclassing Object::Hybrid::CLASS allows to avoid building it from scratch, still hybrid class requirements need to be understood for extending Object::Hybrid::CLASS in subclasses.

Promoting primitive to become hybrid (i.e. bless()ing it into hybrid class) simply adds object interface to primitive and is nothing more than a way to extend Perl primitives. There are many ways hybrid class can be designed. However, designing hybrid class in a special way allows to achieve compatibility and synergy with another major way of extending primitives - perltie API, as well as some other goals. 

Because of this, hybrid class is required to meet specific set of requirements. As a result of these requirements, which narrow design of hybrid class/interface, hybrids are useful not only as simple syntactic sugar, but become compatible with and complementary to using perltie API/classes for extending primitives. 

=begin comment

Essentially, perltie API allows to extend primitives in two related ways: 1) it allows to "pack" non-standard behavior into standard Perl primitives; and at the same time 2) allows to extend tied() objects beyond perltie interface. The first way is nearly perfect (except for gaps in perltie API implementation), but second is far from perfect as accessing extended interface of tied() object requires (conditional) tied() calls and that complicates code that is to manipulate extended tie()d primitives. The solution to this problem is exposing tied() object interface as hybrid object interface and, thus, unifying interfaces of both tie()d and non-tied() primitives. This is something that can be achieved with specific design of hybrid class and by promote()ing primitives to become hybrids.

As a result, promote()ing primitives with such hybrid class allows to write simple, non-specific code that can handle anything ranging from simple plain primitives to highly extended tied() objects. 

=end comment

Thus, for class to qualify as hybrid class it must meet the following requirements (all of them met by just subclassing Object::Hybrid::CLASS):

=over

=item Equivalent perltie API

For class to qualify as hybrid class it must implement perltie interface (all methods) for some type of primitive B<and> that interface (perltie methods) must be equivalent to directly accessing underlying primitive - B<both> tie()d and not tie()d. The equivalence requirement is what makes, say, next two lines to have exactly same effect for hash hybrids:

	$hybrid->{foo};
	$hybrid->FETCH('foo'); # same 

There are several ways to comply with this requirement. If primitive is not tied(), then methods may operate on the primitive to produce same effect as corresponding direct access. There are two ways to modify the effect of direct access itself: tie() and dereference operators overloading. If primitive is tied(), then methods may delegate to tied() object to automatically achieve equivalence, or operate on tie()d primitive to produce same result as direct access would. If dereference operator is overloaded, corresponding access methods should take that into account and produce same result as overloaded direct access would.

For example (this is approximately the way Object::Hybrid::CLASS does it):

	sub FETCH {
		my $self = shift;
		
		return tied(%$self)->FETCH(@_)
		if     tied(%$self);
		
		return $self->{$_[0]}
	}

The equivalence requirement limits hybrid classes to include only a subset of tieclasses. For example, the Tie::StdHash (of standard Tie::Hash module) meets equivalence requirement for non-tied() primitives, while Tie::ExtraHash - not.

In this context alternative to promote()ing primitive is tie()ing it to something like Tie::StdHash as it also will unify its interface with other tie()d hashes. However, this alternative gives up high speed of plain primitives (slowdown of about 30-40 times in some benchmarks), also tie()ing hash full of data may involve sizable costs of data copying, as well as this alternative cannot deliver same range of benefits hybrid objects can.

For performnce comparison of various interface options see L</"Performance"> section.

=item Complete perltie API

Currently tie()ing filehandles is still incomplete in Perl: sysopen(), truncate(), flock(), fcntl(), stat() and -X can't currently be trapped. However, hybrid class should implement equivalent samename methods and their uppercase aliases (SYSOPEN(), TRUNCATE(), FLOCK(), FCNTL(), STAT() and FTEST()) to fill the gap and also allow compatibility with tiehandle classes that also implement them. This will allow to write portable code that works around gaps in tiehandle implementation by B<always> using methods on hybrids filehandles instead of Perl built-in functions, for example:

	promote $FH;
	
	$FH->stat(); 
	$FH->ftest('-X'); 
	
	# same with indirect method calls...
	STAT  $FH;
	FTEST $FH  '-X';

Thus, to avoid problems with gaps in tiehandle implementation simply always call methods on hybrids instead of Perl built-in functions.

=item Delegation to tied() object

In case of tied() primitives, hybrid class should delegate all methods to tied() object. Delegation invokes called method on tied() object instead of hybrid. This is required to allow invoking perltie methods B<with parameters> on hybrid directly (instead of on tied() object):

	$hybrid->STORE($value, @args);

Although methods of the hybrid class may trigger methods of tied() class by simply accessing bless()ed primitive directly, they cannot pass arguments to them - this is why delegation is required.

This requirement makes entire interface of tied() object exposed as hybrid object interface. Caller may use $hybrid->can('foo') to determine whether tied() object (if any) or hybrid implement certain methods.

If tied() object provides no called method, delegation fails and then hybrid class is to fall back to calling its own samename method (of the hybrid class), called "fallback method", on hybrid itself. Fallback method should correspond to type of primitive, in case there are options (for example, FETCH() called for hash and array are supposed to be different methods). Fallback methods are simply same methods that would be called if hybrid is not tied(), meaning same methods of the non-tied() hybrid remain available if hybrid is tied(), except when tied() object exposes and replaces them with its own methods, if any, with same names. 

In particular, this is useful for defining methods that tied() class do not implement, e.g. to work around gaps in perltie implementation. For example, since stat() is not a perltie method currently, it is unlikely to be implemented by tiehandle class, but it is required to be defined for hybrid object to have portable C<< promote($tied_fh)->stat >> workarounds for currently broken C<stat($tied_fh)>, so hybrid's stat() method may look like:

	sub stat { stat $_[0]->self }

The stat() and few other methods provided by default hybrid class are similar to that, they use self() methods and because of that can work either for simple tied() classes like Tie::StdHash without any support form tied() class, or for tied() classes that just implement single additional self() method. If it is not possible for tied() class to define self() method, then tied() class may need to implement stat() and other methods itself.

Note that for perltie methods fallback methods are almost never called, because normally tieclass does implement perltie methods. However, if it is not, the equivalence requirement is violated, since triggering perltie method implicitly, via primitive access, will lead to exception raised as call is on tied() object and corresponding perltie method is not provided, but calling that method on hybrid may end up calling fallback method, i.e. no exception is raised. Though, raising exception in both cases is not to be considered equivalence either.

=item Method aliases

Hybrid classes must provide altered-case aliases for all its methods (e.g. lowercased aliases for all perltie methods). The method alias name is returned by C<< Object::Hybrid->method_alias($method_name) >>.

This requirement is especially relevant in case when there are same-name built-in functions for accessing primitives: shift(), exists(), seek(), etc. In this case and as general coding style for hybrids: the lower case should be used as functions or in direct method calls notation, while upper case can be used for indirect method call notation (later minimizes chances of indirect method notation colliding with non-parenthesized same name function calls). For example:

	seek $FH, 0, 0;         #        function call (coma after $FH, no extra arguments)
	SEEK $FH  0, 0, @args;  # indirect method call (no coma after $FH, @args extended interface)
	$FH->seek(0, 0, @args); #   direct method call (@args extended interface)

Altered-case aliases are automatically provided by promote() to tied() interfaces of tie()d primitives - no need to modify tied() classes.

In addition, calls of lower-case aliases must not fail (be fail-safe) due to method being not defined by either hybrid class or underlying tied() class (if any), becoming no-op and returning empty list or undef scalar. This allows to write portable code without (ab)using can() calls or eval{} wraps, which make code very combersome (in case of eval{} it is necessary to manually distinguish "can't find method" from errors in defined method). In contrast, upper-case aliases are not similarly fail-safe, calling them must be a fatal error if method, either lower-case or upper-case, is not defined, so that they can be used to ensure method is really called (mnemonics: insist on it being called by writing it in upper case). If, however, lower-case method is defined, the upper-case call will call it, not fail. For example:

	$hybrid->non_existing_method();   # must not fail due to "can't find method"
	$hybrid->NON_EXISTING_METHOD();   # fatal "can't find method" error
	
	$hybrid->maybe_existing_method(); # must not fail due to "can't find method"
	$hybrid->MAYBE_EXISTING_METHOD(); # may be a fatal "can't find method" error
	
	$filehandle_hybrid->fetch();      # must not fail due to "can't find method"
	$filehandle_hybrid->FETCH();      # likely fatal "can't find method" error (since filehandles normaly have no FETCH()), but will call fetch() (not fail) if fetch() happens to be defined

Implementing this feature of hybrid class requires autoloading non-defined methods and making AUTOLOAD to behave accordingly, but custom hybrid class can inherit it from default hybrid class.

Automatic altered-case aliases provided by default hybrid class are relatively costly, as they involve extra can() call to locate altered-case method in case originally called method is not defined. However, this is only relevant for tie()d primitives as hybrid class (either default or custom) is likely to have both aliases defined and automatic aliasing is not used. To avoid cost of automatic aliasing in case of tie()d primitives, it is better to call method's aliase that is known (or likely) to be defined by underlying tied() class. This means that, for example, FETCH() is likely to be faster than fetch() for tie()d primitives (as their tied() classes usually define no fetch(), just FETCH()).

=item fast() (aliase call())

The fast() method must efficiently return tied() object for tie()d invocant and invocant itself for non-tied. The fast() method is used for performance optimization:

	$hybrid->fast->FETCH('a'); # for tied() $hybrid is much faster than...
	$hybrid->FETCH('a');
	
	$hybrid->fast->can('foo'); # for tied() $hybrid is much faster than...
	$hybrid->can('foo');

For non-tied hybrids situation is reversed, but in absolute terms using fast() pays off, especially where tied hybrids are more common throughput. The tradeoff however is that $hybrid->fast->FETCH() provides no automatic aliasing, no fail-safety for non-defined methods, and do not fall back to methods of hybrid class in case tied() class defines no called method, so that using it is more risky and requires better knowledge of classes involved.

=item self() method

Any method of the hybrid object has only that object to operate on using either other hybrid methods or manipulating bless()ed primitive, either tie()d or not, directly. However, many tied() objects (like Tie::ExtraHash) transparently delegate operations on tie()d primitive to real primitive encapsulated somewhere inside that tied() object, using object just to store some additional state. If this is the case, tied() class may define self() as accessor for that underlying primitive to expose it to methods of the hybrid class and to users. As a result, methods of hybrid class would have access to both tie()d primitive and underlying real primitive. This can have a number of useful applications, in particular to work around gaps in tiehandle implementation and to increase performance, as it allows hybrid methods to bypass perltie layer and operate on underlying primitive directly, which may be bring large efficiency benefits for some bulk operations on underlying primitive. And in case of not tied() primitive self() simply returns that primitive.

For example, since there is no yet perltie support for stat() and -X tests, called on tiehandle they do not propagate to underlying real filehandle, so they should be somehow propagated manually, but it requires knowing how to get underlying filehandle, if there is any, out of tied() object. Defining self() method in tieclass is supposed to do just that, and hybrid classes are expected to define self() method as well, so that portable code can simply be:

	promote $FH;
	
	stat $FH->self;
	-X   $FH->self;

=for comment
	# NOTE: the indirect notation will not work in this case, so beware...
	#-X SELF $FH; # DON'T!
	#-X SELF $FH; # DON'T!

	# or nearly same using methods (default implementations of these methods also use self() under the hood):
	STAT  $FH;
	FTEST $FH '-X';
	$FH->stat(); 
	$FH->ftest('-X'); 

Note that use of self() method (as in above code) is limited since it requires tiehandle class (with exception for simple tiehandle classes similar to Tie::StdHandle) to also implement self() method, and also there may be tiehandles that simply do not have (single) underlying filehandle or operate it non-transparently, but still it is useful for writing code portable across specific tiehandle classes that are known to support self(), so it is a requirement for hybrid classes (to be compatible).

Simple tieclasses can define only self() method and automatically get sysopen(), truncate(), flock(), fcntl(), stat() and ftest() methods of default hybrid class to work for them correctly.

The self() method is intended primarily for performance optimizations and to work around gaps in perltie (specifically, tiehandles) implementation. If there were no gaps, then there is no need for self() method except for performance optimization, as hybrid primitive is then B<equivalent> to (but much slower than) what self() is supposed to return. However, gaps lead to situations when hybrid primitive is not equivalent and you need to get underlying primitive that tied() object uses under the hood - self() method is expected to return just that. To work around gaps in a way compatible with hybrid objects, it is recommended that tieclasses either implement self(), if possible, or implement corresponding workaround methods themself (see L</"Complete perltie API">).

Accordingly, as a requirement, hybrid class must provide self() method that simply returns the hybrid object: 

	sub self { $_[0] }

Since due to perltie implementation gaps hybrid object may be "non-equivalent" (as mentioned above) and, thus, of little use in case primitive is tie()d, self() may also return tied() object (in hope that tied() class is a simple Tie::StdHash-like class) in case self() of hybrid class is called as B<fallback method> (see L</"Delegation to tied() object">) for tied() hybrid, with return value at least checked to be of the same type as invocant and also that name of the tied() class has 'Std' in it:

	sub self { 
		my      $self; 
		return  $self
		if      $self 
		=   Object::Hybrid->ref_tied($_[0]) 
		and Object::Hybrid->ref_type($_[0]) 
		eq  Object::Hybrid->ref_type($self) 
		and ref($self) =~ /Std/;
		
		!$self or die("tied() class defines nor valid self() method for $_[0] (self() returned $self)");
		
		return $_[0]
	}

This is about how self() is implemented in default hybrid class. Although it works fine for simple tied() classes similar to Tie::StdHash (Tie::StdHandle, etc.) that have 'Std' in name (checked as an extra precaution), still for other, more complex such tied() classes this default self() may silently fail to deliver correct return value (there will be no exception raised by self() itself, but caller will hopefully soon notice things gone unexpected way), so tied() class may need to implement correct self() method itself. Finally, for many tieclasses it may not be possible to implement correct self() method, those tieclasses should implement self{} that simply raises an exception with proper explanations.

The default hybrid class makes no use of self() except for working around tiehandle implementation gaps, so by default hybrids do not depend on tieclass implementing self() method.

=item Optional bless() method

If defined, C<< $hybrid_class->bless($primitive) >> method is called instead of otherwise calling bless($primitive, $hybrid_class). The bless() method is optional. It can be used as constructor/initializer for hybrid object, except it is to reuse $primitive (same as built-in bless()) instead of creating new. TIE*() and new() constructors are not used for that, as they are supposed to be perltie-style constructors and we keep them as such in case tieclass is subclassed to make hybrid class.

=item C<use Object::Hybrid::Class>

To mark complete valid hybrid class as such, put this lines into it:

	package CustomHybridClass;	
	use Object::Hybrid;
	use Object::Hybrid::Class;

This requirement allows to use the following test to find out if $class is a hybrid class:

	Object::Hybrid->Class->is($class);

This test returns true for CustomHybridClass and all its subclasses, since subclasses of valid hybrid class are unlikely to (intentionally) make it invalid, but is not necessarily true in super-classes, as those may be not be (marked as) valid hybrid classes.

Also:

	package CustomHybridClass;	
	use Object::Hybrid;
	use Object::Hybrid::Class 'is mutable_class';

is equivalent to mutable_class => 1 argument for promote(), i.e. allows CustomHybridClass to be always used as mutable hybrid class that allows hybrids being later tie()d or untie()d, changing interface accordingly (thoug mutable hybrid classes/objects are way slower).

=back

Provided that these requirements are met, hybrid class may be arbitrarily extended by adding custom parameters to perltie methods as well as adding custom, non-perltie methods.

It is recommended that hybrid class subclasses Object::Hybrid::CLASS, as it at least ensures proper self() is defined. Or better subclass corresponding Object::Hybrid::FOO class (where FOO is either HASH, SCALAR, ARRAY or GLOB), so that only some methods may be overwritten. To automatically provide required altered-case aliasing of methods, use Object::Hybrid->methods() to define methods in subclass (see source of this module for examples).

=head2 Overriding with methods()

The methods() method should always be used to define methods in custom hybrid classes, as it automatically defines altered-case aliases for methods and can also define other aliases as well. For example, the following defines self(), self(), only(), ONLY(), directly(), DIRECTLY() as aliases for same method:

	Object::Hybrid->methods(
		self     => sub{ $_[0] },
		only     => 'self',
		directly => 'self',
	);

=for comment Since indirect method notation unfortunately do not work for self(), the self() method name should read naturally in direct notation, and preferably be not readable in indirect notation to discourage its accidental use. This criteria reject just(), very() and leaves only(), directly(), itself() alternatives. The only() reads ok, but it reads ok in indirect notation too. The itself() reads nice with $FH, but not with, say, $LINES filehandle, etc. So, directly() seems to be viable alternative: stat $FH->directly.



=head1 AUTHOR

Alexandr Kononoff (L<mailto:parsels@mail.ru>)

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2010 Alexandr Kononoff (L<mailto:parsels@mail.ru>). All rights reserved.

This program is free software; you can use, redistribute and/or modify it either under the same terms as Perl itself or, at your discretion, under following Simplified (2-clause) BSD License terms:

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=cut



