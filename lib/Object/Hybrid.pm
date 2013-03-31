package Object::Hybrid;

#use 5.006; 

use strict qw[vars subs];
$Object::Hybrid::VERSION = '0.03_03';  

=head1 NAME

Object::Hybrid - promote Perl primitives (hashes, scalars, arrays, and filehandles), either tie()d or not, to become hybrid objects

=head1 SYNOPSIS

Here (and everywhere in this documentation) $primitive is a reference to either hash, scalar, array, or filehandle (i.e. perltie types).

Promote $primitive to become hybrid object:

	use Object::Hybrid qw(promote); # declare promote() for use
	promote $primitive; 

Or (especially if you prefer to not export anything) use new() constructor... 

	use           Object::Hybrid;
	$hybrid = new Object::Hybrid      $primitive;  # $primitive becomes hybrid object
	$hybrid =     Object::Hybrid->new($primitive); # same

After that $primitive remains the same, but it is now bless()ed as object that exposes corresponding perltie methods, so that the following, for example, become interchangeable for B<both tied() and not tied()> %$primitive:

	$primitive->{foo};
	$primitive->FETCH('foo'); # same
	$primitive->fetch('foo'); # same

Also, in case of tie()d primitive instead of:

	tied(%$primitive)->method();

just be tied()less:

	$primitive->method();

In case non-tied() primitives need to be interchangeable with tied() ones that have extended tied() interface, instead of cumbersome (possibly repeating many times) conditional code:

	tied(%$primitive) ? 
	tied(%$primitive)->FETCH('foo', @args)
	:     $primitive->{foo};

just:

	$primitive->FETCH('foo', @args);

However, former may still be (much) preferred for performance reasons (and it accepts both plain and promoted $primitive).

If $FH is a plain filehandle or tiehandle tied to class that implements stat(), ftest() (and others) and self() method, then the following simple code need not to discriminate between them:

	promote $FH;

	$FH->stat(); 
	$FH->ftest('-X'); 

	# same in indirect method notation:
	STAT  $FH;
	FTEST $FH '-X';

	# or nearly same with using self() method:
	stat $FH->self;
	-X   $FH->self;

=head1 DESCRIPTION

Some applications need to accept both plain primitives as well as tie()d primitives with additional extended interface available through tied() object. For example, application cache may be allowed to optionally use either screamingly fast plain hash or some highly evolved persistent hashes tie()d to disk storage (like DB_File, etc.). There are many similar examples for filehandles, arrays and even scalars. Those are cases when Object::Hybrid combined with simple coding style can make code that handles those primitives compatible across whole spectrum, from plain primitives to all types of extended tied() primitives. There are also other uses, including working around gaps in tiehandle implementation, adding some fancy operations to primitives without using tie() as well as plain syntactic sugar.

In the context of this module hybrid object is defined as a Perl object that represents primitive that object is implemented with (currently hash, scalar, array, or filehandle). According to this definition, hybrid object can be seen as both primitive and object at the same time. In general case, it is a violation of object encapsulation to access object's underlying (bless()ed) primitive directly (at least outside of class's methods), but in special case of hybrid objects it is perfectly ok to do so - no violation of encapsulation takes place. 

Hybrid objects are instances of the class that is referred to as "hybrid class". This module implements default hybrid class and exports promote() function that bless()es Perl's primitives (hash, scalar, array, or filehandle) into either default or user-specified (custom) hybrid class to make them hybrid objects.

Promoting primitive to become hybrid (i.e. bless()ing it into hybrid class) simply adds object interface to primitive and is a way to extend Perl primitives that is compatible with and complementary to another major way of extending primitives - perltie API. 

Hybrid object has corresponding perltie methods interface for accessing underlying (bless()ed) primitive that object is implemented with (e.g. tiehash methods for accessing underlying bless()ed hash, etc.). Moreover, for hybrid object the following, for example, are equivalent and interchangeable:

	$hybrid->{foo};
	$hybrid->FETCH('foo'); # same 
	$hybrid->fetch('foo'); # same 

As a result, promote()ing primitives to become hybrids allows to write simple, non-specific code that can handle anything ranging from simple plain primitives to highly extended tied() objects. 

Specifically, advantages of promote()ing primitives are:

=over

=item Compatibility

Promoting primitives to become hybrids allows the same simple code to manipulate (be compatible with) both plain primitives and extended tie()d primitives as it unifies their interfaces and make them interchangeable.

For example, if same code is required to accept and handle both plain hashes, i.e. fast, in-memory hashes, and tie()d hashes with extended perltie interface, e.g. slow persistent hashes tie()d to disk storage, then it is useful to promote() each of those hashes to become hybrid. Whenever plain access is required the following code:

	$hybrid->{foo};

will work for both in-memory and persistent hashes, and is really fast in case of in-memory hash. And in case you need to use extended interface, something like next code will also work for promote()d both in-memory and persistent hashes:

	$hybrid->FETCH('foo', @args); 

	$hybrid->can('custom_method') 
	and  $hybrid->custom_method();

In this context alternative to promote()ing primitive is tie()ing it to something like Tie::StdHash as it also will unify its interface with other tie()d hashes. However, this alternative gives up speed of plain primitives (slowdown of about 30-40 times in some benchmarks), also tie()ing hash full of data may involve sizable costs of data copying, as well as this alternative cannot deliver same range of benefits hybrid objects can.

Note that C<< $tied_hybrid->FETCH('foo') >> is itself much slower than C<< tied(%$tied_hybrid)->FETCH('foo') >> (about 10 times in some benchmarks) and even than C<< $tied_hybrid->{'foo'} >> (about 2-3 times in some benchmarks), so that it should be used to make compatible only extended interface calls like C<< $tied_hybrid->FETCH('foo', @args) >>, otherwise using C<< $tied_hybrid->{'foo'} >>. 

Despite promoting primitives to become hybrids turn them into Perl objects, compatibility with arbitrary Perl objects in practice has little value, since code that manipulates objects usually assume objects to be of very specific class. 

=item tied()less access

Accessing tied() interface of tie()d primitive no longer requires cumbersome (possibly conditional) tied() call, i.e. instead of:

	tied(%$hybrid)->method();

one can write:

	$hybrid->method(); # same

=item Incomplete tie() implementation workaround

Currently tie()ing filehandles is still incomplete in Perl: sysopen(), truncate(), flock(), fcntl(), stat() and -X cannot currently be trapped. However, both tieclasses and hybrid classes can define corresponding methods or use self() method  (see L</"Hybrid classes">) to not distinguish between primitives for that matter:

	promote $FH;

	$FH->stat(); 
	$FH->ftest('-X'); 

	# same in indirect method notation:
	STAT  $FH;
	FTEST $FH '-X';

	# or nearly same with using self() method (see below):
	stat $FH->self;
	-X   $FH->self;

=item Operator overloading

Custom hybrid classes can be used for overloading operators on primitives. However, unfortunately such hybrid classes currently can only be used to promote() non-tied() primitives (see L</"Operator overloading">). 

=back

Object::Hybrid is a lightweight pure Perl module with no dependencies beyond core.

=head1 Stop reading now (or how to use this documentation)

Usually, there is no need to read any of the following documentation to use Object::Hybrid - you can stop reading at this point. What you have read so far, or even just self-explanatory SYNOPSIS, is enough in most cases. The following documentation covers optional features that need not to be learned for using Object::Hybrid in most usual case (e.g. occasionally).

=head1 C<use Object::Hybrid>

	use Object::Hybrid;           # exports nothing
	use Object::Hybrid $feature;  # enables single named feature
	use Object::Hybrid %options; # most general form

The following features are supported:

	use Object::Hybrid             'promote';
	use Object::Hybrid feature =>  'promote';  # same
	use Object::Hybrid feature => ['promote']; # same
	use Object::Hybrid export  =>  'promote';  # same
	use Object::Hybrid export  => ['promote']; # same

which exports (i.e. declares for use) the promote() function into caller's namespace.

Next features depend on autobox pragma being installed (can be installed from CPAN archive):

	use Object::Hybrid             'autobox';
	use Object::Hybrid feature =>  'autobox';            # same
	use Object::Hybrid feature => ['autobox'];           # same
	use Object::Hybrid autobox => Object::Hybrid->CLASS; # same, but can be custom hybrid class

which will automatically promote() any primitive within the current scope, and "unpromote" them back beyond that scope. It is is equivalent to:

	use Object::Hybrid;
	use autobox 
	HASH   => Object::Hybrid->CLASS,
	SCALAR => Object::Hybrid->CLASS,
	ARRAY  => Object::Hybrid->CLASS; # can be custom hybrid class instead

And closely related is:

	use Object::Hybrid                 'autopromote';
	use Object::Hybrid feature     =>  'autopromote';         # same
	use Object::Hybrid feature     => ['autopromote'];        # same
	use Object::Hybrid autopromote =>  Object::Hybrid->CLASS; # same, but can be custom hybrid class

which makes any method call on primitive in the lexical scope to automatically promote() that primitive.

=head1 promote() function

	promote $primitive;                   # bless() to make $primitive a hybrid
	promote $primitive => \%args;         # same, but with named arguments
	promote $primitive =>  %args;         # same
	promote $primitive => $class;         # same, but with explicit $class to tie() to or bless() into
	promote $primitive => $class, \%args; # same, but with named arguments
	promote $primitive => $class,  %args; # same

In case $primitive is (or will later be) tied(), the tied() object is used as object interface of the hybrid (see L</"Delegation to tied() object">), unless custom hybrid $class is specified (see L</"Custom hybrid $class">). If hybrid gets tied()d or untie()d, its object interface immediately changes accordingly.

In any case, promote() never (re)tie()s primitive, only bless()es it.

If $primitive specified is of type not currently supported by Object::Hybrid, exception is raised. If not defined($primitive) or no $primitive is specified, then exception is raised unless custom $class is specified (see L</"Custom hybrid $class">).

The return value is a hybrid object, i.e. $primitive itself (may be useful if promote() is used in expressions).

By default, promote() always uses universal Object::Hybrid::CLASS and order of promote() and possible tie() calls is irrelevant. However, to significantly increase performance (times in some cases) it is possible to make promote() to instead use type-specific default hybrid class:

The "wont_tie" => 1 option constitutes a promise that promote()d non-tied() primitive will not be tie()d later (meaning tie(), if any, always happens before promote()), so that type-specific hybrid classes can be used to promote() non-tied() primitives by default instead of otherwise default universal Object::Hybrid::CLASS. This option has no effect if custom hybrid class is used.

If "not_tied" => 1 option is passed to promote(), then it ignores the fact that primitive is or may later be tied() - it makes tied() hybrid to behave as if it is not tied(). In theory, this allows to use hybrid object interface different from tied() interface, which may be useful in some special cases. Also, tied() object's interface will not be exposed as hybrid object interface. This option has no effect if custom hybrid class is used, in which case it is up to custom hybrid class to define desired behavior.

=head2 Custom hybrid $class

If custom hybrid $class is specified and either $primitive is not tied() or not_tied => 1 option is given, then $primitive is bless()ed into that custom hybrid $class instead of default one.

Custom $class must be type-specific for given $primitive, so the type-conditional expression for $class may need to be provided.

If custom hybrid $class is specified without $primitive or with not defined($primitive), then $primitive of the type expected by $class is autovivified. If $class do not allow to determine what $primitive type it expects, then exception is raised.

Refer to L</"Hybrid classes"> for requirements for custom hybrid classes.

=head1 new() method

	$hybrid = new Object::Hybrid $primitive;                   # bless() to make $primitive a hybrid
	$hybrid = new Object::Hybrid $primitive => \%args;         # same, but with named arguments
	$hybrid = new Object::Hybrid $primitive =>  %args;         # same
	$hybrid = new Object::Hybrid $primitive => $class;         # same, but with explicit $class to tie() to or bless() into
	$hybrid = new Object::Hybrid $primitive => $class, \%args; # same, but with named arguments
	$hybrid = new Object::Hybrid $primitive => $class,  %args; # same
	$hybrid = new Object::Hybrid $class;                       # same, but $hybrid is constructed for a given class
	$hybrid = new Object::Hybrid $class, \%args;               # same, but with named arguments
	$hybrid = new Object::Hybrid $class,  %args;               # same
	$hybrid = new Object::Hybrid \%args;                       # same, but with named arguments
	$hybrid = new Object::Hybrid  %args;                       # same

Or corresponding direct method call notation for any of the above can be used, for example:

	$hybrid = Object::Hybrid->new($primitive); # etc.

The new() constructor promote()s $primitive to hybrid and returns it. It is roughly equivalent to:

	sub new { shift; return promote(@_) }

Refer to promote() documentation.

Note that new() do not construct object of Object::Hybrid class, even not $hybrid->isa('Object::Hybrid'), so beware.

=head1 tie() method

	$tied = Object::Hybrid->tie( $primitive, $tieclass, @args); # for %$primitive same as...
	$tied =                 tie(%$primitive, $tieclass, @args); # ... except $primitive also gets promote()d to hybrid

=head1 is() method

	promote            $hybrid;
	Object::Hybrid->is($hybrid);            # true
	Object::Hybrid->is($hybrid) eq $hybrid; # true, useful for chaining methods or in expressions
	Object::Hybrid->is($not_hybrid);         # false

Tells whether argument is a hybrid object, returning it if yes.

=head1 class() method

	package Foo;
	use Object::Hybrid;
	use Object::Hybrid::Class;

	Object::Hybrid->class() eq 'Object::Hybrid::CLASS'; # true
	Object::Hybrid->class('Foo');          # true
	Object::Hybrid->class('Foo') eq 'Foo'; # true, useful for chaining methods or in expressions
	Object::Hybrid->class('Bar');          # false

Argumentless returns default hybrid class name. Otherwise tells whether argument class has been labeled as hybrid class (see L</"use Object::Hybrid::Class">, returning it if yes.

=head1 ref_*() methods

These are utility methods useful to sort things out. Generally, all ref_foo() (ref_*()) methods return boolean that tells whether its argument is reference or not, but exact boolean value depends of the value of 'foo' suffix:

=head2 ref_type() method

	Object::Hybrid->ref_type({})              eq 'HASH'; # true
	Object::Hybrid->ref_type(bless {}, 'Foo') eq 'HASH'; # true

and so on...

=head2 ref_isa() method

	$obj = bless {}, 'Foo';
	Object::Hybrid->ref_isa($obj);                # true
	Object::Hybrid->ref_isa($obj)        eq $obj; # true
	Object::Hybrid->ref_isa($obj, 'Foo') eq $obj; # true
	Object::Hybrid->ref_isa($obj, 'Bar');         # false
	Object::Hybrid->ref_isa({});                  # false

and so on...

This method is useful to try some unknown $thing at hands that it is too uncertain to call $thing->isa('Foo') on it. It returns true, more specifically passes through its argument (for use in expressions and chained calls) if reference is blessed and, if second argument defined, isa() of second argument type (exact type can be obtained with ref(ref_isa($var)) instead). Otherwise returns false. More specifically, returns 0 for blessed references, '' for non-blessed references and undef for non-references.

=head2 ref_tied() method

	$tied =             tie %$primitive, 'Foo';
	Object::Hybrid->ref_tied($primitive) eq $tied; # true
	Object::Hybrid->ref_tied({})         eq '0';   # true
	Object::Hybrid->ref_tied(sub{})      eq '';    # true, since sub{} is not tieable
	Object::Hybrid->ref_tied('string')   eq '';    # true, since 'string' is not tieable

and so on...

=head1 Subclassing Object::Hybrid

Subclassing Object::Hybrid and overriding new() in the subclass will automatically override promote() exported by that subclass, so there is no need to explicitly redefine promote() in subclass.

=head1 Perltie classes

Hybrid objects are compatible with any valid tieclass.

However, to support workarounds for "gaps" in perltie implementation, tieclasses may need to meet additional requirements - those are some of requirements that hybrid classes already comply with, intended specifically to work around perltie implementation gaps, namely: L</"Complete perltie API"> and L</"self() method">. Currently tie()ing filehandles is still incomplete in Perl, so these requirements mainly apply to tiehandle classes. The most simple tiehandle classes, like Tie::StdHandle (loaded by "use Tie::Handle"), already comply with this requirements, as for them default self() and other methods provided by default hybrid class are good enough. A bit more complex tiehandle classes need just to implement self() method. If defining self() is not possible in case of more complex tiehandle classes, additional SYSOPEN(), TRUNCATE(), FLOCK(), FCNTL(), STAT() and FTEST() methods may need to be implemented as workarounds by tiehandle class.

Since tie() do not pass primitive to be tie()d to TIE*() constructor, TIE*() cannot be made to optionally promote() that primitive. Instead, tieclass can expose promote() as one of its methods allowing user to promote primitives or expose Object::Hybrid->tie() method analog for built-in tie() that both tie()s and promote() given primitive. This, however, should probably not be dove via subclassing.

=head1 Hybrid classes

Primitives can be promote()d by blessing it into custom hybrid class specified as argument. There are few advantages of using custom hybrid classes, in particular - opportunity for operator overloading. 

Custom hybrid class is required to subclass Object::Hybrid::CLASS (the default hybrid class) and is to meet certain other requirements, but all those requirements are met automatically by subclassing Object::Hybrid::CLASS, which makes construction of custom hybrid classes easier. For example, custom subclass of Object::Hybrid::CLASS may just define overloading of some operators, etc.  

Although subclassing Object::Hybrid::CLASS allows to avoid building it from scratch, still hybrid class requirements need to be understood for extending Object::Hybrid::CLASS in subclasses.

Promoting primitive to become hybrid (i.e. bless()ing it into hybrid class) simply adds object interface to primitive and is nothing more than a way to extend Perl primitives. There are many ways hybrid class can be designed. However, designing hybrid class in a special way allows to achieve compatibility and synergy with another major way of extending primitives - perltie API, as well as some other goals. 

Because of this, hybrid class is required to meet specific set of requirements. As a result of these requirements, which narrow design of hybrid class/interface, hybrids are useful not only as simple syntactic sugar, but become compatible with and complementary to using perltie API/classes for extending primitives. 

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

=item self() method

Any method of the hybrid object has only that object to operate on using either other hybrid methods or manipulating bless()ed primitive, either tie()d or not, directly. However, many tied() objects (like Tie::ExtraHash) transparently delegate operations on tie()d primitive to real primitive encapsulated somewhere inside that tied() object, using object just to store some additional state. If this is the case, tied() class may define self() as accessor for that underlying primitive to expose it to methods of the hybrid class and to users. As a result, methods of hybrid class would have access to both tie()d primitive and underlying real primitive. This can have a number of useful applications, in particular to work around gaps in tiehandle implementation and to increase performance, as it allows hybrid methods to bypass perltie layer and operate on underlying primitive directly, which may be bring large efficiency benefits for some bulk operations on underlying primitive. And in case of not tied() primitive self() simply returns that primitive.

For example, since there is no yet perltie support for stat() and -X tests, called on tiehandle they do not propagate to underlying real filehandle, so they should be somehow propagated manually, but it requires knowing how to get underlying filehandle, if there is any, out of tied() object. Defining self() method in tieclass is supposed to do just that, and hybrid classes are expected to define self() method as well, so that portable code can simply be:

	promote $FH;

	stat $FH->self;
	-X   $FH->self;

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

To mark valid hybrid class as such, put this lines into it:

	package CustomHybridClass;	
	use Object::Hybrid;
	use Object::Hybrid::Class;

This requirement allows to use the following test to find out if $class is a hybrid class:

	Object::Hybrid->class($class);

This test returns true for CustomHybridClass and all its subclasses, since subclasses of valid hybrid class are unlikely to (intentionally) make it invalid, but is not necessarily true in super-classes, as those may be not be (marked as) valid hybrid classes.

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

=head1 Operator overloading

Custom hybrid classes can be used for overloading operators on promote()d primitives. However, unfortunately hybrid classes with overloaded operators currently can only be used to promote() non-tied() primitives. This is because currently overload pragma is broken - bless()ing tie()d primitive into such class will implicitly untie() it. Should this be fixed in the future, operator overloading can be used without this limitation.

However, even used on non-tied() primitives operator overloading is very powerful and can have some interesting (and possibly even useful) applications. In particular, overloading of dereference operators allows to achieve effects somewhat similar to using tie(), like "back-door" state similar to that of tied() objects. It is even possible to have "hybrid primitive" that is simultaneously hash, scalar, array, subroutine and glob (however such hybrid class may violate equivalence requirement as FETCH(0) need to be equivalent to $hybrid->{0} and $hybrid->[0] at the same time). 

=head1 TODO

Currently tests cover only tiehashes and tiehandles, there should be tests for other types as well.

As soon as (and if) Object::Hybrid interface stabilizes enough, its version is to jump to 1.0.

=head1 SEE ALSO

The C<use autobox> pragma is another way of doing somewhat similar, but not the same things, so autobox and Object::Hybrid are not mutually substitutive. However, if C<autobox> pragma is installed, then either "autobox" or "autopromote" feature can be enabled  - see L</"use Object::Hybrid">.

=head1 SUPPORT

Send bug reports, patches, ideas, suggestions, feature requests or any module-related information to L<mailto:parsels@mail.ru>. They are welcome and each carefully considered.

In particular, if you find certain portions of this documentation either unclear, complicated or incomplete, please let me know, so that I can try to make it better. 

If you have examples of a neat usage of Object::Hybrid, drop a line too.

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

no warnings;  

sub CLASS_TIED      () { 'Object::Hybrid::CLASS'   }
sub CLASS           () { 'Object::Hybrid::CLASS'   }
sub CLASS_HASH      () { 'Object::Hybrid::HASH'   }
sub CLASS_SCALAR    () { 'Object::Hybrid::SCALAR' }
sub CLASS_ARRAY     () { 'Object::Hybrid::ARRAY'  }
sub CLASS_HANDLE    () { 'Object::Hybrid::GLOB'   }
sub CLASS_AUTOPROMO () { 'Object::Hybrid::AUTOPROMOTE' }

my %class4type = (
	HASH   =>   CLASS_HASH,
	ARRAY  =>  CLASS_ARRAY,
	SCALAR => CLASS_SCALAR,
	GLOB   => CLASS_HANDLE,
	CODE   => undef, 
	REF    => undef, 
	LVALUE => undef, 
);

foreach my $class (CLASS_HASH, CLASS_SCALAR, CLASS_ARRAY, CLASS_HANDLE) {
	@{   "${class}2::ISA" } = ($_, CLASS); 
}

my %AD;

__PACKAGE__->import(qw(promote));
sub import {
	my $self = shift;

	# interface...
	my $opt 
	=     @_ > 1          ? {@_} 
	:    !@_              ? {  } 
	: ref $_[0] eq 'HASH' ?  $_[0] 
	: {          feature => [$_[0]] };

	# normalize %$opt...
	foreach my $list (qw(feature export autopromote)) { 
		next 
		if !exists $opt->{$list};
		ref        $opt->{$list} eq 'ARRAY' 
		or         $opt->{$list} 
		=         [$opt->{$list}];
	}

	my @goto;

	# process features first...
	foreach my $feature (ref $opt->{feature} eq 'ARRAY' ? @{$opt->{feature}} : $opt->{feature}) {
		if ($feature eq 'promote') { push @{$opt->{export}}, $feature }

		# mutually exclusive features...
		if ($feature eq 'autobox') {
			unless ($opt->{$feature}) {
				_load_class($self->CLASS);
				_load_class($self->CLASS_HASH);
				_load_class($self->CLASS_SCALAR);
				_load_class($self->CLASS_ARRAY);
			}
			require 
			autobox;
			autobox::import( ref($self)||$self, 
			HASH   =>  $opt->{$feature}||$self->CLASS, # method instead of constant, for subclassing...
			SCALAR =>  $opt->{$feature}||$self->CLASS,
			#GLOB  =>  $opt->{$feature}||$self->CLASS, # not supported by autobox
			ARRAY  =>  $opt->{$feature}||$self->CLASS, );
		}
		elsif ($feature eq 'autopromote') {
			require 
			autobox;
			autobox::import( ref($self)||$self, 
			HASH           => CLASS_AUTOPROMO,
			SCALAR         => CLASS_AUTOPROMO,
			#GLOB          => CLASS_AUTOPROMO, # not supported by autobox
			ARRAY          => CLASS_AUTOPROMO, );
			my $autoload 
			=      __PACKAGE__ . '::AUTOLOAD'; 
			*{ CLASS_AUTOPROMO . '::AUTOLOAD' } = sub{ 
				$self->new($_[0], @{$opt->{$feature}}||()); 
				$$autoload =~ s/^.*:://;
				goto &{ $_[0]->can($$autoload) 
				or croak(_cant_locate_object_method($_[0], $$autoload)) };
			};
		}
	}

	# process options...
	if ($opt->{export}) {
		my @symbols;
		foreach my $symbol (@{$opt->{export}||[]}) {
			if (   $symbol eq 'promote' ) { 
				*{join '::', scalar caller, $symbol} 
				= sub{ unshift @_, $self; goto &{ $self->can(qw(new)) } };
			}
			else { push @symbols, $symbol }
		}

		if (@symbols 
		or  @{(ref($self)||$self).'::EXPORT'}
		or  @{        __PACKAGE__.'::EXPORT'}) {
			require 
			Exporter;
			Exporter::export_to_level(1, $self,       @symbols) or 
			Exporter::export_to_level(1, __PACKAGE__, @symbols); # "inheritance" of export, subclasses can define their own @EXPORTs,
		}
	}

	if (@goto) {
		@_ = @goto;
		goto &{shift(@_)};
	}
}

sub class { @_ <= 1 ? CLASS : Object::Hybrid::Class->is($_[1]) ? $_[1] : undef }
sub is    {        ref $_[1] && Object::Hybrid::Class->is($_[1]) ? $_[1] : undef }

sub new { 
	@_>1 or croak("Error: Nothing to promote ");
	my $self = shift;
	my $primitive = $_[0]; # keep $_[0] to retain alias and autovivification ability...
	my                   ($args,  $class);
	ref $_[1] eq 'HASH' ? $args : $class = splice @_, 1, 1 unless @_ - 2*int(@_/2); # @_ is odd
	%$args = (%$args, @_[1..$#_]); 

	# be idempotent...
	return  $primitive 
	if  ref $primitive eq ($class||CLASS);

	!   _ref_isa($primitive) 
	or $self->is($primitive)
	or croak("Can't promote non-hybrid object $primitive");
	my $tied_primitive = _ref_tied($primitive);

	_load_class(CLASS) if $class and $class->isa(CLASS); 

	unless (         $class #and !$tied_primitive||$args->{not_tied}
	and $self->class($class) 
	&&      _can_tie($class, $primitive) # autovivifies $primitive
	|| croak("Error: Wrong hybrid class $class, either not labled as such or defines no proper perltie methods ") 
	) { 
		my $use_class = CLASS_TIED # must be default to avoid promote() results depend on the order of tie() and promote()
		unless !$tied_primitive 
		&& $args->{wont_tie} 
		or $args->{not_tied};
		$class 
		=  _ref_type($primitive) eq 'HASH'   ? _load_class($use_class, CLASS_HASH)
		:  _ref_type($primitive) eq 'SCALAR' ? _load_class($use_class, CLASS_SCALAR)
		:  _ref_type($primitive) eq 'ARRAY'  ? _load_class($use_class, CLASS_ARRAY)
		:  _ref_type($primitive) eq 'GLOB'   ? _load_class($use_class, CLASS_HANDLE) 
		:   ref      $primitive 
		?  croak("Error: Can't promote since $primitive either non-tie()able or already bless()ed")
		:  croak("Error: Nothing to promote");

		#eval "require $class" # only built-in classes
		#or die("Can't lload $class");
	}

	my 
	$bless = $class->can('bless');
	$bless ? $class->$bless($primitive)
	:                 bless $primitive, $class; 

	$_[0] ||= $primitive; # autovivify in case of method call (otherwise prototype ensures autovivification)
	return    $primitive 
}

sub tie {
	return undef 
	if !ref $_[1];

	my $tied 
	= $_[1] =~ m'(?:^|=)HASH'   ? tie( %{$_[1]}, @_[2..$#_] )
	: $_[1] =~ m'(?:^|=)SCALAR' ? tie( ${$_[1]}, @_[2..$#_] )
	: $_[1] =~ m'(?:^|=)ARRAY'  ? tie( @{$_[1]}, @_[2..$#_] )
	: $_[1] =~ m'(?:^|=)GLOB'   ? tie( *{$_[1]}, @_[2..$#_] )
	:         undef 
	or return undef;

	Object::Hybrid->new($_[1]); 
	return $tied
}

sub _load_class {
	foreach (@_) {
		next if !$_;
		eval( !exists $AD{$_} ? "require $_" 
		:             $AD{$_} ); 
		undef         $AD{$_}; # be idempotent
		!$@ or croak("Error: Can't load $_: $@");
	}
	return( (grep $_, @_)[0] ) # same as $_[0] || $_[1] || ...
}

sub can_tie  { shift; _can_tie(@_) }
sub _can_tie { # if not given, autovivifies (in place) primitive of type that can be tie()d with given class
	#my  ($tie_to, $primitive) = @_;
	return undef 
	if            !$_[0];
	#or    ref     $_[0] 
	#and !_ref_isa($_[0]);

	return ref $_[1] 
	?    eval{ $_[0]->can(  "TIE".ref $_[1]) } ?    $_[1] : undef
	:    eval{ $_[0]->can(qw(TIEHASH))       } ? \%{$_[1]}
	:    eval{ $_[0]->can(qw(TIESCALAR))     } ? \${$_[1]}
	:    eval{ $_[0]->can(qw(TIEARRAY))      } ? \@{$_[1]}
	:    eval{ $_[0]->can(qw(TIEHANDLE))     } ? \*{$_[1]}
	:    undef
}

sub ref_tied  { shift; _ref_tied(@_) }
sub _ref_tied2 { # this is much faster at least for tied hashes, but for non-tied tries all variants (still may be faster)
	return undef if !ref $_[0];
	return eval{ tied( %{$_[0]} ) }
	||     eval{ tied( ${$_[0]} ) }
	||     eval{ tied( @{$_[0]} ) }
	||     eval{ tied( *{$_[0]} ) } 
	||     undef
}
sub _ref_tied {
	return undef 
	if !ref $_[0];
	return  $_[0] =~ m'(?:^|=)HASH'   ? tied( %{$_[0]} )||0
	:       $_[0] =~ m'(?:^|=)SCALAR' ? tied( ${$_[0]} )||0
	:       $_[0] =~ m'(?:^|=)ARRAY'  ? tied( @{$_[0]} )||0
	:       $_[0] =~ m'(?:^|=)GLOB'   ? tied( *{$_[0]} )||0
	: undef
}

sub ref_type  { shift; _ref_type(@_) }
sub _ref_type {
	return undef if !ref $_[0];
	return $1    if      $_[0] =~ /=(\w+)/;
	return           ref $_[0]
}

sub ref_isa  { shift; _ref_isa(@_) }
sub _ref_isa { 
	return undef if !ref    $_[0];
	return ''    if exists $class4type{ref $_[0]};  
	return 0     if defined $_[1] and     !$_[0]->isa($_[1]);
	return $_[0]
}

sub croak { require Carp; goto &Carp::croak; }

sub _alter_case  { $_[0] =~ /[A-Z]/ ? lc($_[0]) : uc($_[0]) };
sub method_alias { _alter_case($_[1]) }

sub methods {
	shift;
	my $subs;
	ref $_[0] eq 'HASH' ? ($subs) : %$subs = @_;
	my $caller = caller;
	foreach my $method (keys %$subs) {
		# explicit aliases...
		ref       $subs->{$method}   eq 'CODE' 
		or ref(   $subs->{$method} 
		= $subs->{$subs->{$method}}) eq 'CODE' 
		or next; 

		# implicit altered-case aliases...
		my     $method2 = _alter_case($method);
		my     $goto;
		*{join '::', $caller, $method } = 
		*{join '::', $caller, $method2} = $subs->{$method};

	}
}

sub _cant_locate_object_method {
	join '', "Object::Hybrid: Can't locate object method \""
	, $_[1], "\" via package \""
	, ref($_[0])||$_[0], "\" (perhaps you forgot to load \""
	, ref($_[0])||$_[0], "\"?) "
}

BEGIN {

	$INC{  "Class\/Tag.pm" } ||= 1; 
	package Class::Tag;

	sub ANTICOLLIDER () { 'aixfHgvpm7hgVziaO' }
	sub _tags_accessor  { _subnames( join '_', $_[0], ANTICOLLIDER(), $_[1] ) }

	sub _subnames { my $a; ($a = $_[0]) =~ s/:/_/g; return $a }

	sub import {
		shift;
		my $tag_class = caller;

		$INC{ join '/', split '::', "$tag_class.pm" } ||= 1; # support inlined tag classes 

		*{join '::', $tag_class, 'import'} = sub{
			shift;
			my   $tags;
			ref          $_[0] eq 'HASH' 
			? (  $tags = $_[0] ) 
			: ( @$tags{  @_ } = (1) x @_ );

			!exists $tags->{is} 
			and     $tags->{is} = 1;

			foreach (keys %$tags) {
				my  $tag = bless \$tags->{$_}, $tag_class; # wrap value of the tag to distingush it from possible AUTOLOAD()ed return values
				*{join '::', scalar caller, _tags_accessor($tag_class, $_) } = sub{ $tag };
			}
		};

		foreach ( @_ ? @_ : 'is' ) {
			my $tags_accessor = _tags_accessor($tag_class, $_);
			*{join '::', $tag_class, $_ } = sub{
				my $tag =   @_ > 1  # called as method
				? eval {    $_[1]->$tags_accessor   }
				: eval { &{"$_[0]::$tags_accessor"} };
				return ref $tag eq $tag_class ? $$tag : undef
			} 
		}
	}

}

{ package Object::Hybrid::Class; use Class::Tag; } # inlined tag class

#$AD{   'Object::Hybrid::CLASS' } = <<'CLASS'; 
$INC{  "Object\/Hybrid\/CLASS.pm" } ||= 1;
package Object::Hybrid::CLASS;

use Object::Hybrid::Class; # just labeling

sub can;  
#sub isa; 

Object::Hybrid->methods( 
	SELF => sub { 
		my      $self; 
		return  $self
		if      $self 
		=   Object::Hybrid::ref_tied($_[0])
		and Object::Hybrid::ref_type($_[0]) 
		eq  Object::Hybrid::ref_type($self) 
		and ref($self) =~ /Std/;

		!$self or die("tied() class defines nor valid self() method for $_[0] (self() returned $self)");

		return $_[0]
	},
);

my $AUTOLOAD = \&AUTOLOAD;
sub AUTOLOAD {
	package Object::Hybrid; # to not qualify _ref_tied(), _ref_type(), croak(), etc...

	my $swap
	;  $swap = splice(@_, 0, 1, $swap) 
	if $swap 
	=  eval{ tied( %{$_[0]} ) }
	|| eval{ tied( ${$_[0]} ) }
	|| eval{ tied( @{$_[0]} ) }
	|| eval{ tied( *{$_[0]} ) }
	|| undef; 

	$Object::Hybrid::CLASS::AUTOLOAD =~ s/^.*:://;
	$Object::Hybrid::CLASS::AUTOLOAD eq 'can' and my $can_method = $_[1];

	my 
	$method;
	$method
	=  $_[0]->UNIVERSAL::can(            $can_method||$Object::Hybrid::CLASS::AUTOLOAD ) 
	|| $_[0]->UNIVERSAL::can(_alter_case($can_method||$Object::Hybrid::CLASS::AUTOLOAD))
	if $swap 
	or $can_method; 

	if (!$method) { 
		splice(@_, 0, 1, $swap)	if $swap; # revert swap, if any
		my 
		$subclass;
		#$subclass = $class4type{_ref_type($_[0])}||'FOO', # instead inlining...
		$subclass  = $class4type{!ref $_[0] ? undef : $_[0] =~ /=(\w+)/ ? $1 : ref $_[0]}||'FOO',
		$method
		=  $subclass->UNIVERSAL::can(            $can_method||$Object::Hybrid::CLASS::AUTOLOAD ) 
		|| $subclass->UNIVERSAL::can(_alter_case($can_method||$Object::Hybrid::CLASS::AUTOLOAD));
	}

	return $method if $can_method;

	$method 
	or croak( _cant_locate_object_method($_[0], $Object::Hybrid::CLASS::AUTOLOAD) );

	$method ne $AUTOLOAD
	#and defined(&$method) # here goto() to not defined(&method) is ok as it may be autoloadable in tied() class or otherwise
	or croak( join '', "Undefined method \""
	, $Object::Hybrid::CLASS::AUTOLOAD, "\" called via package \""
	, ref($_[0])||$_[0], "\"");

	goto &$method
}

#CLASS

$AD{   'Object::Hybrid::HASH' } = <<'CLASS';
$INC{  "Object\/Hybrid\/_HASH.pm" } ||= 1;
package Object::Hybrid::HASH;

use Object::Hybrid::Class; # just labeling

Object::Hybrid->methods({
	self     => sub { $_[0] },
	TIEHASH  => sub { bless {}, ref($_[0])||$_[0] },
	STORE    => sub { $_[0]->{$_[1]} = $_[2] },
	FETCH    => sub { $_[0]->{$_[1]} },
	FIRSTKEY => sub { my $a = scalar keys %{$_[0]}; each %{$_[0]} },
	NEXTKEY  => sub { each %{$_[0]} },
	EXISTS   => sub { exists $_[0]->{$_[1]} },
	DELETE   => sub { delete $_[0]->{$_[1]} },
	CLEAR    => sub { %{$_[0]} = () },
	SCALAR   => sub { scalar %{$_[0]} },
});

sub DESTROY {}

CLASS

$AD{   'Object::Hybrid::SCALAR' } = <<'CLASS';
$INC{  "Object\/Hybrid\/_SCALAR.pm" } = 1;
package Object::Hybrid::SCALAR;

use Object::Hybrid::Class; # just labeling

Object::Hybrid->methods({
	self      => sub { $_[0] },
	TIESCALAR => sub {
		my $class = shift;
		my $instance = shift || undef;
		return bless \$instance => $class;
	},
	FETCH   => sub { ${$_[0]}  },
	STORE   => sub { ${$_[0]} = $_[1] },
});

sub DESTROY { undef ${$_[0]} }

CLASS

$AD{   'Object::Hybrid::ARRAY' } = <<'CLASS';
$INC{  "Object\/Hybrid\/_ARRAY.pm" } ||= 1;
package Object::Hybrid::ARRAY;

use Object::Hybrid::Class; # just labeling

Object::Hybrid->methods({
	self      => sub { $_[0] },
	TIEARRAY  => sub { bless [], $_[0] },
	FETCHSIZE => sub { scalar @{$_[0]} },
	STORESIZE => sub { $#{$_[0]} = $_[1]-1 },
	STORE     => sub { $_[0]->[$_[1]] = $_[2] },
	FETCH     => sub { $_[0]->[$_[1]] },
	CLEAR     => sub { @{$_[0]} = () },
	POP       => sub { pop(@{$_[0]}) },
	PUSH      => sub { my $o = shift; push(@$o,@_) },
	SHIFT     => sub { shift(@{$_[0]}) },
	UNSHIFT   => sub { my $o = shift; unshift(@$o,@_) },
	EXISTS    => sub { exists $_[0]->[$_[1]] },
	DELETE    => sub { delete $_[0]->[$_[1]] },
	EXTEND    => sub {},
	SPLICE    => sub {
		my $ob  = shift;
		my $sz  = $ob->FETCHSIZE;
		my $off = @_ ? shift : 0;
		$off   += $sz if $off < 0;
		my $len = @_ ? shift : $sz-$off;
		return splice(@$ob,$off,$len,@_);
	},
});

sub DESTROY {}

CLASS

$AD{   'Object::Hybrid::GLOB' } = <<'CLASS';
$INC{  "Object\/Hybrid\/_GLOB.pm" } ||= 1;
package Object::Hybrid::GLOB;

use Object::Hybrid::Class; # just labeling

sub new { 
	goto &{ $_[0]->can(qw(TIEHANDLE))
	||Object::Hybrid::croak("Method not defined:  new() / TIEHANDLE()") } 
}

Object::Hybrid->methods({
	self      => sub { $_[0] },
	TIEHANDLE => sub { 
		my ($elf, $fh, @open_args) = @_;

		if ($fh eq '') {
			$fh = \do { local *HANDLE }; 
		} else {
			eval{ $fh = *$fh }, !$@ or Object::Hybrid::croak("Not a GLOB reference");
		}

		$fh->OPEN(@open_args) or Object::Hybrid::croak($!)
		if        @open_args;

		return bless $fh, ref($elf)||$elf
	},

	OPEN => sub {
		defined $_[0]->FILENO
		and     $_[0]->CLOSE;

		@_ == 2 
		? open($_[0], $_[1]) 
		: open($_[0], $_[1], $_[2]);
	},

	WRITE2 => sub { 
		my    $fh = $_[0];
		print $fh substr($_[1],0,$_[2])
	},
	WRITE    => sub { my $fh = shift; write  $fh    },
	PRINT    => sub { my $fh = shift; print  $fh @_ },
	PRINTF   => sub { my $fh = shift; printf $fh @_ },

	READ     => sub { read     $_[0], $_[1], $_[2] },
	READLINE => sub { my $fh = $_[0]; <$fh> },
	GETC     => sub { getc     $_[0] },

	EOF      => sub { eof      $_[0] },
	TELL     => sub { tell     $_[0] },
	FILENO   => sub { fileno   $_[0] },
	SEEK     => sub { seek     $_[0], $_[1], $_[2] },
	CLOSE    => sub { close    $_[0] },
	BINMODE  => sub { binmode  $_[0] },

	SYSOPEN  => sub { 
		eval {
			@_ >= 3 or Object::Hybrid::croak("Not enough arguments for sysopen()");
			@_ == 3 ? sysopen $_[0]->self, $_[1], $_[2] : 
			@_ >= 4 ? sysopen $_[0]->self, $_[1], $_[2], $_[3] :();
		};
		!$@ or Object::Hybrid::croak($@);
	}, 
	FCNTL    => sub { 
		eval {
			@_ >= 3 or Object::Hybrid::croak("Not enough arguments for fcntl()");
			fcntl $_[0]->self, $_[1], $_[2];
		};
		!$@ or Object::Hybrid::croak($@);
	}, # TODO: same as for SYSOPEN()
	STAT     => sub { stat     $_[0]->self },
	FLOCK    => sub { flock    $_[0]->self, $_[1] },
	TRUNCATE => sub { truncate $_[0]->self, $_[1] },
	FTEST    => sub { 
		my $file = $_[0]->self;
		if ($_[1] =~ /^-\w$/) {
			eval "$_[1] \$file";
			!$@ or Object::Hybrid::croak($@);
		}
		else { Object::Hybrid::croak("Unknown argument to FTEST()") }
	},

});

#sub DESTROY;
#sub UNTIE;
sub DESTROY {}

CLASS

1;

