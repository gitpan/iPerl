# the iPerl engine Text::iPerl.pm
#
# Copyright © 1999, 2000, Daniel Pfeiffer <occitan@esperanto.org>
#
# iPerl may be copied only under the terms of either the Artistic License or
# the GNU General Public License, which may be found in the Perl 5.0 source kit.
#
# Info and latest version are at http://beam.to/iPerl/

=head1 NAME

Text::iPerl - engine for bringing any text documents alive
with bits of embedded Perl

=head1 SYNOPSIS

  use Text::iPerl;
  include 'documentname';

or

  perl -MText::iPerl -e include <infile >outfile

See L<iperl> for a far more comfortable command-line variant

=head1 DESCRIPTION

This is the engine of an inverse Perl interpreter, which controls normal text
with macro invocations and specially marked bits of Perl.  This setup of the
document is always the same, though details may vary according to the style in
effect.  (See C<set_style>.)  The engine is invoked with C<include>, or its
variants C<include_filehandle> and C<include_string>.

=head2 Macros

Normal text gets output as is, unless we have macro-definitions.  If macros
are defined, at runtime every bit of normal text gets repeatedly scanned for
macros, which are expanded until no more macro invocations are found,
i.e. macro expansions occur depth-first.  Macros are functions returning a
string.  If they also print something, that comes in the output stream before
the returned string and is not subject to repeated scanning.  Scanning starts
again where the last macro was found, so if a macro returns what might be the
second part of a macro name together with the preceding text, that is not
found.  (See C<define>, C<undefine> & C<macro>.)

Macro invocations consist of the macro name, a string of letters, digits and
underscores, optionally immediately followed by a parenthesized Perl parameter
list.  Depending on the style, macro invocations may be surrounded by
additional syntactic sugar.

=head2 Empedded Perl

Bits of Perl to be evaluated have to be specially marked up as such.  How this
is done differs greatly depending on the style in effect.  But apart from
different syntaxes there are only two fundamental ways in which Perl can be
embedded: non-printing and printing.  Not all styles provide both ways.  The
difference between these two ways is to be seen as default-functionality and
is not restrictive.  Non-printing Perl may very well use the print statement,
or system-commands to output something via C<STDOUT> into the output stream.
If system-commands are used you should first turn on autoflushing (C<$| = 1>)
to ensure that output order is preserved.

The whole document is actually transformed into a Perl-programme, where each
bit of normal text gets transformed into a semicolon-terminated
Perl-statement.  The markup around bits of non-printing Perl simply gets
removed and a terminating semicolon added, which almost never hurts.  If you
want a bit of non-printing Perl to control the preceding or following bit of
normal text, you can prevent the semicolon by starting or ending the Perl code
with C<\;>.  You can delimit the bit of normal text with a bit of non-printing
Perl containing only a semicolon.  Printing bits of Perl, on the other hand,
get passed as an argument list to a print statement.  If a printing bit of
Perl is empty, C<$_> is printed.  If it is a literal integer, C<$_[I<n>]> is
printed.

There are several interesting things you can do with syntactically incomplete
bits of Perl.  You can seal the fate of the following bit of plain text by
preceding it with an expression followed by C<and> or C<or> and terminated
with C<\;>.  Or you can have dangling curly braces of an
C<if-elsif-else>-statement.  They might also be of a loop, which will likely
contain one or more printing bits of Perl.

Dangling curly braces may even be of a sub, which will then print the
contained plain text when called.  Likewise they may be of an anonymous sub
which could be the second argument to C<define>.

=head1 @EXPORT

Text::iPerl exports the following functions by default:

C<include>, C<include_string>, C<include_filehandle>, C<define>, C<undefine>,
C<macro>

=head2 @EXPORT_OK

Text::iPerl optionally exports the following function and variables:

C<set_style>, C<$cache>, C<$debug>, C<$documents>, C<@documents>, C<$header>,
C<$max_macro_growth>, C<$max_macro_expansions>

=cut

package Text::iPerl;

use 5.004;

use Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(include include_string define undefine macro);
@EXPORT_OK = qw(set_style $style @autostyle_by_name @autostyle_by_contents
		$cache $debug $documents @documents $header $max_macro_growth
		$max_macro_expansions);
$VERSION = 0.52;


@include = ('/usr/include', @INC);

$style = 'auto';
@autostyle_by_name =
    ('\.(?:p[cChH]|p[ch]pp|[cChH]p?|[ch]ppp?)$' => cpp,
     '\.(?:pm[4c]|m[4c]p?)$' => m4,
     '\.(?:ppod|podp?|pm|pl)$' => pod,
     '\.(?:p(?:sg|ht|w|x)ml|(?:sg|ht|w|x)mlp?)$' => sgml);
@autostyle_by_contents =
    ('^|[][\s\S]*' => control,
     '<script\s[^>]*runat\s*=\s*[\'\"]?server\b.*?>|<(?:server|perl)\b.*?>' => sgml,
     'perl\((?:<\s*[\s\S]*?\s*>|\{\s*[\s\S]*?\s*\}|\})\)' => m4,
     '^=perl\b|[PM]<[\s\S]*?>' => pod,
     '<\{[\s\S]*?\}>' => sgml,
     '!\{[\s\S]*?\}!|!<[\s\S]*?>!|^!' => bang,
     '^#' => cpp);

$max_macro_expansions = $max_macro_growth = 1000;

$package = '';



sub compile(;$$$);



# internal utilities

sub case($@) {
    my( $str, @conds ) = @_;
    while( @conds ) {
	return $conds[1] if
	    $str =~ /$conds[0]/m;
	splice @conds, 0, 2;
    }
    undef;
}

sub _eval($) {
#local $^W=1;
    my $result = eval( $package ? "package $package; $_[0]" : $_[0] );
    die $@ if $@;
    $result;
}

sub set_style($;@);


# Most functions which don't return anything meaningful, explicitly return
# '', so as to be useable as (or - like output - implicitly in) a macro.

=head1 FUNCTIONS

=over

=item define I<STRING>, I<EXPR>

=item define I<STRING>

=item define

Defines a macro whose value may be interpolated into bits of plain text in
scalar context.  I<STRING> or C<$_> should be a string consisting of letters,
digits and underscores, which is the name of the macro.  I<EXPR>
is the body of the macro.  If it is a reference to a function the macro
interpolation will call that.  If it is a string-reference the macro is an
alias to that macro or to that Perl-builtin, which doesn't allow a function
reference to be taken.  If it is missing, the macro is a
soft reference to a Perl-function of the same name.

If the second argument is a string (should be single-quoted), its variables
will be interpolated at the moment the macro gets called.  The macro arguments
may of course be accessed as C<'... $_[0] ... $_[1] ...'>, but there is a more
comfortable possibility.  The first argument to C<define> may contain
parameter specifications in parantheses after the name.  These are a comma
separated list of scalar variables with optionally a list variable at the end.
Each of these variables may be assigned to, giving the named parameter a
default value.

For styles like C<cpp> which don't allow embedding Perl-expressions into the
document, you can use any one of the following to get a Perl-evaluating macro:

  define PERL => sub { $_[0] };
  define PERL => '@_';
  define 'PERL( $eval = $_ )', '$eval';

=cut

sub define(;$$) {
    my( $macro, $args ) = split /(?=[^a-zA-Z0-9_])/, $_[0], 2;
    $macro = $_ unless $macro;
    my $str = $_[1];
    if( ref $str ) {
	$defines{$macro} = $str;
    } elsif( defined $args ) {
	$args =~ s/^\s*\(\s*(.*)\s*\)\s*$/$1/;
	my @defaults;
	my @args_nodefaults;
	foreach( split /\s*,\s*/, $args ) {
	    push @defaults,
		"$1 = $2 unless " . ($1 =~ /^\s*$/ && "defined ") . "$1;"
		if s/(.+)\s*=\s*(.+)/$1/;
	    push @args_nodefaults, $_;
	}
	if( @args_nodefaults ) {
	    local $" = ',';
	    $args = "my( @args_nodefaults ) = \@_;";
	} else {
	    $args = '';
	}
	if( defined $str ) {
	    $str =~ s/"/\\"/g;
	    $defines{$macro} = _eval "sub {
		$args
		@defaults
		\"$str\"
	    }";
	}
    } elsif( defined $str ) {
	$str =~ s/"/\\"/g;
	$defines{$macro} = _eval "sub {
	    \"$str\"
	}";
    }
    $defines .= ($defines && '|') . $macro
	unless $defines =~ /(?:^|\|)$macro(?:\||$)/;
    '';
}



=item include I<EXPR>, I<REPEATCOUNT>, I<HUSH>

=item include I<EXPR>, I<REPEATCOUNT>

=item include I<EXPR>

=item include

Includes a document, parsing it as iPerl and merging the result into the
current output.  If no filename is given, reads from C<STDIN>.  If called from
within a known file, the file is searched in the directory of that
document, else in the current directory.  If it is not found there, the
directories in C<@opt_I> followed by those in C<@include> are searched.

The second argument may be an integer (often C<1>), meaning to include the
file only if that filename hasn't already been included that many times.
Since this can be fooled by multiple links to the same file, or if you use
chdir, the second argument may also be a reference to an integer (e.g. C<\1>).
In that case the physical identity of the file is used, rather than the
filename.

The third argument, when true, means to continue silently when the file was
not found.

=cut

sub include(;$$$) {
    my( $file, $count, $hush ) = @_;
    my $string;
    if( defined( $file ) && $file ) {
	my @include = (@opt_I, @include);
	if( ref $file ) {
	    $file = $$file;
	} elsif( $file =~ /^\// ) {
	    goto CHECK;
	} elsif( $file =~ /^<&|\|$/ ) {
	    goto READ;
	} else {
	    my $dir;
	    foreach( reverse @documents ) {
		next if /[>|]$/;
		($dir = $_) =~ s|/[^/]*?$||;
		last;
	    }
	    unshift @include, ($dir || '.');
	}
	$file =~ s/^<\s*//;
	foreach( @include ) {
	    next unless -f( $_ .= "/$file" );
	    $file = $_;
	    last;
	}
      CHECK:
	my $ref = join ':', (stat $file)[0,1];
	if( ! defined $count ) {
	} elsif( ref $count ) {
	    return unless $$count > $refcount{$ref};
	} elsif( $count ) {
	    return unless $count > $count{$file};
	}
	$refcount{$ref}++;
	$count{$file}++;
	if( $cache && defined $cache{$file} ) {
	    &{$cache{$file}};
	    return '';
	}
      READ:
	local $/ = undef;
	local *FILE;
	unless( open FILE, $file ) {
	    if( defined( $hush ) && $hush ) {
		return;
	    } else {
		die "cannot open $file";
	    }
	}
	$string = <FILE>;
	close FILE;
    } else {
	$file = '<STDIN>';
	if( $cache && defined $cache{$file} ) {
	    &{$cache{$file}};
	    return '';
	}
	local $/ = undef;
	$string = <STDIN>;
    }
    &{$string = compile $string, caller, $file};
    $cache{$file} = $string if $cache;
    '';
}



=item include_filehandle I<FILEHANDLE>

Likewise, but reads from the I<FILEHANDLE>.

  #! /usr/local/bin/perl
  use Text::iPerl;
  include_filehandle DATA;
  __END__
  Self-parsing iPerl document goes here.

=cut

sub IO::Handle::include_filehandle(*) {
    &{compile do {{		# 'do' in 5.004 ignores 'local'
	local *FH = $_[0];
	local $/ = undef;
	<FH>;
    }}, caller, '<FILEHANDLE>'};
    '';
}



=item include_string I<EXPR>

=item include_string

Likewise, but parses I<EXPR> or C<$_> if none.

=cut


sub include_string(;$$$) {
    &{compile @_ ? $_[0] : $_,
	  $_[1] || caller,
	  $_[2] || '<STRING>'};
    '';
}



=item macro I<STRING>

=item macro

Returns undef or the macro-definition of I<STRING>, either as a
code-reference, or the name as a string if the macro is a soft reference to a
Perl function.  Without an argument returns the list of defined macro-names.

  if( defined macro 'mymacro' ) { ... }
  foreach( macro ) { ... }

=cut

sub macro(;$) {
    if( @_ ) {
	return $defines{$_[0]} if $defines{$_[0]};
	return $_[0] if $defines =~ /(?:^|\|)$_[0](?:\||$)/;
	undef;
    } else {
	return sort split /\|/, $defines;
    }
}



sub output($) {
    if( defined $header ) {
	print $header;
	undef $header;
    }
    my $string = $_[0];
    if( $defines ) {
	my $i = 0;
	my $max_length = $max_macro_growth * length $string;
	my( $length, $args, $post ) = ( 0, '' );
	local $macro;
	while( $string =~ /$macro_start\b($defines)\b(\(.*?\))?($macro_end)/s )
	{
	    print $`;
	    $length += length $`;
	    if( $macro_start_dollar1 ) {
		print $1;
		$length += length $1;
		$macro = $defines{$2} || $2;
		$args = $3; $end = $4;
	    } else {
		$macro = $defines{$1} || $1;
		$args = $2; $end = $3;
	    }
	    $post = $';
	    $macro = $defines{$$macro} || $$macro
		while ref( $macro ) eq 'SCALAR';
	    while( (!$end) && $args =~ tr/(/(/ > $args =~ tr/)/)/ ) {
		$post =~ /^(.*?\))($macro_end)/s or
		    die "missing ')' for inlined macro ``$macro''";
		$args .= $1; $end = $2; $post = $';
	    }
#	    $args = substr $args, 1, -1 if $args;
	    $args ||= '()';
	    $package = caller;
	    $macro = _eval "\n#line 0 'inlined macro \"$macro\"'\n" .
		(ref( $macro ) ?
		 "&\$Text::iPerl::macro$args" :
		 "$macro$args");
	    $string = $macro . $post;
	    die 'Factor $Text::iPerl::max_macro_growth exceeded'
		if $max_length < $length + length $string;
	    die '$Text::iPerl::max_macro_expansions exceeded'
		if $max_macro_expansions < ++$i;
	}
    }
    print $string;
    '';
}


sub compile(;$$$) {
    local $prog = '';
    my @_Text_iPerl;
    local( $macro_start, $macro_end, $macro_start_dollar1,
	   $joiner, $splitter, $style, @documents ) =
	( $macro_start, $macro_end, $macro_start_dollar1,
	  $joiner, $splitter, $style, @documents );
    $documents++;
    push @documents, $_[2] || '<STRING>';
    {
	my $string = @_ ? $_[0] : $_;
	if( $style =~ /^auto/ ) {
	    my $orig_style = $style;
	    my $det_style =
		($string =~
		    /\A(?:\#!.+\n)?
		     .*-\*-.*\b iPerl-style:\s*"(.+?)" .*-\*-/mx) ? $1 :
		(substr( $string, -3000 ) =~
		    /^(.*?)[ \t]* Local\ Variables: [ \t]*(.*)$
		     (?:[\s\S]*?)
		     ^\1[ \t]* iPerl-style:\s*"(.+?)" [ \t]*\2$
		     (?:[\s\S]*?)
		     ^\1[ \t]* End: [ \t]*\2$
		     (?:(?!\n).)*\Z/mx) ? $3 :
		case( $documents[-1], @autostyle_by_name ) ||
		case( $string, @autostyle_by_contents );
	    if( $det_style ) {
		set_style $det_style;
		$style = "auto: $style";
	    } elsif( $orig_style eq 'auto' ) {
		die "No style determined for $documents[-1]";
	    }
	}
	my $length;
	while( $string ) {
	    @res = &$splitter( $string );
	    for( $res[0] ) {
		next unless defined and $length = length;
		$prog .= 'Text::iPerl::output ' .
		    (( $length < 20 and ! /[\\']/ ) ?
		     "'$_';" :
		     ('$_Text_iPerl[' .
		      (push( @_Text_iPerl, $_ ) - 1) .
		      '];'));
	    }
	    for( $res[1] ) {
		last unless defined;
		if( /^$joiner/ ) {
		    $_ = $';
		    chop $prog if $prog =~ /;$/s;
		}
		$_ = "\$_[$_]" if $_ eq int;
		$prog .= 'print STDOUT (' . (/$joiner$/ ?
			(($` || '$_') . "\n)") :
			(($_ || '$_') . "\n);"));
	    }
	    for( $res[2] ) {
		last unless defined;
		if( /^$joiner/ ) {
		    $_ = $';
		    chop $prog if $prog =~ /;$/s;
		}
		$prog .= /$joiner$/ ? "$`\n" : "$_\n;";
	    }
	    $string = $res[3];
	}
    }
    if( $debug ) {
	print "\$style = '$style';\n\n";
	my $j = 0;
	foreach( @_Text_iPerl ) {
	    s/\\/\\\\/g;
	    s/'/\\'/g;
	    print "my \$_Text_iPerl[$j] = '$_';\n\n";
	    $j++;
	}
	print "$prog\n";
	exit;
    }

    my( $macro_start_ref, $macro_end_ref,
	   $joiner_ref, $splitter_ref, $documents_ref ) =
	( \$macro_start, \$macro_end,
	  \$joiner, \$splitter, \@documents );
    $prog = eval qq{
	sub {
	    local( \$macro_start, \$macro_end, \$macro_start_dollar1,
		   \$joiner, \$splitter, \$style, \@documents ) =
		( \$\$macro_start_ref, \$\$macro_end_ref, $macro_start_dollar1,
		  \$\$joiner_ref, \$\$splitter_ref, '$style', \@\$documents_ref );
	    package $_[1];
#line 1 '$documents[-1]'
	    $prog
	}
    };
    die $@ if $@;
    $prog;
}


=item return

The normal Perl keyword, returns from a document when used at its top-level,
i.e. outside of functions, macros or macro invocations, save for the m4-style
pseudo-macros.  This means, that the rest of the document is not processed and
output.


=item set_style I<STRING>[, I<ARGUMENT> ...]

Set one of the following iPerl-styles.  The various styles are more or less
adapted to various document types.  But of course any style can be used
anywhere.  Sometimes this requires some extra care, for example HTML documents
may contain the sequence C<!E<lt>> which can lead to startling effects when
used with the bang-style.

It can sometimes be useful to have two different styles in a document, for
example if you want to do some time-consuming offline treatment in a document
that will nevertheless later be an active web-document.

The macro invocation style is the only one to be immediately effective, being
a runtime affair.  The styles for embedded bits of Perl, being a compiletime
affair only become effective for the next iPerl-documents to be included.

The mnemonic for the variously used C<{...}> is a Perl block, though here it
is simply a stretch of interpolated Perl code, that does B<not> define a
block.  The mnemonic for C<E<lt>...E<gt>> is a Perl input operator, but
inverted here, since the document reads from Perl code.  I<STRING> may be one
of the following:

=over

=cut

sub set_style($;@) {
    my $orig_style = $style;
    $style = shift;

    $macro_start = $macro_end = '';
    $macro_start_dollar1 = 0;
    $joiner = '\\\\;';


=item 'bang'

=item 'unix'

Lines starting with C<#> are deleted.

Lines starting with a C<!> are bits of Perl.  This reminds of interactive unix
programs which thus allow a shell escape.

Perl within lines, potentially spanning several lines, is enclosed in C<!{>
and C<}!>.  This reminds of Perl blocks, but does not delimit a block.  As a
special case C<!}!> without whitespace is equivalent to C<!{}}!>, i.e. one
closing brace.

Perl values to be printed to the document are enclosed in C<!E<lt>> and
C<E<gt>!>.  This reminds of the Perl read operator, inverted here in that the
document reads from a Perl expression.

Macros may be optionally preceded by C<&>, useful to set them of from preceding
alphanumeric characters.

=cut

    if( $style eq 'bang' or $style eq 'unix' ) {
	$splitter = sub {
	    return $`,  $1,  $2 || $3 || $4,  $'
		if $_[0] =~ /
		    ^\# .* \n? |
		    !<\s* ([\s\S]*?) \s*>! |
		    !\{\s* ([\s\S]*?) \s*\}! | !(\})! |
		    ^![^\S\n]* (.*) [^\S\n]*\n?
		/mx;
	    @_;
	};
	$macro_start = '&?';
    }


=item 'control'

Lines starting with a C<^A> are bits of Perl.  This reminds of the beginning
of the alphabet, hence of the line.

Perl within lines, potentially spanning several lines, is enclosed in C<^B>
and C<^E>.  This reminds of beginning and end.

Perl values to be printed to the document are enclosed in C<^P> and
C<^E>.  This reminds of print and end.

=cut

    elsif( $style =~ 'control' ) {
	$splitter = sub {
	    return $`,  $1,  $2 || $3,  $'
		if $_[0] =~ /
		    \s* ([\s\S]*?) \s* |
		    \s* ([\s\S]*?) \s* |
		    ^[^\S\n]* (.*) [^\S\n]*\n?
		/mx;
	    @_;
	};
    }


=item 'cpp'

Lines starting with C<//> are deleted.  Lines starting with C</*> are deleted
upto next C<*/> if it is at end of line.

Lines starting with a C<#> are bits of Perl.  They may be continued over
several lines, as long as each line ends with a C<\>.

=cut

    elsif( $style eq 'cpp' ) {
	$splitter = sub {
	    if( $_[0] =~ /
		    ^\/(?:\/.*|\*(?:(?!\*\/)[\s\S])*?\*\/$) \n? |
		    ^\#[^\S\n]* (.*) \n?
		/mx )
	    {
		my( $pre, $match, $post, $more ) = ($`,  $1,  $');
		while( $match =~ /\\$/ ) {
		    chop $match;
		    ($more, $post) = split "\n", $post, 2;
		    $match .= "\n$more";
		}
		$match =~ s/\s$//; # \ must be very last, \; need not
		return $pre,  undef,  $match,  $post;
	    }
	    @_;
	};
    }


=item generic => I<COMMENT>, I<BEFOREPRINT>, I<AFTERPRINT>,
I<BEFORE>, I<AFTER>

Arguments are 5 regexps, which may not make backreferences via parentheses.
This allows you to define your own simple style.  Anything matching I<COMMENT>
is simply ignored.  I<BEFOREPRINT> and I<AFTERPRINT> markup a printing bit of
Perl.  And I<BEFORE> and I<AFTER> markup a plain bit of Perl.

=cut

    elsif( $style eq 'generic' ) {
	my @style = @_;
	map { $_ ||= '[^\s\S]' } @style;
	$splitter = sub {
	    return $`,  $1,  $2,  $'
		if $_[0] =~ /
		    $style[0] |
		    $style[1]\s* ([\s\S]*?) \s*$style[2] |
		    $style[3]\s* ([\s\S]*?) \s*$style[4]
		/mx;
	    @_;
	};
    }


=item 'm4'

Perl within lines, potentially spanning several lines, is enclosed in the
pseudo-macro C<perl({> and C<})>.  This reminds of Perl blocks, but does not
delimit a block.  As a special case C<perl(})> without whitespace is
equivalent to C<perl({}})>, i.e. one closing brace.

Perl values to be printed to the document are enclosed in the pseudo-macro
C<perl(E<lt>> and C<E<gt>)>.  This reminds of the Perl read operator, inverted
here in that the document reads from a Perl expression.

Everything from the pseudo-macro C<dnl> through end of line is deleted.

The customary m4 macros C<decr>, C<define> (iPerl semantics), C<defn>,
C<errprint>, C<eval>, C<ifdef>, C<ifelse>, C<include> (iPerl semantics),
C<incr>, C<index>, C<len>, C<maketemp>, C<m4exit>, C<sinclude>, C<substr>,
C<syscmd>, C<sysval>, C<translit> (with an additional optional 4th argument
for the modifiers of tr) and C<undefine> are predefined.

The customary m4 macros C<changecom>, C<changequote>, C<divert>, C<divnum>,
C<dumpdef>, C<m4wrap>, C<popdef>, C<pushdef>, C<shift>, C<traceon>,
C<traceoff> and C<undivert> are not implemented.

No macro expansion takes place after a C<#>.  This could be changed with
C<set_style macro =E<gt> ...>, but note that the above mentioned pseudo-macros
are already expanded at compile-time.  Changing this within the document would
lead to two different comment-styles being used.

Remember that macro arguments are Perl code, not just bits of quoted or
unquoted string.

=cut

    elsif( $style eq 'm4' ) {
	$splitter = sub {
	    return $` . $1,  $2,  $3 || $4,  $'
		if $_[0] =~ /
		    ^ ([^#\n]*?) \b
			(?:dnl\b.* \n? |
			perl\( (?: <\s*([\s\S]*?)\s*> |
				\{\s*([\s\S]*?)\s*\} | (\}) ) \))
		/mx;
	    @_;
	};
	$max_macro_growth = 10000;
	$macro_start = '((?:^|\n)[^#\n]*?)';
	$macro_start_dollar1 = 1;
	foreach( qw(define eval include index substr undefine) ) {
	    define;
	}
	define decr => sub { $_[0] - 1 };
	define defn => \&macro;
	define errprint => sub { print STDERR @_ };
	define ifdef => sub { macro( $_[0] ) ? $_[1] : $_[2] };
	define ifelse => sub {
	    while( defined( $_[1] ) && $_[1] ) {
		return $_[2] if $_[0] eq $_[1];
		splice @_, 0, 3;
	    }
	    $_[0];
	};
	define incr => sub { $_[0] + 1 };
	define len => \'length';
	define m4exit => \'exit';
	define maketemp => sub { ($_[0] eq 'XXXXX') ? $$ : $_[0] };
	define sinclude => sub { include $_[0], $_[1], 1 };
	define syscmd => sub { local $| = 1; $sysval = system @_; '' };
	define sysval => sub { $sysval / 256 };
	define translit => sub {
	    local $_ = $_[0];
	    _eval "tr/$_[1]/$_[2]/" . (defined( $_[3] ) ? $_[3] : '');
	    $_;
	};
    }


=item pod => I<ARG>

=item 'pod'

This style can do two things with files containing pod (plain old
documentation).  For one thing, if I<ARG> is true, it can eliminate any pod
from document.  It then does nothing else.  This allows pod to reside in any
file.

For another, if I<ARG> is missing or false, the pod is extracted from the
file, processed with embedded Perl, allowing pods to be dynamic and spread
across several files.  The Perl embedded within the pod has nothing to do with
the programme that contains the pod, even if that is a Perl programme.  This
is because, from a pod-point-of-view, everything that is not pod is ignored.

Paragraphs starting with C<=for perl> or multiple paragraphs surrounded by
C<=begin perl> and C<=end perl> contain plain Perl code that can control the
pod.

Perl within paragraphs, is enclosed in C<PE<lt>{> and C<}E<gt>>.  This reminds
of Perl blocks, but does not delimit a block.  As a special case
C<PE<lt>}E<gt>> without whitespace is equivalent to C<PE<lt>{}}E<gt>>,
i.e. one closing brace.

Perl values to be printed to the document are enclosed in C<PE<lt>> and
C<E<gt>>.  This reminds of the Perl read operator, inverted here in that the
document reads from a Perl expression.

C<ME<lt>> and C<E<gt>> delimit a macro call within a paragraph.

=cut

    elsif( $style eq 'pod' ) {
	my( $last_documents, $in_pod );
	$splitter = $_[0] ? sub {
	    return $`,  undef,  undef,  $'
		if $_[0] =~ /
		    (?:\A\n*|\n\n)= [\s\S]*?
		    (\Z|\n\n=cut\b [\s\S]*? (?=\Z|\n\n))
		/mx;
	    @_;
	} : sub {
	    if( $last_documents != $documents ) {
		$last_documents = $documents;
		$in_pod = 0;
		$_[0] = "\n\n" . $_[0];
	    } elsif( $in_pod ) {
		return @_ unless $_[0] =~ /
		    (?:\n\n)=(cut|for\s+perl)\b\s* ([\s\S]*?) \s*?(?=\Z|\n\n) |
		    (?:\n\n)=begin\s+perl\b\s* ([\s\S]*?)
			\s*?(?:\Z|\n\n=end\s+perl|(?=\n\n=cut\b)) |
		    P<\{\s* ([\s\S]*?) \s*\}> | P<(\})> |
		    P<\s* ([\s\S]*?) \s*>
		/mx;
		if( $1 eq 'cut' ) {
		    $in_pod = 0;
		    return $`,  undef,  undef,  $';
		} else {
		    return $`,  $6,  $2 || $3 || $4 || $5 ,  $';
		}
	    }
	    if( $_[0] =~ /(?=\n\n=)/ ) {
		$in_pod = 1;
		return undef,  undef,  undef,  $';
	    }
	    '';
	};
	$macro_start = 'M<';
	$macro_end = '>';
    }


=item 'sgml'

Lines starting with C<E<lt>!--> are deleted upto next C<--E<gt>> if it is at
end of line.

Bits of Perl are enclosed in C<E<lt>script runat=serverE<gt>> and
C<E<lt>/scriptE<gt>> or C<E<lt>serverE<gt>> and C<E<lt>/serverE<gt>>.
Attributes, such as C<language=Perl> are ignored but recomended to prevent
mistreatment by other parsers.  More general alternate tags are
C<E<lt>perlE<gt>> and C<E<lt>/perlE<gt>>.  As a more convenient (though
probably not SGML compliant) alternative, closer to the other iPerl-styles,
bits of Perl may be enclosed in C<E<lt>{> and C<}E<gt>>.  As a special case
C<E<lt>}E<gt>> without whitespace is equivalent to C<E<lt>{}}E<gt>>, i.e. one
closing brace.  The alternatives are likely not recognized by WISIWYG-HTML
editors, not being proper HTML, and even the server tag might be a Netscape
feature, which other editors cannot handle.  Even the C<script> tag can be
problematic since it may conditionally include one stretch of text or another,
which cannot be done with Javascript, thus confusing an editor which
unconditionally sees both stretches of text.

Perl values to be printed to the document are enclosed in C<&E<lt>> and
C<E<gt>;>.  This reminds of the Perl read operator, inverted here in that the
document reads from a Perl expression.  Alternately, only within C<E<lt>> and
C<E<gt>> (actually C<E<lt>> is not checked for, due to the forward looking
nature of the parser, but should anyway be present before any C<E<gt>>), Perl
values to be printed to the document are enclosed in a pair of C<`>.  When
this is not followed by a C<=> the result is surrounded with double quotes.

Entities (iPerl macros) are enclosed in C<&> and C<;>.  If the enclosed text
is not a defined macro, it is left as an SGML entity.

=cut

    elsif( $style eq 'sgml' ) {
	$splitter = sub {
	    return $`,  defined( $1 ) ? $1 :
		($2 && ($3 ? "$2, '$3'" : "'\"', $2, '\"'")),
		$4 || $5 || $6 || $7 || $8,  $'
		if $_[0] =~ /
		    ^<!--(?:(?!-->)[\s\S])*?-->$ \n? |
		    &<([\s\S]*?)>; |
		    `([^<>]+?)`(\s*=)?(?=[^<>]*>) |
		    <script\s[^>]*runat\s*=\s*[\'\"]?server\b.*?> ([\s\S]*?)
			<\/script> |
		    <server\b.*?> ([\s\S]*?) <\/server> |
		    <perl\b.*?> ([\s\S]*?) <\/perl> |
		    <\{ ([\s\S]*?) \}> | <(\})>
		/mix;
	    @_;
	};
	$macro_start = '&';
	$macro_end = ';';
    }


=item joiner => I<REGEXP>

Sets I<REGEXP> (instead of C<\;>) to match what must be at the beginning or
end of a bit of Perl to suppress the semicolon at that point.  Returns the old
I<REGEXP>.

=cut

    elsif( $style eq 'joiner' ) {
	$style = $orig_style;
	my $old = $joiner;
	$joiner = $_[0] if defined $_[0];
	$old;
    }


=item splitter => I<SUB>

Set a function that, given a string as often as there is still a rest of
document, returns a list of 4 elements: 0) leading plain text, 1) printing
Perl expr, 2) plain Perl and 3) the rest to be treated next time.  Those
elements not matching anything should be C<undef>, epsecially 1) since if it
is the empty string, C<$_> will get printed at that point.  Returns the old
I<SUB>.

=cut

    elsif( $style eq 'splitter' ) {
	$style = $orig_style;
	my $old = $joiner;
	$splitter = $_[0] if defined $_[0];
	$old;
    }


=item macro => I<BEFORE>, I<AFTER>[, I<PAREN>]

I<BEFORE> and I<AFTER> are regexps describing the syntactic sugar which is
eliminated around macro invocations.  If, as in m4-style, I<BEFORE> has to
look backwards, it should contain one paren-pair matching the portion of text
not to discard, and I<PAREN> should then be true.

=cut

    elsif( $style eq 'macro' ) {
	$style = $orig_style;
	($macro_start, $macro_end, $macro_start_dollar1) = @_;
    }

    else {
	die "unknown style '$style'";
    }
    '';
}



=back


=item undefine I<EXPR>

=item undefine

Removes the definedness of I<EXPR> or C<$_>.

=back

=cut

sub undefine(;$) {
    my( $macro ) = @_;
    $macro = $_ unless $macro;
    $defines =
	join '|',
	grep !/^$macro$/,
	split /\|/, $defines;
    delete $defines{$macro};
    '';
}

1;

__END__

=head1 VARIABLES

=over


=item @autostyle_by_contents

Hash-like list of regexps to match against document to determine the mode to
use when C<$style> starts with C<auto>.  Unlike a hash, this list is processed
sequentially until a match is found.


=item @autostyle_by_name

Hash-like list of regexps to match against filenames (actually against
C<$documents[-1]>) to determine the mode to use when C<$style> starts with
C<auto>.  Unlike a hash, this list is processed sequentially until a match is
found.


=item $cache

Make C<include> cache the compiled form of the document for quick reuse when
called again for the same file if C<true>.


=item $debug

Output the generated Perl program rather than executing it if C<true>.


=item $documents

Incremented for each document included.


=item @documents

Contains the list of all nested includes currently active, innermost last.
Where a filename is not known for the document, contains the strings
C<'E<lt>FILEHANDLEE<gt>'>, C<'E<lt>STDINE<gt>'> or C<'E<lt>STRINGE<gt>'>.


=item @include

Second list of directories where C<include> searches for files not found in
the same directory as the file where C<include> was called.  Defaults to
F</usr/include> followed by the contents of C<@INC>.


=item $max_macro_growth

One bit of plain text may grow by no more than this factor through macro
expansions.


=item $max_macro_expansions

In one bit of plain text no more than this many macro expansions may occur.


=item @opt_I

First list of directories where C<include> searches for files not found in the
same directory as the file where C<include> was called.  This is not set by
C<Text::iPerl> but is used if set outside.  The strange name comes from the
fact that C<iperl> like the various invokers of the C preprocessor and some m4
implementations use the C<-I> option for this.


=item $preoutput_handler

Coderef called and reset every time iPerl wants to output a bit of plain text.
Will normally be set by programmes to offer some initialization that can be
overridden by the beginning of a document.


=item $style

This is the name of the style currently in effect.  If this starts with
C<auto>, the style used for an included document is determined in three steps
as follows.  This variable is then set to C<auto: I<style>>.

=over

=item Style specified in the file

This is identical to Emacs' local variables specification inside a file.
There are two possibilities (here shown for style bang): On the first line, or
on the second if the first line is a shebang magic number (C<#!
I<interpreter>>), with possibly other semicolon separated variables for use by
Emacs:

  -*- iPerl-style: "bang" -*-

Or, within the last 3000 characters of the document and not followed by a page
break (C<^L>), C</*> and C<*/> being examples of optional comment delimiters,
which, if present, must however be identical on all lines, with possibly other
specification-lines only used by Emacs:

  /* Local Variables: */
  /* iPerl-style: "bang" */
  /* End: */

The style must be given as a double-quoted literal string.  This can appear
anywhere, i.e. in a bit of Perl as a comment or string or in the host
document.  If neither of these appear the next step is tried.

=item Document-name matched against C<@autostyle_by_name>

If no match is found, the next step is tried.

=item Document-contents matched against C<@autostyle_by_contents>

If no match is found, the style of the including document is maintained.  If
there is none, we die.

=back


=back


=head1 SEE ALSO

L<iperl>, L<web-iPerl>, L<perl>, http://beam.to/iPerl/
