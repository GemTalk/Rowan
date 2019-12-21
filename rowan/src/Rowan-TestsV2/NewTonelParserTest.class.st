Class {
	#name : 'NewTonelParserTest',
	#superclass : 'TestCase',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
NewTonelParserTest >> _CPreprocessor_initializePaths_method_source [

	^ '| list |
  self nativeCppCmd .
	list := { '''' }.
	(path ~~ nil and: [path first == $$]) ifTrue: [list add: self directory].
  cppArchMType ifNil:[  "not using native cpp"
	  list add: ''/usr/include/''; add: ''/usr/local/include/'' .
    systemSearchPaths := { } .
  ] ifNotNil:[
    systemSearchPaths := { ''/usr/include/'' . ''/usr/local/include/'' }.
    cppArchMType == 50 ifTrue:[ systemSearchPaths add: ''/usr/include/linux'' ].
  ].
  searchPaths := list .
'
]

{ #category : 'private' }
NewTonelParserTest >> _issue_45_method_source [
^'"

RePlugin -- A Regular Expressions Plugin for Squeak

Introduction

RePlugin is a Squeak Plugin providing modern regular expression matching operations similar to those found in Perl. It was written by Andrew C. Greenberg (werdna@gate.net), with contributions by Markus Kohler, Stephen Pair and others. RePlugin 3.2 (and ''the Gory Details'' portion of this document) is directly taken from Version 2.04 of the excellent PCRE library by Philip Hazel with only minor modifications.

RePlugin, an Overview

While the primary functionality (and documentation) for RePlugin is found in new classes RePattern and ReMatch and the operations set forth therein, a comprehensive set of convenience functions are provided in the String class for ease of use. 

A Simple Example to Get You Started

After installing RePlugin, you can execute the following in a workspace: 

	''Candy is dandy, but liquor is quicker.''  reMatch: ''a.*y''

This reMatch: message directs RePlugin to search the longer string for the leftmost occurrence of the letter ''a'', followed by the longest string that can be collected thereafter comprising any characters, but ending in a ''y.'' The message answers: 

	 a ReMatch(''andy is dandy'')

(*blush*) which is an object of type ReMatch. As you shall see later, ReMatch objects can be saved to obtain a wide range of information about the match result. When printed, as here, it conveniently identifies the substring that was actually matched, which can also be obtained from the ReMatch instance by sending it the message match. (Note that the longer string ''andy is dandy'' was matched, and not the shorter ''andy''.) If there was no match of the string, for example if the subject string were 

	''You got ''y'', but only after the ''a''''

then the message would answer nil.  A common use of regular expression matching is simply to determine as a boolean result whether the pattern has been matched (similar to the #match method).  Accordingly, a convenience function is provided:

	(''Candy is dandy, but liquor is quicker.'' matchRe: ''a.*y'') ifTrue: [''matched''] ifFalse: [''not matched'']

Global Searching and Replacing

It is sometimes convenient to ask ReMatch to repeatedly search for non-overlapping matches of a regular expression, and to report a collection of information with respect to each of the matches found. For example, the message: 

	''Stupid is as stupid does.'' reMatch: ''stupid'' andCollect: [:m | m match ].

This message looks for occurrences of the regular expression ''stupid'' in the subject string. Each time a match is found, the corresponding match object is passed to the block associated with the collect: keyword, and the results of those computations are returned in an OrderedCollection. Since the first occurrence begins with a capital, only one match is found. (You could collect all occurrences either by using a character class or the i modifier, for example, using the reMatch:opt:collect: message.) In this case, however, the answer will be: 

	OrderedCollection (''stupid'' )

As a somewhat more useful example, 

	''Stupid is as stupid does.'' reMatch: ''\w+'' andCollect: [:m | m match ].

can be used to collect an ordered collection of all non-whitespace phrases in the string, in this case: 

 	OrderedCollection (''Stupid'' ''is'' ''as'' ''stupid'' ''does'' )

This particular form (collecting matches) is used with such frequency that a convenience function is provided:

	''Stupid is as stupid does.'' collectRe: ''\w+''
 
Sometimes you will want to substitute text for the matched text, which you can accomplish with the reMatch:collect: message and some fancy footwork, or which you can do quite easily, for example, as follows: 

	''Stupid is as stupid does.'' reMatch: ''stupid'' opt: ''i'' sub: [:m | ''Andy'' ].

which answers a string replacing all occurrences of stupid (because of the opt: ''i'', the search is done without regard to case) with ''Andy'', yielding: 

	''Andy is as Andy does.''

You can also ''capture'' text by surrounding regular expression subexpressions with parentheses. For example, consider the following expression: 

	''    line has leading spaces'' reMatch: ''^\W+(.*)''

which answers 

	a ReMatch(''     line has leading spaces'')

This would have little utility, since it merely copies the line of text entirely. But since RePlugin keeps track of which text is ''captured'' by which parenthetical group, which is numbered in the order the left parenthesis appears in the string. These group matches can be seperately obtained by sending the resulting match object the message ''matchAt:,'' for example: 

	(''    line has leading spaces'' reMatch: ''^\W+(.*)'') matchAt: 1

which answers 

	''line has leading spaces''

That is, the line without the leading white space. Indeed, RePlugin remembers these parenthetical captures during the match, so that you can check for double words as follows: 

	''this line has has a double word'' reMatch: ''(\w+)\W+\1''

which matches 

	a ReMatch(''has has'')

These and other regular expression operations are discussed in substantially greater detail below. 


Matching With RePlugin

The Principal Messages

You may call RePlugin in any of the following ways: 

	subjectString reMatch: pattern [from: from] [to: to] [opt: optionString]
	subjectString reMatch: [opt: optionString] sub: aBlock [num: maxMatches]
	subjectString reMatch: [opt: optionString] collect: aBlock [num: maxMatches]

The keywords in square brackets are optional, in the sense that messages are available with every combination of keywords shown, with and without the optional keywords. 

The first message performs a single search on the substring of subjectString from from to to, using the modifiers set forth in optionString. If from: is not specified, then 1 is used, if to: is not specified, then subjectString size is used, and if opt: is not specified, then the empty string is used. 
It should be noted that everywhere a pattern is permitted in these operations, either a string or compiled pattern object (an Re) may be used. If a string is used, then RePlugin will first search to see if the object was recently compiled, and if so, use that object, or if not, compiles the expression and remembers it for later reuse. If a compiled pattern object (an Re) is used, then that compiled object will be used, thereby avoiding recompilations and table lookups. 

The second message performs repeated searches of subjectString for nonoverlapping matches of pattern, using compile and matching options optionString until no more matches are present or maxMatches have been found. (If maxMatches is less than zero, the number of matches will be limited only by the number of matches in the string.) Then, for each match found, replace the matched substring with the result of applying the corresponding match object to aBlock. If opt: is not specified, then the empty string is used, and if num: is not specified, then the equivalent of -1 is used. 

There is a special case in the instance where the empty string is matched, because the ''next'' match would begin in the same place, thereby creating an infinite loop. This case is handled as in Perl 5.004, where an empty string is replaced with the result of calling the block, and the next search begins after ''bumping'' the string to the next character. Accordingly, 

	''Thanks Markus and Steve for all your help'' reMatch: ''x*'' sub: [:m | ''!!'' ].  

will answer: 

	''!!T!!h!!a!!n!!k!!s!! !!M!!a!!r!!k!!u!!s!! !!a!!n!!d!! !!S!!t!!e!!v!!e!! !!f!!o!!r!! !!a!!l!!l!! !!y!!o!!u!!r!! !!h!!e!!l!!p!!''

Finally, the third message performs repeated searches of subjectString for nonoverlapping matches of pattern, using compile and matching options optionString until no more matches are present or maxMatches have been found. (If maxMatches is less than zero, the number of matches will be limited only by the number of matches in the string.) Then, for each match found, evalute aBlock with the corresponding matchObject, and maintain and then answer an ordered collection of the results in the order they were computed. If opt: is not specified, then the empty string is used, and if num: is not specified, then the equivalent of -1 is used. 

reMatch:collect: handles empty string in the same manner as reMatch:sub:, with the added proviso that an empty match will not be counted if it immediately follows a non-empty match. Accordingly 

	''123'' reMatch: ''\d*'' collect: [:m | m match]

answers 

	OrderedCollection (''123'' )

and not ''OrderedCollection (''123'' ''''),'' although 

	''123'' reMatch: ''\d*'' sub: [:m | ''<'', m match, ''>'']

will answer 

	 ''<123><>''

These null match rules mirror the semantics of Perl 5''s m/.../g and s/.../g operators. 

Using ReMatch to Obtain Principal Match Information

The substring of the substring matched by re is given by: 

	m match

The beginning and end of the substring in searchString is given by the messages from and to, respectively, so that the substring matched (the result of m match could be obtained with: 

 	m searchString
		copyFrom: (m from)
		to: (m to)


Using ReMatch to Obtain Captured Groups (and Collections of Captured Groups)

The number of substrings capturable by a parenthetical grouping in an re (regardless of the number actually matched to create m) is given by: 

	m numGroups
	
The string captured by parenthetical grouping i, where 1<=i<=(m numGroups) is given by 

	m matchAt: i

and this can be generated as follows: 

	m searchString
		copyFrom: (m fromAt: i)
		to: (m toAt: i)

And an array of size (m numGroups) can be generated from strings and indices accordingly: 

	m matches
	m froms
	m tos


Efficient Regular Expression Matching

RePattern tests for regular expression matching in three stages: 

1. Compiles the regular expression into a convenient internal form. 
2. Searches an object string or substring for matches. 
3. Produces results of queries on a match object. 

If you intend to repeatedly matching a single regular expression against many different strings, for example each line of a file or element of a collection, then repeating Step 1, the compilation, would be wasteful and inefficient. RePattern avoids recompilation by keeping track of the last dozen or so compiled regular expressions, avoiding the costly process of recompilation. Unfortunately, this adds the (less inefficient) cost of a table lookup with each regular expression match. 

Accordingly, RePattern permits you to generate and keep ''compiled pattern objects,'' for repeated matching against subsequent strings without recompiling or searching the compilation cache. You can create an compiled pattern object with the asRePattern message: 

	''\w+'' asRePattern

which answers 

	an Re(''\w+\00'')

and the resulting pattern can be used wherever a pattern string can be used, except that no recompilation or table lookup occurs. The following: 

	re := ''\w+'' asRePattern
	myCollection do: [:i|
		Transcript show: ((i reMatch: re) match); cr]

will be substantially faster than 

	myCollection do: [:i|
		Transcript show: ((i reMatch: ''\w+'') match); cr]

Regular Expression Syntax Summary

A regular expression (or regexp) specifies a set of strings that matches it. Regular expressions can be concatenated to form new regular expressions; if A and B are both regular expressions, then AB is also an regular expression. If a string p matches A and another string q matches B, the string pq will match AB. Thus, complex expressions are easily constructed from simpler primitive expressions. 

Regular expressions can contain both special and ordinary characters. Most ordinary characters, like ''A'', ''a'', or ''0'', are the simplest regular expressions; they simply match themselves. You can concatenate ordinary characters, so last matches the string ''last''. 

Some characters, like ''|'' or ''('', are special. Special characters either stand for classes of ordinary characters, or affect how the regular expressions around them are interpreted. 

The special characters are: 

''.'' 
(Dot.) In the default mode, this matches any character except a newline. If the ''s'' option has been specified, dot matches any character at all, including a newline. 

''^'' 
(Caret.) Matches the start of the string, and if the ''m'' option has been specified, then this also matches immediately after each newline. 

''$'' 
Matches the end of the string, and if the ''m'' option has been specified, then this also matches before a newline. foo matches both ''foo'' and ''foobar'', while the regular expression foo$ matches only ''foo''. 

''*'' 
Causes the resulting regexp to match 0 or more repetitions of the preceding regexp, as many repetitions as are possible. ab* will match ''a'', ''ab'', or ''a'' followed by any number of ''b''s. 

''+'' 
Causes the resulting regexp to match 1 or more repetitions of the preceding regexp. ab+ will match ''a'' followed by any non-zero number of ''b''s; it will not match just ''a''. 

''?'' 
Causes the resulting regexp to match 0 or 1 repetitions of the preceding regexp. ab? will match either ''a'' or ''ab''. 

*?, +?, ?? 
The ''*'', ''+'', and ''?'' qualifiers are all greedy; they match as much text as possible. Sometimes this behaviour isn''t desired; if the regexp <.*> is matched against ''<H1>title</H1>'', it will match the entire string, and not just ''<H1>''. Adding ''?'' after the qualifier makes it perform the match in non-greedy or minimal fashion; as few characters as possible will be matched. Using .*? in the previous expression will match only ''<H1>''. 

{m,n} 
Causes the resulting regexp to match from m to n repetitions of the preceding regexp, attempting to match as many repetitions as possible. For example, a{3,5} will match from 3 to 5 ''a'' characters. Omitting n specifies an infinite upper bound; you can''t omit m. 

{m,n}? 
Causes the resulting regexp to match from m to n repetitions of the preceding regexp, attempting to match as few repetitions as possible. This is the non-greedy version of the previous qualifier. For example, on the 6-character string ''aaaaaa'', a{3,5} will match 5 ''a'' characters, while a{3,5}? will only match 3 characters. 

''\'' 
Either escapes special characters (permitting you to match characters like ''*'', ''?'', and so forth), or signals a special sequence; special sequences are discussed below. 

[] 
Used to indicate a set of characters. Characters can be listed individually, or a range of characters can be indicated by giving two characters and separating them by a ''-''. Special characters are not active inside sets. For example, [akm$] will match any of the characters ''a'', ''k'', ''m'', or ''$''; [a-z] will match any lowercase letter, and [a-zA-Z0-9] matches any letter or digit. Character classes such as \w or \S(defined below) are also acceptable inside a range. If you want to include a '']'' or a ''-'' inside a set, precede it with a backslash, or place it as the first character. The pattern []] will match '']'', for example. 

You can match the characters not within a range by complementing the set. This is indicated by including a ''^'' as the first character of the set; ''^'' elsewhere will simply match the ''^'' character. For example, [^5] will match any character except ''5''. 

''|'' 
A|B, where A and B can be arbitrary regexps, creates a regular expression that will match either A or B. This can be used inside groups (see below) as well. To match a literal ''|'', use \|, or enclose it inside a character class, as in [|]. 

(...) 
Matches whatever regular expression is inside the parentheses, and indicates the start and end of a group; the contents of a group can be retrieved after a match has been performed, and can be matched later in the string with the \number special sequence, described below. To match the literals ''('' or '''')'', use \( or \), or enclose them inside a character class: [(] [)]. 

(?...) 
This is an extension notation (a ''?'' following a ''('' is not meaningful otherwise). The first character after the ''?'' determines what the meaning and further syntax of the construct is. Following are the currently supported extensions: 

(?imsx[-imsx]) 
(One or more letters from the set ''i'', ''m'', ''s'', ''x''.) The group matches the empty string and set (or unset if the letters follow a ''-'') corresponding options for the regular expression or subexpression in which it is contained. 

(?:...) 
A non-grouping version of regular parentheses. Matches whatever regular expression is inside the parentheses, but the substring matched by the group cannot be retrieved after performing a match or referenced later in the pattern. 

(?#...) 
A comment; the contents of the parentheses are simply ignored. 

(?=...) 
Matches if ... matches next, but doesn''t consume any of the string. This is called a lookahead assertion. For example, Isaac (?=Asimov) will match ''Isaac'' only if it''s followed by ''Asimov''. 

(?!!...) 
Matches if ... doesn''t match next. This is a negative lookahead assertion. For example, Isaac (?!!Asimov) will match ''Isaac'' only if it''s not followed by ''Asimov''. 

(?<=...) 
Matches if ... matches, but doesn''t consume any of the string. This is called a lookbehind assertion. For example, (?<=foo|fooey)bar will match ''bar'' only if it''s preceded by ''foo'' or ''fooey''. All lookbehinds must have some fixed length, although alternatives need not be of the same length, as in the example. 

(?<!!...) 
Matches if ... doesn''t match, and doesn''t consume any of the string. This is called a negative lookbehind assertion. For example, (?<=foo|fooey)bar will match ''bar'' only if it''s not preceded by ''foo'' or ''fooey''. 

(?(condition)yes-pattern) 
Matches if condition is false or if condition is true and yes-pattern matches. 

(?(condition)yes-pattern|no-pattern) 
Matches if condition is true and yes-pattern matches, or if condition is false and no-pattern matches. 

The special sequences consist of ''\'' and a character from the list below. If the ordinary character is not on the list, then the resulting regexp will match the second character. For example, \$ matches the character ''$''. 

\number 
Matches the contents of the group of the same number. Groups are numbered starting from 1. For example, (.+) \1 matches ''the the'' or ''55 55'', but not ''the end'' (note the space after the group). This special sequence can only be used to match one of the first 99 groups. If the first digit of number is 0, or number is 3 octal digits long, it will not be interpreted as a group match, but as the character with octal value number. Inside the ''['' and '']'' of a character class, all numeric escapes are treated as characters. 
\A 
Matches only at the start of the string. 
\b 
Matches the empty string, but only at the beginning or end of a word. A word is defined as a sequence of alphanumeric characters, so the end of a word is indicated by whitespace or a non-alphanumeric character. 
\B 
Matches the empty string, but only when it is not at the beginning or end of a word. 
\d 
Matches any decimal digit; this is equivalent to the set [0-9]. 
\D 
Matches any non-digit character; this is equivalent to the set [^0-9]. 
\s 
Matches any whitespace character; this is equivalent to the set [ \t\n\r\f\v]. 
\S 
Matches any non-whitespace character; this is equivalent to the set [^ \t\n\r\f\v]. 
\w 
Matches any alphanumeric character; this is equivalent to the set [a-zA-Z0-9_]. 
\W 
Matches any non-alphanumeric character; this is equivalent to the set [^a-zA-Z0-9_]. 

\Z 
Matches only at the end of the string. 

\\ 
Matches a literal backslash. 


Compiler and Matching Option Modes Summary

  i  for Caseless Matching Mode
  m  for Multiline Mode
  s  for Dotall Mode (Dot matches newlines)
  x  for Extended Mode (whitespace not meaningful, comments permitted)
  A  for Anchored mode
  B  for NOTBOL mode (see below)
  E  for ''Dollar end only'' mode (see below)
  U  for Ungreedy mode -- greediness of operators is reversed
  X  for PCRE ''Extra'' mode (see below)
  Z  for NOTEOL mode (see below)

Options B and Z are available only when matching. Option A is available for both matching and compiling. The remaining options are available only for compiling patterns. "
	^ self'
]

{ #category : 'asserting' }
NewTonelParserTest >> assertParse: aString rule: rule equals: result [ 
	self 
		assert: (self parse: aString rule: rule)
		equals: result
]

{ #category : 'private' }
NewTonelParserTest >> newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  ^ TonelCypressReader
    newClassDefinitionForClassNamed: nameString
    superclassName: superclassString
    category: categoryString
    instVarNames: ivarArray
    classVarNames: cvarArray
    poolDictionaryNames: poolArray
    classInstVarNames: civarArray
    type: typeSymbol
    comment: commentString
]

{ #category : 'private' }
NewTonelParserTest >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [
  ^ TonelCypressReader
    newMethodDefinitionForClassNamed: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source
]

{ #category : 'private' }
NewTonelParserTest >> newTypeDefinitionFrom: anArray [
  ^ TonelCypressReader newTypeDefinitionFrom: anArray
]

{ #category : 'private' }
NewTonelParserTest >> parse: aString rule: rule [
  | parser |
  parser := self tonelParserClass onString: aString forReader: self.
  ^ parser perform: rule
]

{ #category : 'asserting' }
NewTonelParserTest >> shouldParse: aString rule: rule raise: error [
  | parser |
  parser := self tonelParserClass onString: aString  forReader: self.
  self should: [ parser perform: rule ] raise: error
]

{ #category : 'tests' }
NewTonelParserTest >> testComment [
	self 
		shouldParse: 'this "should" be an error'
		rule: #comment
		raise: TonelParseError.
	
	self  
		assertParse: '"this is a comment"'
		rule: #comment
		equals: 'this is a comment'.
	
	self  
		assertParse: '"""this"" is a comment with ""nested"" colons ""also at the end"""'
		rule: #comment
		equals: '"this" is a comment with "nested" colons "also at the end"'
]

{ #category : 'tests' }
NewTonelParserTest >> testCPreprocessor_initializePaths [
	self 
		assertParse: '
{ #category: ''Private'' }
CPreprocessor>>_initializePaths [
', self _CPreprocessor_initializePaths_method_source, '
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #CPreprocessor
			classIsMeta: false
			selector: #_initializePaths
			category: 'Private'
			source: '_initializePaths
', self _CPreprocessor_initializePaths_method_source).
]

{ #category : 'tests' }
NewTonelParserTest >> testExtractSelector [
	| parser |
	
	parser := self tonelParserClass new.
	
	self assert: (parser extractSelector: 'unary') equals: #unary.
	self assert: (parser extractSelector: '+ something') equals: #+.
	self assert: (parser extractSelector: '==> other') equals: #==>.
	self 
		assert: (parser extractSelector: 'some: arg1 keyword: arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: 	arg1 keyword:arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: arg1 
keyword: arg2 
selector: arg3') 
		equals: #some:keyword:selector:.
]

{ #category : 'tests' }
NewTonelParserTest >> testIssue465 [
	self 
		assertParse: '
{ #category: ''documentation'' }
Object>>anReOverviewComment [
', self _issue_45_method_source, '
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: false
			selector: #anReOverviewComment
			category: 'documentation'
			source: 'anReOverviewComment
', self _issue_45_method_source).
]

{ #category : 'tests' }
NewTonelParserTest >> testMetadata [
	self 
		assertParse: '{ #vars: [ #a, #b ] }' 
		rule: #metadata 
		equals: { #vars -> #(a b) } asDictionary.
	
	self 
		assertParse: '{ 
	#vars: [ #a, #b ],
	#uses: { #someNested: 42 } 
	}' 
		rule: #metadata 
		equals: { 
			#vars -> #(a b). 
			#uses -> { #someNested -> 42 } asDictionary
		} asDictionary
]

{ #category : 'tests' }
NewTonelParserTest >> testMethod [

	self 
		assertParse: 'Object>>name' 
		rule: #method
		equals: #(('Object' nil) 'name').
		
	self 
		assertParse: 'Object >> name: aString' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').

false ifTrue: [	"I'm not sure that this pattern will show up in the wild ...so it can fail"
	self 
		assertParse: 'Object >> name: aString [ I do not care ]' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
].
	self 
		assertParse: 'Object class >> name: aString' 
		rule: #method
		equals: #(('Object' 'class') 'name: aString').
]

{ #category : 'tests' }
NewTonelParserTest >> testMethodBody [

true ifTrue: [ "skip this test for now" ^ self ].
	self 
		assertParse: '[ method body... I will ignore what is inside ]'
		rule: #methodBody
		equals: ' method body... I will ignore what is inside'.

	self 
		assertParse: '[
method 
	[body... [I 
		will ignore] 
what] is inside
]'
		rule: #methodBody
		equals: '
method 
	[body... [I 
		will ignore] 
what] is inside'.
		
	self 
		assertParse: '[ method body with "''", ''"'', "[", '']'' ]'
		rule: #methodBody
		equals: ' method body with "''", ''"'', "[", '']'''.
]

{ #category : 'tests' }
NewTonelParserTest >> testMethodDef [
	self 
		assertParse: '
{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: false
			selector: #name
			category: 'accessing' 
			source: 'name
	^ self printString').
			
	self 
		assertParse: '
Object class>>name [
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: true
			selector: #name
			category: ''
			source: 'name
	^ self printString').

	self 
		assertParse: '
TClass classSide >> template: aSystemCategoryName [
	"I really do not care"
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #TClass
			classIsMeta: true
			selector: #template:
			category: ''
			source: 'template: aSystemCategoryName
	"I really do not care"').
]

{ #category : 'tests' }
NewTonelParserTest >> testMethodDefList [
	| parsed |
	
	parsed := self 
		parse: '
Object class>>new [
	^ self basicNew initialize
]

{ #category: ''accessing'' }
Object>>name [
	^ self printString
]

{ #category: ''printing'' }
Object>>printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: [''an ''] ifFalse: [''a '']);
		nextPutAll: title

]'
		rule: #methodDefList.
		
	self assert: parsed flattened size equals: 3
]

{ #category : 'tests' }
NewTonelParserTest >> testRemoveFromEnclosingStartEnd [
  | parser nl |
  nl := TonelWriter lineEnding.
  parser := self tonelParserClass new.
  self
    assert: (parser removeFrom: '[ ^ self ]' enclosingStart: $[ end: $])
    equals: '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self ]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self
	]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 

	^ self

]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals:
      ('
	^ self
' withLineEndings: nl)
]

{ #category : 'tests' }
NewTonelParserTest >> testType [
	self assertParse: 'Class' rule: #type equals: 'Class'.
	self assertParse: 'Trait' rule: #type equals: 'Trait'.
	self assertParse: 'Extension' rule: #type equals: 'Extension'
]

{ #category : 'tests' }
NewTonelParserTest >> testTypeDef [
  self
    assertParse:
      '
"
this is a test
"
Class { 
	#name: ''X'',
	#superclass: ''Y'',
	#category: ''Z'' 
}'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'X'
        superclassName: 'Y'
        category: 'Z'
        instVarNames: #()
        classVarNames: #()
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')
]

{ #category : 'tests' }
NewTonelParserTest >> testTypeDefWithClassVars [
  self
    assertParse:
      '
"
this is a test
"
Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : #MonticelloMocks
}
'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'MCMockASubclass'
        superclassName: 'MCMockClassA'
        category: 'MonticelloMocks'
        instVarNames: #(#'x')
        classVarNames: #(#'Y')
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')
]

{ #category : 'private' }
NewTonelParserTest >> tonelParserClass [

	^true
		ifTrue: [ NewTonelParser ]
		ifFalse: [ TonelParser ]
]
