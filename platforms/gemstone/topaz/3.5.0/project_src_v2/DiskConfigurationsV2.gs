! Class Declarations
! Generated file, do not Edit

doit
(ByteArray
	subclass: 'UUID'
	instVarNames: #(  )
	classVars: #( GeneratorClass )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID';
		comment: '';
		immediateInvariant.
true.
%

doit
(Dictionary
	subclass: 'STONTestMap'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONTestMap is used to support unit tests.

I am a Dictionary but I don''t print my elements during #printOn: to allow safe inspection of structures containing cycles that would otherwise lead to infinite loops.';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'FileException'
	instVarNames: #( fileName )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'Common superclass for exceptions while using file streams';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'CannotDeleteFileException'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'Notfify when not able to delete';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'FileAlreadyExistsException'
	instVarNames: #( file )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'I am an exception that notifies that a file already exists.';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'FileDoesNotExistException'
	instVarNames: #( readOnly )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'I am raised when an operation is attempted on a file that does not exist.

The method used to signal the exception depends on the form the file name exists in:

- For strings: (FileDoesNotExistException file fileName: aString) signal 
- For Files: FileDoesNotExistException signalOnFile: aFile
- For FileReferences: FileDoesNotExistException signalWith: aFileReference


Applications that want to offer the user the opportunity to select a different file can use:

	UIManager default fileDoesNotExistsDefaultAction: exception
';
		immediateInvariant.
true.
%

doit
(FileException
	subclass: 'FileWriteError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'Notify about an error when trying to attempt to write to a file';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'FileSystemError'
	instVarNames: #( reference )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for errors that may occur during filesystem operations.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'DirectoryDoesNotExist'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised when I an operation is attempted inside a directory that does not exist. ';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'DirectoryExists'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to create a directory that already exists.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'DirectoryIsNotEmpty'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to delete a directory when is not empty.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'FileExists'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to create a file or directory over top of an existing file.';
		immediateInvariant.
true.
%

doit
(FileSystemError
	subclass: 'IllegalName'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am raised on an attempt to use an illegal file name
';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'STONReaderError'
	instVarNames: #( streamPosition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONReaderError is the error/exception signalled by STONReader when illegal/incorrect input is seen. 
';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'STONWriterError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONWriterError is the error/exception signalled by STONWriter when illegal/incorrect input is seen. ';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'ZnCharacterEncodingError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterEncodingError.
I am an Error.

I signal when something goes wrong while encoding or decoding characters.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(Magnitude
	indexableSubclass: 'RwGemStoneVersionNumber'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-Configurations-Common';
		comment: '# GemStone version format

GemStone versions are simply an unbounded collection of $. separated positive integers.';
		immediateInvariant.
true.
%

doit
(Magnitude
	subclass: 'RwSemanticVersionNumber'
	instVarNames: #( normalVersion preReleaseVersion buildVersion )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-Configurations-Common';
		comment: 'RwSemanticVersionNumber conforms to version 2.0.0 of  [Semantic Versioning 2.0.0](http://semver.org/)

**Semantic Versioning Specification** extracted from [Semantic versioning 2.0.0](https://raw.githubusercontent.com/semver/semver/347f73f880ebe1de61891832bf8702e864ca0998/semver.md):

Semantic Versioning 2.0.0
==============================

Summary
-------

Given a version number MAJOR.MINOR.PATCH, increment the:

1. MAJOR version when you make incompatible API changes,
1. MINOR version when you add functionality in a backwards-compatible
   manner, and
1. PATCH version when you make backwards-compatible bug fixes.

Additional labels for pre-release and build metadata are available as extensions
to the MAJOR.MINOR.PATCH format.

Introduction
------------

In the world of software management there exists a dreaded place called
"dependency hell." The bigger your system grows and the more packages you
integrate into your software, the more likely you are to find yourself, one
day, in this pit of despair.

In systems with many dependencies, releasing new package versions can quickly
become a nightmare. If the dependency specifications are too tight, you are in
danger of version lock (the inability to upgrade a package without having to
release new versions of every dependent package). If dependencies are
specified too loosely, you will inevitably be bitten by version promiscuity
(assuming compatibility with more future versions than is reasonable).
Dependency hell is where you are when version lock and/or version promiscuity
prevent you from easily and safely moving your project forward.

As a solution to this problem, I propose a simple set of rules and
requirements that dictate how version numbers are assigned and incremented.
These rules are based on but not necessarily limited to pre-existing
widespread common practices in use in both closed and open-source software.
For this system to work, you first need to declare a public API. This may
consist of documentation or be enforced by the code itself. Regardless, it is
important that this API be clear and precise. Once you identify your public
API, you communicate changes to it with specific increments to your version
number. Consider a version format of X.Y.Z (Major.Minor.Patch). Bug fixes not
affecting the API increment the patch version, backwards compatible API
additions/changes increment the minor version, and backwards incompatible API
changes increment the major version.

I call this system "Semantic Versioning." Under this scheme, version numbers
and the way they change convey meaning about the underlying code and what has
been modified from one version to the next.


Semantic Versioning Specification (SemVer)
------------------------------------------

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be
interpreted as described in [RFC 2119](http://tools.ietf.org/html/rfc2119).

1. Software using Semantic Versioning MUST declare a public API. This API
could be declared in the code itself or exist strictly in documentation.
However it is done, it SHOULD be precise and comprehensive.

1. A normal version number MUST take the form X.Y.Z where X, Y, and Z are
non-negative integers, and MUST NOT contain leading zeroes. X is the
major version, Y is the minor version, and Z is the patch version.
Each element MUST increase numerically. For instance: 1.9.0 -> 1.10.0 -> 1.11.0.

1. Once a versioned package has been released, the contents of that version
MUST NOT be modified. Any modifications MUST be released as a new version.

1. Major version zero (0.y.z) is for initial development. Anything MAY change
at any time. The public API SHOULD NOT be considered stable.

1. Version 1.0.0 defines the public API. The way in which the version number
is incremented after this release is dependent on this public API and how it
changes.

1. Patch version Z (x.y.Z | x > 0) MUST be incremented if only backwards
compatible bug fixes are introduced. A bug fix is defined as an internal
change that fixes incorrect behavior.

1. Minor version Y (x.Y.z | x > 0) MUST be incremented if new, backwards
compatible functionality is introduced to the public API. It MUST be
incremented if any public API functionality is marked as deprecated. It MAY be
incremented if substantial new functionality or improvements are introduced
within the private code. It MAY include patch level changes. Patch version
MUST be reset to 0 when minor version is incremented.

1. Major version X (X.y.z | X > 0) MUST be incremented if any backwards
incompatible changes are introduced to the public API. It MAY also include minor
and patch level changes. Patch and minor version MUST be reset to 0 when major
version is incremented.

1. A pre-release version MAY be denoted by appending a hyphen and a
series of dot separated identifiers immediately following the patch
version. Identifiers MUST comprise only ASCII alphanumerics and hyphen
[0-9A-Za-z-]. Identifiers MUST NOT be empty. Numeric identifiers MUST
NOT include leading zeroes. Pre-release versions have a lower
precedence than the associated normal version. A pre-release version
indicates that the version is unstable and might not satisfy the
intended compatibility requirements as denoted by its associated
normal version. Examples: 1.0.0-alpha, 1.0.0-alpha.1, 1.0.0-0.3.7,
1.0.0-x.7.z.92.

1. Build metadata MAY be denoted by appending a plus sign and a series of dot
separated identifiers immediately following the patch or pre-release version.
Identifiers MUST comprise only ASCII alphanumerics and hyphen [0-9A-Za-z-].
Identifiers MUST NOT be empty. Build metadata MUST be ignored when determining
version precedence. Thus two versions that differ only in the build metadata,
have the same precedence. Examples: 1.0.0-alpha+001, 1.0.0+20130313144700,
1.0.0-beta+exp.sha.5114f85.

1. Precedence refers to how versions are compared to each other when ordered.
Precedence MUST be calculated by separating the version into major, minor, patch
and pre-release identifiers in that order (Build metadata does not figure
into precedence). Precedence is determined by the first difference when
comparing each of these identifiers from left to right as follows: Major, minor,
and patch versions are always compared numerically. Example: 1.0.0 < 2.0.0 <
2.1.0 < 2.1.1. When major, minor, and patch are equal, a pre-release version has
lower precedence than a normal version. Example: 1.0.0-alpha < 1.0.0. Precedence
for two pre-release versions with the same major, minor, and patch version MUST
be determined by comparing each dot separated identifier from left to right
until a difference is found as follows: identifiers consisting of only digits
are compared numerically and identifiers with letters or hyphens are compared
lexically in ASCII sort order. Numeric identifiers always have lower precedence
than non-numeric identifiers. A larger set of pre-release fields has a higher
precedence than a smaller set, if all of the preceding identifiers are equal.
Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta <
1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.

BackusNaur Form Grammar for Valid SemVer Versions
--------------------------------------------------

    <valid semver> ::= <version core>
                     | <version core> "-" <pre-release>
                     | <version core> "+" <build>
                     | <version core> "-" <pre-release> "+" <build>

    <version core> ::= <major> "." <minor> "." <patch>

    <major> ::= <numeric identifier>

    <minor> ::= <numeric identifier>

    <patch> ::= <numeric identifier>

    <pre-release> ::= <dot-separated pre-release identifiers>

    <dot-separated pre-release identifiers> ::= <pre-release identifier>
                                              | <pre-release identifier> "." <dot-separated pre-release identifiers>

    <build> ::= <dot-separated build identifiers>

    <dot-separated build identifiers> ::= <build identifier>
                                        | <build identifier> "." <dot-separated build identifiers>

    <pre-release identifier> ::= <alphanumeric identifier>
                               | <numeric identifier>

    <build identifier> ::= <alphanumeric identifier>
                         | <digits>

    <alphanumeric identifier> ::= <non-digit>
                                | <non-digit> <identifier characters>
                                | <identifier characters> <non-digit>
                                | <identifier characters> <non-digit> <identifier characters>

    <numeric identifier> ::= "0"
                           | <positive digit>
                           | <positive digit> <digits>

    <identifier characters> ::= <identifier character>
                              | <identifier character> <identifier characters>

    <identifier character> ::= <digit>
                             | <non-digit>

    <non-digit> ::= <letter>
                  | "-"

    <digits> ::= <digit>
               | <digit> <digits>

    <digit> ::= "0"
              | <positive digit>

    <positive digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

    <letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
               | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T"
               | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d"
               | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n"
               | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x"
               | "y" | "z"


Why Use Semantic Versioning?
----------------------------

This is not a new or revolutionary idea. In fact, you probably do something
close to this already. The problem is that "close" isn''t good enough. Without
compliance to some sort of formal specification, version numbers are
essentially useless for dependency management. By giving a name and clear
definition to the above ideas, it becomes easy to communicate your intentions
to the users of your software. Once these intentions are clear, flexible (but
not too flexible) dependency specifications can finally be made.

A simple example will demonstrate how Semantic Versioning can make dependency
hell a thing of the past. Consider a library called "Firetruck." It requires a
Semantically Versioned package named "Ladder." At the time that Firetruck is
created, Ladder is at version 3.1.0. Since Firetruck uses some functionality
that was first introduced in 3.1.0, you can safely specify the Ladder
dependency as greater than or equal to 3.1.0 but less than 4.0.0. Now, when
Ladder version 3.1.1 and 3.2.0 become available, you can release them to your
package management system and know that they will be compatible with existing
dependent software.

As a responsible developer you will, of course, want to verify that any
package upgrades function as advertised. The real world is a messy place;
there''s nothing we can do about that but be vigilant. What you can do is let
Semantic Versioning provide you with a sane way to release and upgrade
packages without having to roll new versions of dependent packages, saving you
time and hassle.

If all of this sounds desirable, all you need to do to start using Semantic
Versioning is to declare that you are doing so and then follow the rules. Link
to this website from your README so others know the rules and can benefit from
them.


FAQ
---

### How should I deal with revisions in the 0.y.z initial development phase?

The simplest thing to do is start your initial development release at 0.1.0
and then increment the minor version for each subsequent release.

### How do I know when to release 1.0.0?

If your software is being used in production, it should probably already be
1.0.0. If you have a stable API on which users have come to depend, you should
be 1.0.0. If you''re worrying a lot about backwards compatibility, you should
probably already be 1.0.0.

### Doesn''t this discourage rapid development and fast iteration?

Major version zero is all about rapid development. If you''re changing the API
every day you should either still be in version 0.y.z or on a separate
development branch working on the next major version.

### If even the tiniest backwards incompatible changes to the public API require a major version bump, won''t I end up at version 42.0.0 very rapidly?

This is a question of responsible development and foresight. Incompatible
changes should not be introduced lightly to software that has a lot of
dependent code. The cost that must be incurred to upgrade can be significant.
Having to bump major versions to release incompatible changes means you''ll
think through the impact of your changes, and evaluate the cost/benefit ratio
involved.

### Documenting the entire public API is too much work!

It is your responsibility as a professional developer to properly document
software that is intended for use by others. Managing software complexity is a
hugely important part of keeping a project efficient, and that''s hard to do if
nobody knows how to use your software, or what methods are safe to call. In
the long run, Semantic Versioning, and the insistence on a well defined public
API can keep everyone and everything running smoothly.

### What do I do if I accidentally release a backwards incompatible change as a minor version?

As soon as you realize that you''ve broken the Semantic Versioning spec, fix
the problem and release a new minor version that corrects the problem and
restores backwards compatibility. Even under this circumstance, it is
unacceptable to modify versioned releases. If it''s appropriate,
document the offending version and inform your users of the problem so that
they are aware of the offending version.

### What should I do if I update my own dependencies without changing the public API?

That would be considered compatible since it does not affect the public API.
Software that explicitly depends on the same dependencies as your package
should have their own dependency specifications and the author will notice any
conflicts. Determining whether the change is a patch level or minor level
modification depends on whether you updated your dependencies in order to fix
a bug or introduce new functionality. I would usually expect additional code
for the latter instance, in which case it''s obviously a minor level increment.

### What if I inadvertently alter the public API in a way that is not compliant with the version number change (i.e. the code incorrectly introduces a major breaking change in a patch release)?

Use your best judgment. If you have a huge audience that will be drastically
impacted by changing the behavior back to what the public API intended, then
it may be best to perform a major version release, even though the fix could
strictly be considered a patch release. Remember, Semantic Versioning is all
about conveying meaning by how the version number changes. If these changes
are important to your users, use the version number to inform them.

### How should I handle deprecating functionality?

Deprecating existing functionality is a normal part of software development and
is often required to make forward progress. When you deprecate part of your
public API, you should do two things: (1) update your documentation to let
users know about the change, (2) issue a new minor release with the deprecation
in place. Before you completely remove the functionality in a new major release
there should be at least one minor release that contains the deprecation so
that users can smoothly transition to the new API.

### Does SemVer have a size limit on the version string?

No, but use good judgment. A 255 character version string is probably overkill,
for example. Also, specific systems may impose their own limits on the size of
the string.

### Is "v1.2.3" a semantic version?

No, "v1.2.3" is not a semantic version. However, prefixing a semantic version
with a "v" is a common way (in English) to indicate it is a version number.
Abbreviating "version" as "v" is often seen with version control. Example:
`git tag v1.2.3 -m "Release version 1.2.3"`, in which case "v1.2.3" is a tag
name and the semantic version is "1.2.3".


About
-----

The Semantic Versioning specification is authored by [Tom
Preston-Werner](http://tom.preston-werner.com), inventor of Gravatar and
cofounder of GitHub.

If you''d like to leave feedback, please [open an issue on
GitHub](https://github.com/mojombo/semver/issues).


License
-------

Creative Commons - CC BY 3.0
http://creativecommons.org/licenses/by/3.0/';
		immediateInvariant.
true.
%

doit
(Notification
	subclass: 'ResolutionRequest'
	instVarNames: #( origin )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I represent a request for user assistance in resolving an origin. I am a resumable exception that gets raised when there is no way of automatically resolving a particular origin. ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'AbstractFileReference'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for FileLocator and FileReference. By implementing most of the methods on myself most code duplucation between the locator and the reference can be avoided';
		immediateInvariant.
true.
%

doit
(AbstractFileReference
	subclass: 'FileLocator'
	instVarNames: #( origin path )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a late-bound reference. I refer to a file or directory in relation to a well-known location on the filesystem, called an origin. When asked to perform concrete operation, I look up the current location of my origin, and resolve my path against it. 

Usage
----------
FileLocator vmDirectory parent pathString
	> ''/Applications''


FileLocator desktop.
FileLocator desktop basename.

FileLocator home basename.
FileLocator image.
FileLocator vmBinary asAbsolute pathString 
	>  ''/Applications/CogVM.app/Contents/MacOS/CogVM''
FileLocator vmBinary pathString 
	> ''/Applications/CogVM.app/Contents/MacOS/CogVM''
		




Implementation
------------------------
origin 
	A symbolic name for base reference I use to resolve myself.

path
	A relative path that is resolved against my origin"
	
	';
		immediateInvariant.
true.
%

doit
(AbstractFileReference
	subclass: 'FileReference'
	instVarNames: #( filesystem path )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I combine a filesystem and path, which is sufficient to refer to a concrete file or directory. I provide methods for navigating my filesystem, performing filesystem operations and opening and closing files.  I am the primary mechanism for working with files and directories. 

| working |
working := FileSystem disk workingDirectory.
working files 

| disk |
disk := FileSystem disk.
disk root.                               	"a reference to the root directory"
disk working.                         	"a reference to the working directory"';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FastUUIDGenerator'
	instVarNames: #( bits1 bits2 bits3 bits4 random semaphoreForGenerator )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'File'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'I represent a sequential binary File. I provide the minimum operations to:

- move the cursor fo the file
- reading
- writing

I am also the entry point of the FilePlugin primitives.

!Examples of usage

"Creating a file"
file := File named: ''asd.txt'' asFileReference fullName.

"Opening / closing it"
file open.
file openForAppend.
file close.

"Accessing the file properties"
file size.
file position.
file position: 0.
file seekAbsolute: 10.
file seekRelative: 10.
file atEnd.

"Writing"
file nextPutAll: ''sdd''.

"Reading"
file next: 2.

"Buffered write"
file next: 2 putAll: ''abc'' startingAt: 2.

"Buffered read"
buffer := ByteArray new: 5.
file readInto: buffer startingAt: 1 count: 5.
buffer asString.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystem'
	instVarNames: #( store )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I present a low-level protocol for interacting with filesystems. I hold a reference to
a store (a subinstance of FileSystemStore) which takes care of the details of performing 
file and directory operations on the filesystem I represent. 

I am responsible for resolving all paths that
I pass into my store. My store acts as a factory and offers platform specific actions.


FileSystem instances know two methods that return a Reference object: workingDirectory and root.

FileSystem disk workingDirectory
FileSystem disk root

';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemDirectoryEntry'
	instVarNames: #( reference creation modification isDirectory isSymlink size posixPermissions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a cache for metadata about a file or directory. The information I hold is as follows:

reference
	A reference to the file or directory to which my data pertains.
	
creation
	The creation date and time, stored as number seconds since the 
	Smalltalk epoch.
	
modification
	The modification date and time, number seconds since the Smalltalk epoch.
	
isDirectory 
	True if my data pertains to a directory, false if a file.
	
size
	Size in bytes for a file, 0 for a directory.
';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemGuide'
	instVarNames: #( visitor work selectChildren )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for objects that fulfill the Guide role in the Guide/Visitor pattern. My subclasses know how to traverse a filesystem in a specific order, "showing" the files and directories they encounter to a visitor.

visitor
	An object that fulfills the Visitor role and implements the visitor protocol.
	
work
	An OrderedCollection, used to keep track of filesystem nodes that have not yet been visited';
		immediateInvariant.
true.
%

doit
(FileSystemGuide
	subclass: 'BreadthFirstGuide'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I traverse the filesystem in breadth-first order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order:  alpha, delta, beta, gamma, epsilon.

I use my work instance variable as a queue, adding nodes to be visited to the end and retrieving them from the beginning.
';
		immediateInvariant.
true.
%

doit
(FileSystemGuide
	subclass: 'PostorderGuide'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I traverse the filesystem in depth-first post order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order:  beta, gamma, alpha, epsilon, delta.

I use my work instance variable as a stack. I push messages that cause nodes to be traversed or visited, and execute them in reverse order.';
		immediateInvariant.
true.
%

doit
(FileSystemGuide
	subclass: 'PreorderGuide'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I traverse the filesystem in depth-first pre order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order: alpha, beta, gamma, delta, epsilon.

I use my work instance variable as a stack. I push nodes to be visited and visit them in reverse order.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemHandle'
	instVarNames: #( reference writable )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for file handle implementations. I provide a uniform interface that streams can use for read and write operations on a file regardless of the filesystem. I encapsulate the actual IO primitives.';
		immediateInvariant.
true.
%

doit
(FileSystemHandle
	subclass: 'FileHandle'
	instVarNames: #( id )
	classVars: #( Registry )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I provide an interface for doing IO on an open file. I keep an id, which as an opaque identifier used by the FilePlugin primitives. I translate positions from the 1-based indexes used in Smalltalk to the 0-based offsets used by the primitives.

I implement the primitives on my class side.';
		immediateInvariant.
true.
%

doit
(FileSystemHandle
	subclass: 'MemoryHandle'
	instVarNames: #( entry )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I provide "primitives" for doing IO on files in a MemoryFileSystem. I delegate most of my actions to the MemoryFile. This way there is only one place needed where the data is stored.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemPermission'
	instVarNames: #( posixPermission )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I''m a set of permissions for a Directory Entry';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemResolver'
	instVarNames: #( next )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for objects that can resolve origins into references. Such objects use the Chain of Responsibility pattern, and when unable to resolve a particular origin, delegate that resolution request to the next resolver in the list.

next
	The next resolver in the list, or nil
';
		immediateInvariant.
true.
%

doit
(FileSystemResolver
	subclass: 'InteractiveResolver'
	instVarNames: #( cache )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I resolve origins by consulting the user. I maintain a cache of the user''s responses.';
		immediateInvariant.
true.
%

doit
(FileSystemResolver
	subclass: 'PlatformResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for platform-specific resolvers.';
		immediateInvariant.
true.
%

doit
(PlatformResolver
	subclass: 'MacOSResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an expert on Mac OS X filesystem conventions. I resolve origins according to these conventions.';
		immediateInvariant.
true.
%

doit
(PlatformResolver
	subclass: 'UnixResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an expert on Unix filesystem conventions. I resolve origins according to these conventions.';
		immediateInvariant.
true.
%

doit
(PlatformResolver
	subclass: 'WindowsResolver'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an expert on Windows filesystem conventions. I resolve origins according to these conventions.';
		immediateInvariant.
true.
%

doit
(FileSystemResolver
	subclass: 'SystemResolver'
	instVarNames: #(  )
	classVars: #( UserLocalDirectory )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I resolve origins that are related to the currently running Smalltalk system, using primitives provided by the VM. ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemStore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for store implementations. My subclasses provide access to the actual data storage of a particular kind of filesystem. 

The file system can be accessed via
	FileSystem disk 
	FileSystem memory
	
My associated filesystem can be accessed as follows:
      DiskStore currentFileSystem';
		immediateInvariant.
true.
%

doit
(FileSystemStore
	subclass: 'DiskStore'
	instVarNames: #( maxFileNameLength )
	classVars: #( CurrentFS DefaultWorkingDirectory )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I am an abstract superclass for disk store implementations. My subclasses provide access to the actual data storage of a particular kind of filesystem. 
';
		immediateInvariant.
true.
%

doit
(DiskStore
	subclass: 'UnixStore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I''m a specific store for Unix file systems';
		immediateInvariant.
true.
%

doit
(UnixStore
	subclass: 'MacStore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I''m a specific store for OSX file systems';
		immediateInvariant.
true.
%

doit
(DiskStore
	subclass: 'WindowsStore'
	instVarNames: #( disk )
	classVars: #( Disks )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Disk';
		comment: 'I''m a specific store for Windows file systems';
		immediateInvariant.
true.
%

doit
(FileSystemStore
	subclass: 'MemoryStore'
	instVarNames: #( root )
	classVars: #( CurrentFS )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I''m a specific store for memory file system';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'FileSystemVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am an abstract superclass for objects that can perform operations on directory trees. My subclasses implement the visitor protocol, and process filesystem nodes shown to them by guides.';
		immediateInvariant.
true.
%

doit
(FileSystemVisitor
	subclass: 'AbstractEnumerationVisitor'
	instVarNames: #( out block )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I''m an abstract superclass for enumeration operations on directory entries. ';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitor
	subclass: 'CollectVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a visitor that collects objects from the nodes I visit. I take a block similar to those passed to Collection>>collect:. I evaluate the block with DirectoryEntries for the nodes I visit, and collect the objects answered into an array.

I can use any guide, and the objects in the array I produce will reflect the order imposed by the guide.';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitor
	subclass: 'SelectVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I am a visitor that selects objects from the nodes I visit. I take a block similar to those passed to Collection>>select:. I evaluate the block with DirectoryEntries for the nodes I visit.

I can use any guide, and the objects in the array I produce will reflect the order imposed by the guide.';
		immediateInvariant.
true.
%

doit
(FileSystemVisitor
	subclass: 'CopyVisitor'
	instVarNames: #( source dest )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I create a copy of the directory tree that I visit. I use the PreorderGuide so that I can create directories before creating their contents. ';
		immediateInvariant.
true.
%

doit
(FileSystemVisitor
	subclass: 'DeleteVisitor'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Core';
		comment: 'I delete the directory tree that I visit. I use the PostorderGuide so that I can delete files before deleting their containing directories.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsTonelOrderedDictionary'
	instVarNames: #( size keys values )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-GsBase-ComponentsV2';
		comment: 'I am an implementation of a dictionary. Compared to other dictionaries I am very efficient for small sizes, speed- and space-wise. I also mantain the order in which elements are added when iterating. My implementation features some ideas from the RefactoringBrowser.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'MemoryFileSystemEntry'
	instVarNames: #( creationTime modificationTime basename )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I am an abstract file system entry for a memory file system.
My subclasses should specialize on the kind of file they are.';
		immediateInvariant.
true.
%

doit
(MemoryFileSystemEntry
	subclass: 'MemoryFileSystemDirectory'
	instVarNames: #( entries )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I represent a memory file system entry for a directory';
		immediateInvariant.
true.
%

doit
(MemoryFileSystemEntry
	subclass: 'MemoryFileSystemFile'
	instVarNames: #( bytes size closed )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I represent a memory file system entry for a regular file';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'MemoryFileWriteStream'
	instVarNames: #( file writeStream stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'A file write stream - but within memory';
		immediateInvariant.
true.
%

doit
(Object
	indexableSubclass: 'Path'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Path';
		comment: 'I''m a private and abstract filesystem path, independent of the string representation used to describe paths on a specific filesystem. I provide methods for navigating the filesystem hierarchy and working with absolute and relative paths. I only refer to a concrete file or directory with regard to a specific filesystem. Normally users should not use me directly. 

API instance creation:

- #from: parses the supplied string using the default delimeter
- #from:delimiter: parses the supplied string using the supplied delimiter.
- #/ creates an absolute path from the supplied string
- #* creates a relative path from the supplied string

API path manipulation:

- #/ adds the supplied string to the receiver
';
		immediateInvariant.
true.
%

doit
(Path
	subclass: 'AbsolutePath'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Path';
		comment: 'I represent an absolute path (a position starting from Path root)';
		immediateInvariant.
true.
%

doit
(Path
	subclass: 'RelativePath'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Path';
		comment: 'I represent a relative path (a position starting from Path workingDirectory)';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RwAbstractConfigurationPlatformAttributeMatcher'
	instVarNames: #( pattern patternMatchBlock )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-Configurations-Common';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwAbstractConfigurationPlatformAttributeMatcher
	subclass: 'RwGemStoneVersionConfigurationPlatformAttributeMatcher'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-Configurations-Common';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwGemStoneVersionConfigurationPlatformAttributeMatcher
	subclass: 'RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher'
	instVarNames: #( pattern2 )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-Configurations-Common';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwAbstractConfigurationPlatformAttributeMatcher
	subclass: 'RwStringConfigurationPlatformAttributeMatcher'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-Configurations-Common';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RwAbstractProjectComponentVisitorV2'
	instVarNames: #( projectLoadSpecs visitedComponents visitedComponentNames platformConditionalAttributes definedGroupNames projectNames groupNames componentNames )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-ComponentsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwAbstractProjectComponentVisitorV2
	subclass: 'RwIndependentComponentVisitorV2'
	instVarNames: #( packageNames componentsPath projectsPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-ComponentsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwAbstractProjectComponentVisitorV2
	subclass: 'RwResolvedProjectComponentVisitorV2'
	instVarNames: #( resolvedProject )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-ComponentsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RwAbstractProjectLoadComponentV2'
	instVarNames: #( name comment projectName definedGroupNames conditionalProperties conditionalPropertyMatchers conditionalPackageMapSpecs conditionalPackageMapSpecMatchers )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-ComponentsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwAbstractProjectLoadComponentV2
	subclass: 'RwNestedProjectLoadComponentV2'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-ComponentsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwAbstractProjectLoadComponentV2
	subclass: 'RwProjectLoadComponentV2'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-ComponentsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RwSpecification'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-Specifications';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwSpecification
	subclass: 'RwLoadSpecificationV2'
	instVarNames: #( specName projectName projectAlias gitUrl diskUrl mercurialUrl svnUrl revision projectSpecFile componentNames groupNames customConditionalAttributes platformProperties comment projectsHome repositoryResolutionPolicy )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-SpecificationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwLoadSpecificationV2
	subclass: 'RwEmbeddedLoadSpecificationV2'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-SpecificationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwSpecification
	subclass: 'RwProjectSpecificationV2'
	instVarNames: #( specName projectName projectSpecPath componentsPath packagesPath projectsPath specsPath packageFormat packageConvention comment repoType loadedCommitId )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanTools
	options: #()
)
		category: 'Rowan-SpecificationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'RwUrl'
	instVarNames: #( fragment )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.

The classes in this hierarchy:

RwUrl
 RwFileUrl
  RwCypressUrl
  RwFiletreeUrl
   RwTonelUrl
  RwGithubUrl
 RwGenericUrl
 RwHierarchicalUrl
  RwSmalltalkRepositoryUrl
   RwGitFileTreeUrl

are specificaly designed for parsing URLS for Rowan source code repositories:

 file:/opt/git/shared/repos/rowan/sample/repository
 http://gsdevkit.github.io/GsDevKit_home/rowan/Sample.ston
 https://raw.githubusercontent.com/dalehenrich/sample/master/specs/Sample.ston

 github://GsDevKit/GsDevKit:master/repository
 filetree:///opt/git/shared/repos/rowan/sample/repository
 tonel:/opt/git/shared/repos/rowan/sample/repository
 gitfiletree://gitlab.com/GsDevKit/GsDevKit:master/repository

 cypress:/opt/git/shared/repos/rowan/sample/repository/
 smalltalk://dkh:pass@gitlab.ferlicot.fr:3456/Projet/Bazard:dev/src
 smalltalk://git@git.gemtalksystems.com/btree340:dev/repository
 smalltalk://git@github.com/GsDevKit/GsDevKit:350/repository
 smalltalk://github.com/GsDevKit/GsDevKit:350/repository


The file:, http: and https: schemes should conform to the standard specs. ZnUrl is used for parsing http: and https: urls.

The github:, filetree:, gitfiletree: and tonel: schemes are supported for backward compatibility with schemes that have historically been used to identify Metacello repository urls.

The cypress: and smalltalk: schemes are new and intended to be used moving forward with Metacello and Rowan.

The cypress: url is used to indicate that path to a Cypress-style repository, i.e., a disk-based format for storing Smalltalk packages in filetree or tonel format. A cypress: url does not define the specific repository type (filetree or tonel). The type of the repository is encoded in a .filetree file located in the directory specified by pathString of the url.

The smalltalk: scheme is based on Thierry Goubier''s gitfiletree url[1]. The original github: scheme cannot be used for arbitrary git servers or git servers using non-standard ports or requiring usernames and passwords. Thierry''s scheme handles the full range of possibilites.

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st';
		immediateInvariant.
true.
%

doit
(RwUrl
	subclass: 'RwFileUrl'
	instVarNames: #( host path isAbsolute )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'This class models a file URL according to (somewhat) RFC1738, see http://www.w3.org/Addressing/rfc1738.txt

Here is the relevant part of the RFC:

3.10 FILES

   The file URL scheme is used to designate files accessible on a
   particular host computer. This scheme, unlike most other URL schemes,
   does not designate a resource that is universally accessible over the
   Internet.

   A file URL takes the form:

       file://<host>/<path>

   where <host> is the fully qualified domain name of the system on
   which the <path> is accessible, and <path> is a hierarchical
   directory path of the form <directory>/<directory>/.../<name>.

   For example, a VMS file

     DISK$USER:[MY.NOTES]NOTE123456.TXT

   might become

     <URL:file://vms.host.edu/disk$user/my/notes/note12345.txt>

   As a special case, <host> can be the string "localhost" or the empty
   string; this is interpreted as `the machine from which the URL is
   being interpreted''.

   The file URL scheme is unusual in that it does not specify an
   Internet protocol or access method for such files; as such, its
   utility in network protocols between hosts is limited.

From the above we can conclude that the RFC says that the <path> part never starts or ends with a slash and is always absolute. If the last name can be a directory instead of a file is not specified clearly.

The path is stored as a SequenceableCollection of path parts.

Notes regarding non RFC features in this class:

- If the last path part is the empty string, then the FileUrl is referring to a directory. This is also shown with a trailing slash when converted to a String.

- The FileUrl has an attribute isAbsolute which signals if the path should be considered absolute or relative to the current directory. This distinction is not visible in the String representation of FileUrl, since the RFC does not have that.

- Fragment is supported (kept for historical reasons)

';
		immediateInvariant.
true.
%

doit
(RwFileUrl
	subclass: 'RwCypressUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'This class implements the cypress: scheme:

 cypress:/opt/git/shared/repos/rowan/sample/repository/

The cypress: url specifies the file system path to the directory where a disk-based Smalltalk package repository resides. The name comes from the original name used to specifay a cross-platform package disk layout[1].

This url does not encode the specific disk format used by the repository (like the filetree: and tonel: schemes). The disk format is specified in a .cypress file in the directory using STON ike the following:

  { #format : ''filetree'' }
  { #format : ''tonel'' }
  { #format : ''cypress'' }

When the cypress: url is resolved, the appropriate platform-specific repository reader will be used to read the packages from disk.

[1] https://raw.githubusercontent.com/CampSmalltalk/Cypress/master/img/CypressStructure-STIC2012.png';
		immediateInvariant.
true.
%

doit
(RwCypressUrl
	subclass: 'RwTonelUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'This class implements the tonel: scheme:

  tonel:/opt/git/shared/repos/rowan/sample/repository

The scheme is supported for backward compatibility with the tonel disk format[1]. Not sure whether it''s use has made it into the wild, but it _is_ currently being used in the Rowan implementation.

[1] https://github.com/pharo-vcs/tonel';
		immediateInvariant.
true.
%

doit
(RwFileUrl
	subclass: 'RwFiletreeUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'This class implements the filetree: scheme[1]:

 filetree:///opt/git/shared/repos/rowan/sample/repository

This scheme is supported for backward compatibility. Moving forward the cypress: scheme should be used (see RwCypressUrl).

[1] https://github.com/Metacello/metacello/blob/master/docs/MetacelloScriptingAPI.md#filetree';
		immediateInvariant.
true.
%

doit
(RwFileUrl
	subclass: 'RwGithubUrl'
	instVarNames: #( project committish dir )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'This class implements the github: scheme[1]:

  github://GsDevKit/GsDevKit:master/repository

This scheme is supported for backward compatibility. Moving forward the metacello: scheme should be used (see RwMetacelloRepositoryUrl).

[1] https://github.com/Metacello/metacello/blob/master/docs/MetacelloScriptingAPI.md#github';
		immediateInvariant.
true.
%

doit
(RwUrl
	subclass: 'RwGenericUrl'
	instVarNames: #( schemeName locator )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'a URL type that can''t be broken down in any systematic way.  For example, mailto: and telnet: URLs.  The part after the scheme name is stored available via the #locator message.';
		immediateInvariant.
true.
%

doit
(RwUrl
	subclass: 'RwHierarchicalUrl'
	instVarNames: #( schemeName authority path query port username password )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'A URL which has a hierarchical encoding.  For instance, http and ftp URLs are hierarchical.';
		immediateInvariant.
true.
%

doit
(RwHierarchicalUrl
	subclass: 'RwHttpUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwHttpUrl
	subclass: 'RwHttpsUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(RwHierarchicalUrl
	subclass: 'RwSmalltalkRepositoryUrl'
	instVarNames: #( project committish dir )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'This class implements the smalltalk: scheme:

 smalltalk://dkh:pass@gitlab.ferlicot.fr:3456/Projet/Bazard:dev/src
 smalltalk://git@git.gemtalksystems.com/btree340:dev/repository
 smalltalk://git@github.com/GsDevKit/GsDevKit:350/repository
 smalltalk://github.com/GsDevKit/GsDevKit:350/repository

The smalltalk: scheme is based on Thierry Goubier''s gitfiletree url[1]:

  smalltalk:// <[user[:password]@]host[:port]> / user/ projectName [ : versionIdentifier ] [ / repositoryPath ]

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st';
		immediateInvariant.
true.
%

doit
(RwSmalltalkRepositoryUrl
	subclass: 'RwGitFileTreeUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Url-Core';
		comment: 'This class implements the gitfiletree: scheme:

 gitfiletree://gitlab.com/GsDevKit/GsDevKit:master/repository

The gitfiletree scheme [1] encodes the following information:

  gitfiletree:// [ user [ : password ] @ ] host [ : port ] / user / project [ : branch ] [ / repositoryPath ]

From the method comment[1]:

  Parameters are:
		dir : the directory inside the repository where the target MC packages are.
		branch : the git branch to fetch.
		protocol: the user name part to add to the ssh Url, default to git, but can also be https (which implies read only access).
		readOnly : is the repository read only? If present, reduce the history to a minimum (and change the GUI).
	Alternative url syntax:
		gitfiletree://github.com/dalehenrich/filetree:pharo5.0_dev/repository
	with:
		host : github.com
		project : dalehenrich/filetree
		branch : pharo5.0_dev
		dir : repository

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STON'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STON implements serialization and materialization using the Smalltalk Object Notation format.
 
S y n t a x

	value
	  primitive-value
	  object-value
	  reference
	  nil
	primitive-value
	  number
	  true
	  false
	  symbol
	  string
	object-value
	  object
	  map
	  list
	object
	  classname map
	  classname list
	reference
	  @ int-index-previous-object-value
	map
	  {}
	  { members }
	members
	  pair
	  pair , members
	pair
	  string : value
	  symbol : value
	  number : value
	list
	  []
	  [ elements ]
	elements
	  value 
	  value , elements
	string
	  ''''
	  '' chars ''
	chars
	  char
	  char chars
	char
	  any-printable-ASCII-character-
	    except-''-"-or-\
	  \''
	  \"
	  \\
	  \/
	  \b
	  \f
	  \n
	  \r
	  \t
	  \u four-hex-digits
	symbol
	  # chars-limited
	  # '' chars ''
	chars-limited
	  char-limited
	  char-limited chars-limited
	char-limited
	  a-z A-Z 0-9 - _ . /
	classname
	  uppercase-alpha-char alphanumeric-char
	number
	  int
	  int frac
	  int exp
	  int frac exp
	int
	  digit
	  digit1-9 digits 
	  - digit
	  - digit1-9 digits
	frac
	  . digits
	exp
	  e digits
	digits
	  digit
	  digit digits
	e
	  e
	  e+
	  e-
	  E
	  E+
	  E-
';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONReader'
	instVarNames: #( readStream objects classes unresolvedReferences stringStream allowComplexMapKeys stack )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONReader materializes objects using the Smalltalk Object Notation format.

This parser is backwards compatible with standard JSON.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONReference'
	instVarNames: #( index )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONReference holds a forward reference to another object during materialization.
';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONStreamWriter'
	instVarNames: #( writer first )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONStreamWriter helps in streaming writing STON representations.
This is an abstract class.';
		immediateInvariant.
true.
%

doit
(STONStreamWriter
	subclass: 'STONListWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONArrayWriter helps in writing array based STON representations.
';
		immediateInvariant.
true.
%

doit
(STONListWriter
	subclass: 'STONShortListWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONShortArrayWriter helps in writing short array based STON representations.
';
		immediateInvariant.
true.
%

doit
(STONStreamWriter
	subclass: 'STONMapWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONDictionaryWriter helps in writing dictionary based STON representations.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONTestDomainObject'
	instVarNames: #( created modified integer float description color tags bytes boolean )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONTestDomainObject is used to support unit tests.

Instance Variables
	boolean:		<Boolean>
	bytes:			<ByteArray>
	color:			<Symbol>
	created:		<DateAndTime>
	description:	<String>
	float:			<Float>
	integer:		<Integer>
	modified:	<DateAndTime>
	tags:			<Array of: Symbol>';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONTestUser'
	instVarNames: #( username password enabled )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONTestUser is used to support unit tests.

Instance Variables
	enabled:		<Boolean>
	password:	<String>
	username:	<String>
';
		immediateInvariant.
true.
%

doit
(STONTestUser
	subclass: 'STONTestUser2'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONTestUser2 extends STONTestUser with explicit implementations of #fromSton: and #stonOn:';
		immediateInvariant.
true.
%

doit
(STONTestUser
	subclass: 'STONTestUser3'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONTestUser3 extends STONTestUser but wants nil instance variables to be written';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'STONWriter'
	instVarNames: #( writeStream prettyPrint newLine jsonMode referencePolicy level objects )
	classVars: #( STONCharacters STONSimpleSymbolCharacters )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Core';
		comment: 'STONWriter serializes objects using the Smalltalk Object Notation format. 

Customization options are:

- prettyPrint <Boolean> default is false
	if true, produce pretty printed output
- jsonMode <Boolean> default is false
	if true, the follow changes occur
	- strings are delimited with double quotes
	- nil is encoded as null
	- symbols are treated as strings
	- only STON listClass and STON mapClass instances are allowed as composite objects
	it is wise to also use either #error or #ignore as referencePolicy to avoid references
- referencePolicy <#normal|#ignore|#error> default is #normal
	if #normal, track and count object references and use references to implement sharing and break cycles
	if #error, track object references and signal STONWriterError when a shared reference is encountered
	if #ignore, don''t track object references which might loop forever on cycles
 ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnBufferedReadStream'
	instVarNames: #( stream buffer position limit )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnBufferedReadStream.

I wrap another ReadStream and add efficient buffering for the typical access pattern of parsers: sending lots of #next, #peek and #atEnd messages.

By design I do not implement #position and #position: or anything based on that.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnBufferedReadWriteStream'
	instVarNames: #( readStream writeStream lastRead )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnBufferedReadWriteStream.
I wrap a buffered read stream and a buffered write stream on the same file.

I discard my read buffer on writes, and flush my write buffer on reads.
Make sure to always send me #flush or #close when you''re done,
otherwise the last buffer might not yet have been written.
My class side''s #on:do: helps to ensure this.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnBufferedWriteStream'
	instVarNames: #( stream buffer position )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnBufferedWriteStream.
I wrap a write stream and add buffering.

Make sure to always send me #flush or #close when you''re done,
otherwise the last buffer might not yet have been written.
My class side''s #on:do: helps to ensure this.

I can wrap both binary or character streams and act accordingly.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnCharacterEncoder'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterEncoder, I encode and decode Character objects to and from a binary stream.
I am an abstract class with following protocol:

#nextFromStream:
#nextPut:toStream:
#encodedByteCountFor:
#backOnStream:

The first two are compatible with TextConverter and subclasses.

I add some convenience methods:

#encodeString:
#decodeBytes:
#encodedByteCountForString:

Contrary to older encoders, I work strictly from strings to bytes and vice versa and I will throw errors instead of silently ignoring them.

I also implement optimized bulk operations:

#next:putAll:startingAt:toStream:
#readInto:startingAt:count:fromStream:

Additionally, I can encode Integer code points to a binary stream as well as read Integer code points from a binary stream. This is in a sense a more fundamental operation that avoids instanciating Character objects.

#nextCodePointFromStream:
#nextPutCodePoint:toStream:
#encodedByteCountForCodePoint:

#decodeAsCodePoints:
#encodeCodePoints:
#encodedByteCountForCodePoints:

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(ZnCharacterEncoder
	subclass: 'ZnUTF8Encoder'
	instVarNames: #(  )
	classVars: #( Default )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am the GemStone/S implementation of ZnUTF8Encoder.
I implement the variable length UTF-8 encoding and decoding of Unicode according to RFC 3629.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-8

Part of Zinc HTTP Components.

I use the ICU library to encode strings and decode ByteArray and Utf8 instances ... ';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnCharacterReadWriteStream'
	instVarNames: #( readStream writeStream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am a read-write character stream. I am mainly used to open the Pharo source and changes files.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'ZnEncodedStream'
	instVarNames: #( stream encoder )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnEncodedStream, an abstract support class for read and write streams on an encoded binary stream.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnEncodedStream
	subclass: 'ZnEncodedReadStream'
	instVarNames: #( peeked )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnEncodedReadStream, an abstract support class for read streams on a binary encoded wrapped stream.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnEncodedReadStream
	subclass: 'ZnCharacterReadStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterReadStream.
I wrap another binary ReadStream and use a ZnCharacerEncoder to allow Characters to be read.

I am not positionable, but I do allow a one character peek using a one character internal buffer.

Part of Zinc HTTP Components.
';
		immediateInvariant.
true.
%

doit
(ZnEncodedStream
	subclass: 'ZnEncodedWriteStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnEncodedWriteStream, an abstract support class for write streams on a binary encoded wrapped stream.

Part of Zinc HTTP Components';
		immediateInvariant.
true.
%

doit
(ZnEncodedWriteStream
	subclass: 'ZnCharacterWriteStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Core';
		comment: 'I am ZnCharacterWriteStream.
I wrap another binary WriteStream and use a ZnCharacerEncoder to allow Characters to be written.

Part of Zinc HTTP Components.';
		immediateInvariant.
true.
%

doit
(Stream
	subclass: 'AbstractBinaryFileStream'
	instVarNames: #( file handle forWrite )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'Provides a simple, platform-independent, file stream. I am 
   - binary
   - not buffered
   - provide no encoding/conversions

!Examples of usage

"Creating a file"
stream := (File named: ''asd.txt'' asFileReference fullName) readStream.

"Accessing the stream properties"
stream position.
stream atEnd.

"Writing"
stream nextPut: 17.
stream nextPutAll: ''sdd''.

"Reading"
stream next.
stream next: 2.

"Skipping"
stream skip: 2. 

"reading up to something"
stream upTo: 23.
stream upToAnyOf: #[ 13 30 ].

"peeking"
stream peek.';
		immediateInvariant.
true.
%

doit
(AbstractBinaryFileStream
	subclass: 'BinaryFileStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files';
		comment: 'I am a concrete subclass of AbstractBinaryFileStream for normal files. Regardless the position of the file, I will make my operarions on my position and then return the file it''s own position.

In addition to my superclass'' API I provide the following methods.

stream upToEnd
"reads the full stream up to the end and returns the contents"';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'AbstractUUIDTest'
	instVarNames: #( currentUuidGeneratorClass )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(AbstractUUIDTest
	subclass: 'FastUUIDTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Network-UUID-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'DirectoryEntryTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for FileSystemDirectoryEntry';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileLocatorTest'
	instVarNames: #( locator )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit test for FileLocator';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileReferenceAttributeTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Attributes';
		comment: 'Try and test file attribute access from FileReference.

As Pharo doesn''t provide a mechanism to set the attributes, pick a few well known files and make sure they have reasonable attributes, e.g. the VM is executable, it isn''t older than when the classes were first created or newer than today, etc.
';
		immediateInvariant.
true.
%

doit
(TestCase
	indexableSubclass: 'FileReferenceTest'
	instVarNames: #( filesystem )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for file reference';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemHandleTest'
	instVarNames: #( filesystem handle reference )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for FileSystemHandle';
		immediateInvariant.
true.
%

doit
(FileSystemHandleTest
	subclass: 'FileHandleTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: 'SUnit tests for file handles, the tests may be found in superclass';
		immediateInvariant.
true.
%

doit
(FileSystemHandleTest
	subclass: 'MemoryHandleTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Memory';
		comment: 'SUnit tests for memory handles, the tests may be found in superclass';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemResolverTest'
	instVarNames: #( resolver )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for FileSystemResolver';
		immediateInvariant.
true.
%

doit
(FileSystemResolverTest
	subclass: 'InteractiveResolverTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for InteractiveResolver';
		immediateInvariant.
true.
%

doit
(FileSystemResolverTest
	subclass: 'PlatformResolverTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for PlatformResolver';
		immediateInvariant.
true.
%

doit
(FileSystemResolverTest
	subclass: 'SystemResolverTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for SystemResolver';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemTest'
	instVarNames: #( filesystem toDelete )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'I am an abstract file system test. 
I directly test 
- the public interface of a FileSystem using these methods directly
- the FileSystem in general through the operation methods of the FileReference';
		immediateInvariant.
true.
%

doit
(FileSystemTest
	subclass: 'DiskFileSystemTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: 'SUnit tests for the disk filesystem';
		immediateInvariant.
true.
%

doit
(FileSystemTest
	subclass: 'MemoryFileSystemTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Memory';
		comment: 'SUnit tests for MemoryFileSystem';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileSystemTreeTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for a file system tree';
		immediateInvariant.
true.
%

doit
(FileSystemTreeTest
	subclass: 'CopyVisitorTest'
	instVarNames: #( source dest )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for CopyVisitor';
		immediateInvariant.
true.
%

doit
(FileSystemTreeTest
	subclass: 'SingleTreeTest'
	instVarNames: #( filesystem )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for a single file system tree (depth one)';
		immediateInvariant.
true.
%

doit
(SingleTreeTest
	subclass: 'AbstractEnumerationVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for AbstractEnumerationVisitor';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitorTest
	subclass: 'CollectVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class CollectVisitor';
		immediateInvariant.
true.
%

doit
(AbstractEnumerationVisitorTest
	subclass: 'SelectVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class SelectVistor
';
		immediateInvariant.
true.
%

doit
(SingleTreeTest
	subclass: 'DeleteVisitorTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class DeleteVisitor';
		immediateInvariant.
true.
%

doit
(SingleTreeTest
	subclass: 'GuideTest'
	instVarNames: #( guide visited )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'Common superclass for tests of guidance through the filesystem';
		immediateInvariant.
true.
%

doit
(GuideTest
	subclass: 'BreadthFirstGuideTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class BreadthFirstGuide';
		immediateInvariant.
true.
%

doit
(GuideTest
	subclass: 'PostorderGuideTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class PostorderGuide';
		immediateInvariant.
true.
%

doit
(GuideTest
	subclass: 'PreorderGuideTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for class PreorderGuide';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FileTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Files-Tests';
		comment: 'Unit tests for file operations';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'FSGemStoneKernelTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-GemStone';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	indexableSubclass: 'PathTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Core';
		comment: 'SUnit tests for file system paths';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'RwGemStoneVersionNumberTestCase'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tests-DiskConfigurationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'RwLoadSpecificationV2Test'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tests-DiskConfigurationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'RwProjectComponentVisitorV2Test'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tests-DiskConfigurationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'RwProjectLoadComponentV2Test'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tests-DiskConfigurationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'RwProjectSpecificationV2Test'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tests-DiskConfigurationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'RwSemanticVersionNumber200TestCase'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tests-DiskConfigurationsV2';
		comment: 'The tests in this class were extracted from the examples in Semantic Version Specification ...';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'RwSemanticVersionNumberTestCase'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: RowanKernel
	options: #()
)
		category: 'Rowan-Tests-DiskConfigurationsV2';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'STONReaderTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONReaderTests test materialization.
';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'STONTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONTests tests the API offered by STON.
';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'STONWriteReadTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONWriteReadTests test serialization followed by materialization.';
		immediateInvariant.
true.
%

doit
(STONWriteReadTests
	subclass: 'STONLargeWriteReadTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONLargeWriteReadTests test the optimalizations for large structures.';
		immediateInvariant.
true.
%

doit
(STONWriteReadTests
	subclass: 'STONWritePrettyPrinterReadTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONWritePrettyPrinterReadTests tests pretty printed serialization followed by materialization.';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'STONWriterTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'STON-Tests';
		comment: 'STONWriterTests test serialization.';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'WindowsStoreTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Disk';
		comment: 'SUnit tests for class WindowsStore';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedReadStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: 'I am ZnBufferedReadStreamTests.';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedReadWriteStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedStreamByteTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: 'Tests for bufferer read/write streams that are assumed to contain bytes.

Right now, integer encoding/decoding

References

https://en.wikipedia.org/wiki/Endianness
https://en.wikipedia.org/wiki/Two%27s_complement';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnBufferedWriteStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnCharacterEncoderTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'ZnCharacterStreamTests'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Zinc-Character-Encoding-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestResource
	subclass: 'DiskFileAttributesTestsResources'
	instVarNames: #( file beforeCreationTime afterCreationTime )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Tests-Attributes';
		comment: 'DiskFileAttributesTestsResources provides a file with known attributes that can be used to test attribute retrieval in the File System.

DiskFileAttributesTestsResources is currently used by FileReferenceAttributeTests.

    Instance Variables
	afterCreationTime:	The time just after file was created	<DateAndTime>
	beforeCreationTime:	The time just before file was created	<DateAndTime>
	file:					The name of the file (with known attributes)	<FileReference>


    Implementation Points';
		immediateInvariant.
true.
%

doit
(WriteStreamPortable
	subclass: 'MemoryWriteStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'FileSystem-Memory';
		comment: 'I am like a WriteStream but I have more capacities than it.
I am closer to a FileStream so I delegate some operations to my handle (collection instance variable)';
		immediateInvariant.
true.
%

! Class implementation for 'UUID'

!		Class methods for 'UUID'

category: 'instance creation'
classmethod: UUID
fromString: aString
	| object |
	aString size ~= 36 ifTrue: [Error signal].
	object := self nilUUID. 
	object asUUID: aString.
	^object
%

category: 'accessing'
classmethod: UUID
generatorClass
  self _generatorClass ifNil: [ ^ FastUUIDGenerator ].
  ^ self _generatorClass
%

category: 'accessing'
classmethod: UUID
generatorClass: aClass
  GeneratorClass := aClass.
  aClass ifNotNil: [ aClass initialize ]
%

category: 'instance creation'
classmethod: UUID
new
  ^ (self new: 16) initialize
%

category: 'instance creation'
classmethod: UUID
nilUUID
	"Must call basicNew: here because I have a non-trivial initialize method."

	^self basicNew: 16
%

category: 'private'
classmethod: UUID
_generatorClass
  ^ GeneratorClass
%

!		Instance methods for 'UUID'

category: 'comparing'
method: UUID
< aMagnitude
  "Answer whether the receiver is less than the argument."

  self size = aMagnitude size
    ifFalse: [ ^ self size < aMagnitude size ].
  1 to: self size do: [ :i | 
    (self at: i) = (aMagnitude at: i)
      ifFalse: [ ^ (self at: i) < (aMagnitude at: i) ] ].
  ^ false
%

category: 'comparing'
method: UUID
<= uuid
	^ (self = uuid) or: [ self < uuid ]
%

category: 'comparing'
method: UUID
> uuid
	^ uuid < self
%

category: 'comparing'
method: UUID
>= uuid
	^ (self = uuid) or: [ uuid < self ]
%

category: 'converting'
method: UUID
asInteger
  ^ self inject: 0 into: [ :sum :each | sum * 256 + each ]
%

category: 'converting'
method: UUID
asString
	| result data |
	data := String new: 36.
	result := WriteStream on: data.
	1 to: 4 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	5 to: 6 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	7 to: 8 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	9 to: 10 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	11 to: 16 do:[:i| self printHexAt: i to: result].
	^data.
%

category: 'converting'
method: UUID
asString36
	"Encode the UUID as a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems, and it fits into short filenames like on
	the old MacOS HFS filesystem. The check for 36r is to make this code
	work in versions before Squeak 3.8."

	| num candidate |
	num := 0.
	1 to: self size do: [:i | num := num + ((256 raisedTo: i - 1) * (self at: i))].
	candidate := num printStringBase: 36.
	^((candidate beginsWith: '36r')
			ifTrue: [candidate copyFrom: 4 to: candidate size]
			ifFalse: [candidate]) asLowercase
%

category: 'converting'
method: UUID
asUUID: aString
	| stream token byte sz |
	stream := ReadStream on: (aString copyReplaceAll: '-' with: '') asUppercase.
	sz := stream _collection size.
	1 to: sz/2 do: [:i | 
		token := stream next: 2.
		byte := Integer fromHexString: token.
		self at: i put: byte].
	^self
%

category: 'initialization'
method: UUID
initialize
  self _primMakeUUID
%

category: 'testing'
method: UUID
isNilUUID
  1 to: self size do: [ :i | 
    (self at: i) ~= 0
      ifTrue: [ ^ false ] ].
  ^ true
%

category: 'converting'
method: UUID
printHexAt: index to: aStream
	| map v |
	map := '0123456789abcdef'.
	v := self at: index.
	aStream nextPut: (map at: (v bitShift: -4) + 1). 
	aStream nextPut: (map at: (v bitAnd: 15) + 1).
%

category: 'printing'
method: UUID
printOn: aStream
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'
%

category: 'printing'
method: UUID
printString

	^self asString
%

category: 'private'
method: UUID
_primMakeUUID
  self class generatorClass default generateBytes: self forVersion: 4
%

! Class implementation for 'STONTestMap'

!		Class methods for 'STONTestMap'

category: 'instance creation'
classmethod: STONTestMap
classTree
	^ self classTree: Object
%

category: 'instance creation'
classmethod: STONTestMap
classTree: topClass
	| map |
	map := IdentityDictionary new.
	topClass withAllSubclasses do: [ :eachClass | | info |
		(info := self new)
			at: #name put: eachClass name asString;
			at: #comment put: eachClass comment;
			at: #isMeta put: eachClass isMeta;
			at: #methods put: eachClass selectors.		
		map at: eachClass put: info ].
	map keysAndValuesDo: [ :eachClass :eachInfo |
		eachClass == topClass
			ifFalse: [ eachInfo at: #superclass put: (map at: eachClass superclass) ].
		eachInfo at: #subclasses put: (eachClass subclasses collect: [ :subClass | map at: subClass ]) ].
	^ map at: topClass
%

category: 'instance creation'
classmethod: STONTestMap
classTreeExtended
	^ self classTreeExtended: Object
%

category: 'instance creation'
classmethod: STONTestMap
classTreeExtended: topClass
	| map |
	map := IdentityDictionary new.
	topClass withAllSubclasses do: [ :eachClass | | info methodsInfo |
		(info := self new)
			at: #name put: eachClass name asString;
			at: #comment put: eachClass comment;
			at: #isMeta put: eachClass isMeta;
			at: #methods put: (methodsInfo := self new).
		eachClass methods do: [ :eachMethod | | methodInfo |
			(methodInfo := self new)
				at: #name put: eachMethod selector;
				at: #numArgs put: eachMethod numArgs;
				at: #class put: info.
			methodsInfo at: eachMethod selector put: methodInfo ].
		map at: eachClass put: info ].
	map keysAndValuesDo: [ :eachClass :eachInfo |
		eachClass == topClass 
			ifFalse: [ eachInfo at: #superclass put: (map at: eachClass superclass) ].
		eachInfo at: #subclasses put: (eachClass subclasses collect: [ :subClass | map at: subClass ]) ].
	^ map at: topClass
%

category: 'ston-core'
classmethod: STONTestMap
stonName
	^ #TestMap
%

!		Instance methods for 'STONTestMap'

category: 'printing'
method: STONTestMap
printElementsOn: stream
	stream
		nextPut: $(;
		nextPut: $#;
		print: self size;
		nextPut: $)
%

! Class implementation for 'FileException'

!		Class methods for 'FileException'

category: 'exceptioninstantiator'
classmethod: FileException
fileName: aFileName
	^self new fileName: aFileName
%

category: 'instance creation'
classmethod: FileException
signalOnFile: aFile 
	
	(self fileName: aFile basename) signal: aFile name
%

category: 'instance creation'
classmethod: FileException
signalWith: aReference
	"Signal a new instance of the receiver with the supplied reference.
	aReference is something that can be converted to a path, e.g. a String, Path or FileReference"
  | str |
	^(self fileName: (str := aReference asPath pathString)) signal: str
%

!		Instance methods for 'FileException'

category: 'exceptiondescription'
method: FileException
fileName
	^fileName
%

category: 'exceptionbuilder'
method: FileException
fileName: aFileName
	fileName := aFileName
%

category: 'exceptiondescription'
method: FileException
isResumable
	"Determine whether an exception is resumable."

	^true
%

category: 'exceptiondescription'
method: FileException
messageText
	"Return an exception's message text."

	^ messageText isNil
		ifTrue: [ fileName printString ]
		ifFalse: [ messageText ]
%

! Class implementation for 'FileAlreadyExistsException'

!		Class methods for 'FileAlreadyExistsException'

category: 'instance creation'
classmethod: FileAlreadyExistsException
signalOnFile: aFile 
	
	self new
		file: aFile;
		signal: aFile name "use signal: so filename shows up in exception printString"
%

!		Instance methods for 'FileAlreadyExistsException'

category: 'accessing'
method: FileAlreadyExistsException
file
	^ file
%

category: 'accessing'
method: FileAlreadyExistsException
file: aFile
	
	file := aFile
%

category: 'accessing'
method: FileAlreadyExistsException
messageText

	^ 'File already exists: ', file basename
%

! Class implementation for 'FileDoesNotExistException'

!		Class methods for 'FileDoesNotExistException'

category: 'signalling'
classmethod: FileDoesNotExistException
signalWithFile: aFile writeMode: writeMode

  | ex |
	^ (ex := self fileName: aFile basename)
		readOnly: writeMode not;
		signal: ex fileName  "use signal: so file name shows up in  ex printString"
%

!		Instance methods for 'FileDoesNotExistException'

category: 'accessing'
method: FileDoesNotExistException
readOnly
	^readOnly == true
%

category: 'accessing'
method: FileDoesNotExistException
readOnly: aBoolean
	readOnly := aBoolean
%

! Class implementation for 'FileSystemError'

!		Class methods for 'FileSystemError'

category: 'instance creation'
classmethod: FileSystemError
reference: aReference
	^ self basicNew initializeWithReference: aReference
%

category: 'instance creation'
classmethod: FileSystemError
signalWith: aReference
	^ (self reference: aReference) signal
%

!		Instance methods for 'FileSystemError'

category: 'initialize-release'
method: FileSystemError
initializeWithReference: aReference
	reference := aReference.
	messageText := aReference printString
%

category: 'testing'
method: FileSystemError
isResumable
	^ true
%

category: 'accessing'
method: FileSystemError
reference
	^ reference
%

! Class implementation for 'IllegalName'

!		Class methods for 'IllegalName'

category: 'instance creation'
classmethod: IllegalName
name: aName
	^ self basicNew initializeWithName: aName
%

category: 'instance creation'
classmethod: IllegalName
signalWith: aName
	^ (self name: aName) signal
%

!		Instance methods for 'IllegalName'

category: 'initialization'
method: IllegalName
initializeWithName: aName
	name := aName.
	self messageText: aName
%

category: 'accessing'
method: IllegalName
name
	^ name
%

! Class implementation for 'STONReaderError'

!		Class methods for 'STONReaderError'

category: 'instance creation'
classmethod: STONReaderError
signal: aString streamPosition: streamPosition 
	^ self new
		streamPosition: streamPosition;
		signal: aString;
		yourself
%

!		Instance methods for 'STONReaderError'

category: 'accessing'
method: STONReaderError
messageText
	^ streamPosition 
		ifNil: [ 
			super messageText ] 
		ifNotNil: [ :pos | 
			'At character {1}: {2}' format: 
				(Array with: streamPosition with: super messageText) ]
%

category: 'accessing'
method: STONReaderError
streamPosition
	^ streamPosition
%

category: 'accessing'
method: STONReaderError
streamPosition: aNumber
	streamPosition := aNumber
%

! Class implementation for 'RwGemStoneVersionNumber'

!		Class methods for 'RwGemStoneVersionNumber'

category: 'instance creation'
classmethod: RwGemStoneVersionNumber
fromString: aString

	| new components |
	components := OrderedCollection new.
	(aString substrings: '.') do: [:subString | 
		components add: subString asInteger].
	new := self new: components size.
	1 to: components size do: [:i | new at: i put: (components at: i) ].
	^new
%

!		Instance methods for 'RwGemStoneVersionNumber'

category: 'comparing'
method: RwGemStoneVersionNumber
< aRwGemStoneVersionNumber

	| condensed aCondensed |
	aRwGemStoneVersionNumber species = self species
		ifFalse: [ ^ false ].
	condensed := self collapseZeros.
	aCondensed := aRwGemStoneVersionNumber collapseZeros.
	(condensed ~~ self or: [ aCondensed ~~ aRwGemStoneVersionNumber ])
		ifTrue: [ ^ condensed compareLessThan: aCondensed ].
	^ self compareLessThan: aRwGemStoneVersionNumber
%

category: 'comparing'
method: RwGemStoneVersionNumber
= aRwGemStoneVersionNumber

	| condensed aCondensed |
	aRwGemStoneVersionNumber species = self species
		ifFalse: [ ^ false ].
	condensed := self collapseZeros.
	aCondensed := aRwGemStoneVersionNumber collapseZeros.
	(condensed ~~ self or: [ aCondensed ~~ aRwGemStoneVersionNumber ])
		ifTrue: [ ^ condensed compareEqualTo: aCondensed ].
	^ self compareEqualTo: aRwGemStoneVersionNumber
%

category: 'accessing'
method: RwGemStoneVersionNumber
approximateBase

	| base condensed |
	condensed := self collapseZeros.
	base := condensed copyFrom: 1 to: condensed size - 1.
	base at: base size put: (base at: base size) + 1.
	^base
%

category: 'converting'
method: RwGemStoneVersionNumber
asRwGemStoneVersionNumber

	^self
%

category: 'printing'
method: RwGemStoneVersionNumber
asString
	"Answer a string that represents the receiver."

	^ self printString
%

category: 'private'
method: RwGemStoneVersionNumber
collapseZeros
	"the rule must be that zeros can be collapsed as long as the series of zeros ends in a 0"

	| collection newSize new j lastElementIsStringOrZero canCollapse |
	(self size = 0 or: [ self at: 1 ]) == 0
		ifTrue: [ ^ self ].
	collection := OrderedCollection new.
	lastElementIsStringOrZero := true.
	canCollapse := true.
	self size to: 1 by: -1 do: [ :i | 
		| element |
		element := self at: i.
		(canCollapse and: [ element == 0 ])
			ifTrue: [ 
				lastElementIsStringOrZero
					ifFalse: [ 
						canCollapse := false.
						collection addFirst: element.]]
			ifFalse: [ 
				collection addFirst: element.
				canCollapse := lastElementIsStringOrZero := element isString ] ].
	collection size = self size
		ifTrue: [ ^ self ].
	newSize := collection size.
	new := self species new: newSize.
	j := 0.
	collection
		do: [ :element | 
			new at: j + 1 put: element.
			j := j + 1 ].
	^ new
%

category: 'private'
method: RwGemStoneVersionNumber
compareEqualTo: aRwGemStoneVersionNumber

	| mySize |
	aRwGemStoneVersionNumber species = self species ifFalse: [ ^false ].
	mySize := self size.
	mySize = aRwGemStoneVersionNumber size 
		ifFalse: [ ^false ].
	1 to: mySize do: [:i |
		(self at: i) = (aRwGemStoneVersionNumber at: i) ifFalse: [ ^false ]].
	^true
%

category: 'private'
method: RwGemStoneVersionNumber
compareLessThan: aRwGemStoneVersionNumber

	| mySize aSize commonSize count more |
	mySize := self size.
	aSize := aRwGemStoneVersionNumber size.
	commonSize :=  mySize min: aSize.
	count := 0.
	more := true.
	[ more and: [ count < commonSize ]] whileTrue: [
		(self at: count + 1) = (aRwGemStoneVersionNumber at: count + 1)
			ifTrue: [ count := count + 1 ]
			ifFalse: [ more := false ]].
	count < commonSize
		ifTrue: [ 
			^(self at: count + 1) < (aRwGemStoneVersionNumber at: count + 1) ].
	mySize < aSize
		ifTrue: [ 
			mySize = 0 ifTrue: [ ^true ].
			"if the versions at commonSize are equal and the next version slot in aRwGemStoneVersionNumber 
			 is a string, then it's considered that I'm > aRwGemStoneVersionNumber
			 (i.e., '2.9.9' is greater than '2.9.9-alpha.2')"
			(self at: commonSize) = (aRwGemStoneVersionNumber at: commonSize)
				ifFalse: [ ^true ]. 
			^(aRwGemStoneVersionNumber at: commonSize+1) isString not]
		ifFalse: [ 
			mySize = aSize ifTrue: [ ^false ].
			aSize <= 0 ifTrue: [ ^false ].
			"if the versions at commonSize are equal and the next version slot is a string, 
			 then it's considered that I'm < aRwGemStoneVersionNumber
			 (i.e., '2.9.9-alpha.2' is less than '2.9.9')"
			(self at: commonSize) = (aRwGemStoneVersionNumber at: commonSize)
				ifFalse: [ ^false ].
			 ^(self at: commonSize+1) isString]
%

category: 'copying'
method: RwGemStoneVersionNumber
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize new j |
	newSize := stop - start + 1.
	new := self species new: newSize.
	j := 0.
	start to: stop do: [:i |
		new at: j + 1 put: (self at: i).
		j := j + 1 ].
	^new
%

category: 'enumerating'
method: RwGemStoneVersionNumber
do: aBlock 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]
%

category: 'enumerating'
method: RwGemStoneVersionNumber
do: elementBlock separatedBy: separatorBlock
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: each]
%

category: 'comparing'
method: RwGemStoneVersionNumber
hash

"Returns a numeric hash key for the receiver."

| mySize interval hashValue |

(mySize := self size) == 0
  ifTrue: [ ^15243 ].

"Choose an interval so that we sample at most 5 elements of the receiver"
interval := ((mySize - 1) // 4) max: 1.

hashValue := 4459.
1 to: mySize by: interval do: [ :i | | anElement |
  anElement := self at: i.
  (anElement isKindOf: SequenceableCollection)
    ifTrue: [
      hashValue := (hashValue bitShift: -1) bitXor: anElement size.
      ]
    ifFalse: [
      hashValue := (hashValue bitShift: -1) bitXor: anElement hash.
      ].
  ].

^ hashValue abs
%

category: 'printing'
method: RwGemStoneVersionNumber
printOn: aStream

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [
				each isString
					ifTrue: [ aStream nextPut: $- ]
					ifFalse: [ aStream nextPut: $. ] ].
		aStream nextPutAll: each asString ]
%

category: 'matching'
method: RwGemStoneVersionNumber
rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher

	^ anRwGemStoneVersionConfigurationPlatformAttributeMatcher matchVersion: self
%

category: 'matching'
method: RwGemStoneVersionNumber
rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher

	^ anRwStringConfigurationPlatformAttributeMatcher matchString: self printString
%

category: 'accessing'
method: RwGemStoneVersionNumber
versionString

	| strm |
	strm := WriteStream on: String new.
	self printOn: strm.
	^strm contents
%

category: 'comparing'
method: RwGemStoneVersionNumber
~> aRwGemStoneVersionNumber

	aRwGemStoneVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aRwGemStoneVersionNumber and: [ self < aRwGemStoneVersionNumber approximateBase ]
%

! Class implementation for 'RwSemanticVersionNumber'

!		Class methods for 'RwSemanticVersionNumber'

category: 'private'
classmethod: RwSemanticVersionNumber
extractNumericComponent: subString
    "$. separated components are integers"

    | number stream |
	stream := subString readStream.
	number := [ Integer fromStream: stream ] on: Error do: [:ex | ^ subString ].
	^ stream atEnd
		ifTrue: [ 
			(subString size > 1 and: [ (subString at: 1) = $0 ])
				ifTrue: [ self error: 'invalid version number: numberic components may not have a leading 0' ]
				ifFalse: [ number ] ]
		ifFalse: [ subString ]
%

category: 'instance creation'
classmethod: RwSemanticVersionNumber
fromString: aString
  | preRelease build versionString identifierCount normalEnd preReleaseEnd normalComponents preReleaseComponents buildComponents |
  normalComponents := OrderedCollection new.
  preReleaseComponents := OrderedCollection new.
  buildComponents := OrderedCollection new.
  preRelease := aString indexOf: $- startingAt: 1.
  build := aString indexOf: $+ startingAt: 1.
  (build > 0 and: [ preRelease > build ])
    ifTrue: [ preRelease := 0 ].
  normalEnd := preRelease = 0
    ifTrue: [ 
      build = 0
        ifTrue: [ aString size ]
        ifFalse: [ build - 1 ] ]
    ifFalse: [ preRelease - 1 ].
  versionString := aString copyFrom: 1 to: normalEnd.
  identifierCount := 0.
  (versionString subStrings: '.')
    do: [ :subString | 
      | integer |
      subString isEmpty
        ifTrue: [ self error: 'invalid version number: normal version component MUST NOT be empty' ].
	  integer := self integerFromString: subString.
	  integer < 0
		ifTrue: [ 
		  self
			error:
			  'invalid version number: normal version component MUST be integer '
				, subString printString ] .
      normalComponents add: integer.
      identifierCount := identifierCount + 1 ].
  identifierCount ~= 3
    ifTrue: [ self error: 'invalid version number: normal version MUST have only 3 components' ].
  preReleaseEnd := build = 0
    ifTrue: [ aString size ]
    ifFalse: [ build - 1 ].
  preRelease > 0
    ifTrue: [ 
      versionString := aString copyFrom: preRelease + 1 to: preReleaseEnd.
      (versionString subStrings: '.')
        do: [ :subString | 
	      subString isEmpty
              ifTrue: [ self error: 'invalid version number: preRelease version component MUST NOT be empty' ].
          (self isSemanticIdentifier: subString)
            ifFalse: [ 
              self
                error:
                  'invalid version number: preRelease version component must be one of [0-9A-Za-z-], MUST NOT be empty, and first component MUST NOT be 0' ].
          preReleaseComponents
            add:
              (self extractNumericComponent: subString) ] ].
  build > 0
    ifTrue: [ 
      versionString := aString copyFrom: build + 1 to: aString size.
      (versionString subStrings: '.')
        do: [ :subString | 
	      subString isEmpty
              ifTrue: [ self error: 'invalid version number: preRelease version component MUST NOT be empty' ].
          (self isSemanticIdentifier: subString)
            ifFalse: [ 
              self
                error:
                  'invalid version number: build version component must be one of [0-9A-Za-z-] and MUST NOT be empty' ].
          buildComponents add: subString ] ].

  ^ self new
    normalVersion: normalComponents;
    preReleaseVersion: preReleaseComponents;
    buildVersion: buildComponents;
    yourself
%

category: 'private'
classmethod: RwSemanticVersionNumber
integerFromString: aString
  aString
    detect: [ :char | char isDigit not ]
    ifNone: [ 
      | integer |
      integer := aString asInteger.
      ((aString at: 1) = $0 and: [ aString size > 1 ])
        ifTrue: [ 
          self
            error:
              'invalid version number: normal version component must not have leading 0s'
                , aString asString ].
      ^ integer ].
  self
    error:
      'invalid version number: normal version component must be integer '
        , aString asString
%

category: 'private'
classmethod: RwSemanticVersionNumber
isSemanticIdentifier: aString
    "whether the receiver is composed entirely of alphanumerics"

   aString do: [ :c | 
     c isAlphaNumeric
       ifFalse: [ c = $- ifFalse: [ ^ false ] ] ].
    ^ true
%

category: 'private'
classmethod: RwSemanticVersionNumber
validateVersionNumber: svn against: aString
  svn printString = aString
    ifFalse: [ 
      self
        error:
          'The printString of a semantic version number should be equal to the source version string' ]
%

!		Instance methods for 'RwSemanticVersionNumber'

category: 'comparing'
method: RwSemanticVersionNumber
< aRwSemanticVersionNumber
    aRwSemanticVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareLessThan: aRwSemanticVersionNumber
%

category: 'comparing'
method: RwSemanticVersionNumber
= aMetacelloVersionNumber
    aMetacelloVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareEqualTo: aMetacelloVersionNumber
%

category: 'accessing'
method: RwSemanticVersionNumber
approximateBase

	| base |
	base := self copyFrom: 1 to: self size - 1.
	base at: base size put: (base at: base size) + 1.
	^base
%

category: 'converting'
method: RwSemanticVersionNumber
asRwSemanticVersionNumber
    ^ self
%

category: 'printing'
method: RwSemanticVersionNumber
asString
	"Answer a string that represents the receiver."

	^ self printString
%

category: 'accessing'
method: RwSemanticVersionNumber
buildVersion
    buildVersion ifNil: [ buildVersion := #() ].
    ^ buildVersion
%

category: 'accessing'
method: RwSemanticVersionNumber
buildVersion: anObject
	buildVersion := anObject
%

category: 'private'
method: RwSemanticVersionNumber
compareEqualTo: aRwSemanticVersionNumber
    aRwSemanticVersionNumber species = self species
        ifFalse: [ ^ false ].
    (self compareEqualTo: self normalVersion other: aRwSemanticVersionNumber normalVersion)
        ifFalse: [ ^ false ].
    (self compareEqualTo: self preReleaseVersion other: aRwSemanticVersionNumber preReleaseVersion)
        ifFalse: [ ^ false ].
    ^ true
%

category: 'private'
method: RwSemanticVersionNumber
compareEqualTo: myComponents other: otherComponents
    | mySize |
    mySize := myComponents size.
    mySize = otherComponents size
        ifFalse: [ ^ false ].
    1 to: mySize do: [ :i | 
        (myComponents at: i) = (otherComponents at: i)
            ifFalse: [ ^ false ] ].
    ^ true
%

category: 'private'
method: RwSemanticVersionNumber
compareLessThan: aRwSemanticVersionNumber
    | myComponents otherComponents defaultResult |
    aRwSemanticVersionNumber species = self species
        ifFalse: [ ^ false ].
    myComponents := self normalVersion.
    otherComponents := aRwSemanticVersionNumber normalVersion.
    defaultResult := true.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ defaultResult := false ]
        ifFalse: [ 
            (self compareLessThan: myComponents other: otherComponents version: #'normal')
                ifFalse: [ ^ false ] ].
    myComponents := self preReleaseVersion.
    otherComponents := aRwSemanticVersionNumber preReleaseVersion.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ 
            myComponents size > 0
                ifTrue: [ defaultResult := false ] ]
        ifFalse: [ ^ self compareLessThan: myComponents other: otherComponents version: #'preRelease' ].
    ^ defaultResult
%

category: 'private'
method: RwSemanticVersionNumber
compareLessThan: myComponents other: otherComponents version: version
    | mySize aSize commonSize count more |
    mySize := myComponents size.
    aSize := otherComponents size.
    commonSize := mySize min: aSize.
    count := 0.
    more := true.
    [ more and: [ count < commonSize ] ]
        whileTrue: [ 
            (myComponents at: count + 1) = (otherComponents at: count + 1)
                ifTrue: [ count := count + 1 ]
                ifFalse: [ more := false ] ].
    count < commonSize
        ifTrue: [ ^ (myComponents at: count + 1) rwSemanticVersionComponentLessThan: (otherComponents at: count + 1) ].
    mySize < aSize
        ifTrue: [ 
            mySize = 0
                ifTrue: [ 
                    #'preRelease' == version
                        ifTrue: [ ^ false ].
                    ^ true ].
            (myComponents at: commonSize) = (otherComponents at: commonSize)
                ifFalse: [ ^ true ].
            ^ true ]
        ifFalse: [ 
            mySize = aSize
                ifTrue: [ ^ false ].
            aSize = 0
                ifTrue: [ 
                    #'build' == version
                        ifTrue: [ ^ false ].
                    ^ true ].
            (myComponents at: commonSize) = (otherComponents at: commonSize)
                ifFalse: [ ^ false ].
            ^ true ]
%

category: 'copying'
method: RwSemanticVersionNumber
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize new j |
	newSize := stop - start + 1.
	new := self species new: newSize.
	j := 0.
	start to: stop do: [:i |
		new at: j + 1 put: (self at: i).
		j := j + 1 ].
	^new
%

category: 'operations'
method: RwSemanticVersionNumber
decrementMajorVersion
  self decrementNormalVersionAt: 1
%

category: 'operations'
method: RwSemanticVersionNumber
decrementMinorVersion
  self decrementNormalVersionAt: 2
%

category: 'operations'
method: RwSemanticVersionNumber
decrementMinorVersionNumber
  self decrementNormalVersionAt: 3
%

category: 'private'
method: RwSemanticVersionNumber
decrementNormalVersionAt: index
  | int col |
  col := self normalVersion.
  int := col at: index.
  int > 0
    ifTrue: [ col at: index put: int - 1 ]
%

category: 'operations'
method: RwSemanticVersionNumber
decrementPatchVersion
  self decrementNormalVersionAt: 3
%

category: 'enumerating'
method: RwSemanticVersionNumber
do: aBlock 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]
%

category: 'enumerating'
method: RwSemanticVersionNumber
do: elementBlock separatedBy: separatorBlock
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: each]
%

category: 'comparing'
method: RwSemanticVersionNumber
hash
    ^ self versionComponents hash
%

category: 'operations'
method: RwSemanticVersionNumber
incrementMajorVersion
  self incrementNormalVersionAt: 1
%

category: 'operations'
method: RwSemanticVersionNumber
incrementMinorVersion
  self incrementNormalVersionAt: 2
%

category: 'operations'
method: RwSemanticVersionNumber
incrementMinorVersionNumber
  self incrementNormalVersionAt: 3
%

category: 'private'
method: RwSemanticVersionNumber
incrementNormalVersionAt: index
  | int col |
  col := self normalVersion.
  int := col at: index.
  col at: index put: int + 1
%

category: 'operations'
method: RwSemanticVersionNumber
incrementPatchVersion
  self incrementNormalVersionAt: 3
%

category: 'accessing'
method: RwSemanticVersionNumber
normalVersion
    normalVersion ifNil: [ normalVersion := #() ].
    ^ normalVersion
%

category: 'accessing'
method: RwSemanticVersionNumber
normalVersion: anObject
	normalVersion := anObject
%

category: 'copying'
method: RwSemanticVersionNumber
postCopy
  normalVersion := normalVersion copy.
  preReleaseVersion := preReleaseVersion copy.
  buildVersion := buildVersion copy
%

category: 'accessing'
method: RwSemanticVersionNumber
preReleaseVersion
    preReleaseVersion ifNil: [ preReleaseVersion := #() ].
    ^ preReleaseVersion
%

category: 'accessing'
method: RwSemanticVersionNumber
preReleaseVersion: anObject
	preReleaseVersion := anObject
%

category: 'printing'
method: RwSemanticVersionNumber
print: components prefix: prefixChar on: aStream
    | beforeFirst |
    beforeFirst := true.
    components
        do: [ :component | 
            beforeFirst
                ifTrue: [ 
                    beforeFirst := false.
                    prefixChar ifNotNil: [ aStream nextPut: prefixChar ] ]
                ifFalse: [ aStream nextPut: $. ].
            aStream nextPutAll: component asString ]
%

category: 'printing'
method: RwSemanticVersionNumber
printOn: aStream
    self print: self normalVersion prefix: nil on: aStream.
    self print: self preReleaseVersion prefix: $- on: aStream.
    self print: self buildVersion prefix: $+ on: aStream
%

category: 'private'
method: RwSemanticVersionNumber
versionComponents
    ^ self normalVersion , self preReleaseVersion , self buildVersion
%

category: 'accessing'
method: RwSemanticVersionNumber
versionString

	| strm |
	strm := WriteStream on: String new.
	self printOn: strm.
	^strm contents
%

category: 'comparing'
method: RwSemanticVersionNumber
~> aMetacelloVersionNumber

	"if this selector is to survive it will need work ... see RwGemStoneVersionNumber ... I think that collapseZeroes will be needed (in some form) to 
		make this boy give expected results"

	aMetacelloVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aMetacelloVersionNumber and: [ self < aMetacelloVersionNumber approximateBase ]
%

! Class implementation for 'ResolutionRequest'

!		Class methods for 'ResolutionRequest'

category: 'instance creation'
classmethod: ResolutionRequest
for: origin
	^ self new
		origin: origin;
		signal
%

!		Instance methods for 'ResolutionRequest'

category: 'exceptionDescription'
method: ResolutionRequest
defaultAction
"
	| filedir ref |
	filedir := UIManager default chooseDirectory: 'Where is ', origin asString, '?'.
	ref := filedir ifNotNil: [FileSystem disk referenceTo: filedir fullName].
	self resume: ref
"

	self error: 'Not yet implemented'
%

category: 'accessing'
method: ResolutionRequest
origin: aSymbol
	origin := aSymbol
%

! Class implementation for 'AbstractFileReference'

!		Instance methods for 'AbstractFileReference'

category: 'copying'
method: AbstractFileReference
, extension
	^ self resolve, extension
%

category: 'navigating'
method: AbstractFileReference
/ aString
	"aString is either a file or path.  If aString is relative, it is appended to the receiver, if it is absolute, an instance of the receiver with the path is answered"

	^ self withPath: (self path resolvePath: (self fileSystem pathFromString: aString))
%

category: 'comparing'
method: AbstractFileReference
<= other
	^ self path <= other path
%

category: 'accessing'
method: AbstractFileReference
absolutePath
	"Returns the absolute path"
	
	^ self subclassResponsibility
%

category: 'enumerating'
method: AbstractFileReference
allChildren
	"Return all the files and folders recursively nested in the receiver"
	
	^ CollectVisitor breadthFirst: self resolve collect: [:entry | entry reference]
%

category: 'enumerating'
method: AbstractFileReference
allChildrenMatching: aPattern
    "Return all the files and folders recursively nested in the receiver and matching the pattern, aPattern"
    
    ^ self allChildren select: [ :each |  aPattern match: each basename ]
%

category: 'enumerating'
method: AbstractFileReference
allDirectories
	"Return all the directories recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isDirectory ])
		collect: [ :each| each reference ]
%

category: 'enumerating'
method: AbstractFileReference
allDirectoriesMatching: aPattern
    "Return all the directories recursively nested in the receiver."
    ^ (SelectVisitor 
			breadthFirst: self resolve 
			select: [:entry | entry isDirectory and: [ aPattern match: entry basename ] ])
        collect: [ :each | each reference ]
%

category: 'enumerating'
method: AbstractFileReference
allEntries
	^ CollectVisitor breadthFirst: self resolve
%

category: 'enumerating'
method: AbstractFileReference
allFiles
	"Return all the files (not directories) recursively nested in the receiver."
	^ (SelectVisitor breadthFirst: self resolve select: [:entry | entry isFile ])
		collect: [ :each| each reference ]
%

category: 'converting'
method: AbstractFileReference
asAbsolute 
	self subclassResponsibility
%

category: 'converting'
method: AbstractFileReference
asFileReference 
	self subclassResponsibility 
%

category: 'converting'
method: AbstractFileReference
asPath
	"Answer the receiver's path"

	self subclassResponsibility 
%

category: 'delegated'
method: AbstractFileReference
asPathWith: anObject
	^ self resolve asPathWith: anObject
%

category: 'resolving'
method: AbstractFileReference
asResolvedBy: anObject
	^ anObject resolveReference: self
%

category: 'accessing'
method: AbstractFileReference
base
	"Returns the base of the basename, i.e. /foo/gloops.taz base is 'gloops'"
	^ self fullPath base
%

category: 'accessing'
method: AbstractFileReference
basename
	"Returns the basename, i.e. /foo/gloops.taz basename is 'gloops.taz'"
	^ self fullPath basename
%

category: 'accessing'
method: AbstractFileReference
basenameWithIndicator
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is 'gloops.taz', whereras /foo basenameWithIndicator is 'foo/'" 
	^ self basename, self indicator   
%

category: 'accessing'
method: AbstractFileReference
basenameWithoutExtension
	"Returns the basename, i.e. /foo/gloops.taz basenameWithoutExtension is 'gloops'"
	^ self fullPath basenameWithoutExtension
%

category: 'accessing'
method: AbstractFileReference
basenameWithoutExtension: anExtension
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asFileReference basenameWithoutExtension: 'taz') = 'gloops'
	"
	^ self fullPath basenameWithoutExtension: anExtension
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStream
	^ self subclassResponsibility
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStreamDo: aBlock
	| stream |
	stream := self binaryReadStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStreamDo: doBlock ifAbsent: absentBlock
	^ self isFile 
		ifTrue: [ self binaryReadStreamDo: doBlock ]
		ifFalse: absentBlock
%

category: 'streams-compatibility'
method: AbstractFileReference
binaryReadStreamIfAbsent: absentBlock
	^ self isFile
		ifTrue: [ self binaryReadStream ]
		ifFalse: absentBlock
%

category: 'streams'
method: AbstractFileReference
binaryWriteStream
	"Answer a binary write stream on the receiver"

	^ self subclassResponsibility
%

category: 'streams'
method: AbstractFileReference
binaryWriteStreamDo: aBlock
	"Pass a binary write stream on the receiver to the supplied block, and ensure that the stream is closed after evaluation."

	| stream |

	stream := self binaryWriteStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
%

category: 'streams'
method: AbstractFileReference
binaryWriteStreamDo: doBlock ifPresent: presentBlock
	^ self isFile
		ifTrue: presentBlock
		ifFalse: [ self binaryWriteStreamDo: doBlock ]
%

category: 'delegated'
method: AbstractFileReference
canonicalize
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"

	^ self withPath: self resolve path canonicalize
%

category: 'private'
method: AbstractFileReference
childGeneratorBlock: doBlock matching: patterns
	"
	FileSystem workingDirectory filesMatching: '*'
	FileSystem workingDirectory filesMatching: '*.image;*.changes'
	"
	
	| files reference|
	files := Set new.
	reference := self resolve.
	
	(patterns subStrings: ';', String crlf) do: [ :pattern | 
		doBlock value: reference value: [ :basename|
			(pattern match: basename)
				ifTrue: [ files add: (self / basename) ]]].
	
	^files asOrderedCollection
%

category: 'enumerating'
method: AbstractFileReference
childNames
	^ self children collect: [:each | each basename ]
%

category: 'enumerating'
method: AbstractFileReference
children
	"Answer an array containing references to the direct children of this reference."
	| reference |
	reference := self resolve.
	^ (reference fileSystem childNamesAt: reference path) 
		collect: [ :aName | self / aName ]
%

category: 'enumerating'
method: AbstractFileReference
childrenMatching: patterns
	"
	FileSystem disk workingDirectory childrenMatching: '*'
	FileSystem disk workingDirectory childrenMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem childNamesAt: reference path do: aBlock ]
		matching:  patterns 
%

category: 'comparing'
method: AbstractFileReference
contains: anObject
	"Return true if anObject is in a subfolder of me"
	^ anObject isContainedBy: self resolve
%

category: 'comparing'
method: AbstractFileReference
containsPath: aPath
	^ self fullPath containsPath: aPath
%

category: 'accessing'
method: AbstractFileReference
contents
	self readStreamDo: [ :stream |
		^ stream contents ]
%

category: 'operations'
method: AbstractFileReference
copyAllTo: aResolvable
	"Performs a deep copy of the receiver, to a location specified by the argument. If the receiver is a file, the file will be copied; if a directory, the directory and its contents will be copied recursively. The argument must be a reference that doesn't exist; it will be created by the copy."
	
	CopyVisitor copy: self resolve asAbsolute to: aResolvable resolve
%

category: 'delegated'
method: AbstractFileReference
copyTo: aReference
	^ self resolve copyTo: aReference resolve
%

category: 'copying'
method: AbstractFileReference
copyWithPath: newPath
	self subclassResponsibility
%

category: 'enumerating'
method: AbstractFileReference
createDirectory
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist.If the parents does not exist return an exception"
	self parent exists ifFalse:[DirectoryDoesNotExist signalWith: self parent path].
	^ self resolve ensureCreateDirectory
%

category: 'enumerating'
method: AbstractFileReference
createFile
	"Create if necessary a file for the receiver. If the parent does not exist return an exception"
	self parent exists ifFalse:[DirectoryDoesNotExist signalWith: self parent path].
	self writeStream close.
	
%

category: 'accessing'
method: AbstractFileReference
creationTime
	^ self resolve creationTime 
%

category: 'operations'
method: AbstractFileReference
delete
	"Delete the receiver, does raise an error if it is not present."
	
	^ self resolve delete
%

category: 'operations'
method: AbstractFileReference
deleteAll
	"Delete this directory and all children of it, raise an error if the file does not exist."
	DeleteVisitor delete: self resolve
%

category: 'operations'
method: AbstractFileReference
deleteAllChildren
	"delete all children of the receiver, raise an error if the receiver does not exist"
	
	self children do: [:aReference | aReference deleteAll ]
%

category: 'operations'
method: AbstractFileReference
deleteIfAbsent: aBlock
	"Delete the receiver, when it does not exist evaluate the block"
	self resolve deleteIfAbsent: aBlock
%

category: 'enumerating'
method: AbstractFileReference
directories
	"Return all the directories (by opposition to files) contained in the receiver"

	| reference |
	reference := self resolve.
	^ (reference fileSystem directoryNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]
%

category: 'enumerating'
method: AbstractFileReference
directoriesMatching: patterns
	"
	FileSystem disk workingDirectory directoriesMatching: '*'
	FileSystem disk workingDirectory directoriesMatching: 'package-cache'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem directoryNamesAt: reference path do: aBlock ]
		matching:  patterns 
%

category: 'enumerating'
method: AbstractFileReference
directoryNames
	^ self directories collect: #basename
%

category: 'operations'
method: AbstractFileReference
ensureCreateDirectory
	"Verifies that the directory does not exist and only creates if necessary. Do not remove files contained if they exist.Creates the parents if needed"
	^ self resolve ensureCreateDirectory
%

category: 'operations'
method: AbstractFileReference
ensureCreateFile
	"Create if necessary a file for the receiver. If the parent does not exist creates it"
	self parent ensureCreateDirectory.
	self writeStream close.
	
%

category: 'operations'
method: AbstractFileReference
ensureDelete
	"Delete the file and does not raise exception if it does not exist contrary to delete.
	However if it is a directory and it has children an error is signaled. If it is required to 
	delete even with children, use #ensureDeleteAll."
	
	self deleteIfAbsent: [].
%

category: 'operations'
method: AbstractFileReference
ensureDeleteAll
	"Delete this directory and all children of it, and does not raise an error if the file does not exist."
	
	self exists ifFalse: [ ^ self  ].
	self deleteAll
	
%

category: 'operations'
method: AbstractFileReference
ensureDeleteAllChildren
	"delete all children of the receiver and does not raise an error if the receiver does not exist"
	
	self exists ifFalse: [ ^ self  ].
	self deleteAllChildren
%

category: 'enumerating'
method: AbstractFileReference
entries
	^ self resolve entries
%

category: 'accessing'
method: AbstractFileReference
entry
	^ self resolve entry
%

category: 'delegated'
method: AbstractFileReference
exists
	"Return true if the file reference exist (e.g., if there is a file on the hard disk pointed by the file reference)
	
	E.g., (if you are on Unix or OSX)
	'/tmp/' asFileReference exists => true
	'/tmp/zorkbar' asFileReference exists => false
	"
	^ self resolve exists
%

category: 'accessing'
method: AbstractFileReference
extension
	
	^ self fullPath extension.
%

category: 'accessing'
method: AbstractFileReference
extensions
	  "#('foo' 'foo.tar' 'foo.tar.gz' 'foo.1.tar' 'foo.1.tar.gz') collect: [:thing| thing extensions] => #(#() #('tar') #('tar' 'gz') #('1' 'tar') #('1' 'tar' 'gz'))"
	
        ^ self fullPath extensions
%

category: 'enumerating'
method: AbstractFileReference
fileNames
	^ self files collect: #basename
%

category: 'enumerating'
method: AbstractFileReference
files
	"Return all the files (as opposed to folders) contained in the receiver"
	
	| reference |
	reference := self resolve.
	^ (reference fileSystem fileNamesAt: reference path) 
		collect: [ :aName | self withPath: self path / aName ]
%

category: 'enumerating'
method: AbstractFileReference
filesMatching: patterns
	"
	FileSystem disk workingDirectory filesMatching: '*'
	FileSystem disk workingDirectory filesMatching: '*.image;*.changes'
	"
	^ self 
		childGeneratorBlock: [:reference : aBlock| 
			reference fileSystem fileNamesAt: reference path do: aBlock ]
		matching:  patterns 
%

category: 'accessing'
method: AbstractFileReference
fileSystem
	^ self resolve fileSystem
%

category: 'delegated'
method: AbstractFileReference
fullName

	^ self resolve fullName
%

category: 'accessing'
method: AbstractFileReference
fullNameWithIndicator
	"Returns the basename with the indicator appended, i.e. /foo/gloops.taz basenameWithIndicator is '/foo/gloops.taz', whereras /foo basenameWithIndicator is '/foo/'" 
	^ self fullName, self indicator   
%

category: 'accessing'
method: AbstractFileReference
fullPath
	"Returns the absolute path, better use absolutePath"
	
	^ self subclassResponsibility
%

category: 'enumerating'
method: AbstractFileReference
glob: aBlock
	^ SelectVisitor breadthFirst: self resolve select: aBlock
%

category: 'testing'
method: AbstractFileReference
hasChildren
	^self resolve hasChildren
%

category: 'testing'
method: AbstractFileReference
hasDirectories
	^self resolve hasDirectories
%

category: 'testing'
method: AbstractFileReference
hasFiles
	^self resolve hasFiles
%

category: 'comparing'
method: AbstractFileReference
hash
	"Hash is reimplemented because #= is reimplemented"
	^ self path  hash
%

category: 'accessing'
method: AbstractFileReference
humanReadableSize
	^ self size humanReadableSIByteSize
%

category: 'testing'
method: AbstractFileReference
ifFile: fBlock ifDirectory: dBlock ifAbsent: aBlock
	^ self isFile
		ifTrue: fBlock
		ifFalse: [
			self isDirectory
				ifTrue: dBlock
				ifFalse: aBlock ]
%

category: 'printing'
method: AbstractFileReference
indicator
	"Returns a string indicating the type of reference:
	- '?' for a non existing reference',
	- '/' for a directory,
	- the empty string for a file."
	"When this framework gets more complete, it is possible to extend this behavior with the following indicators (taken from unix ls utility):
	- '*' for a regular file that is executable
	- '@' for a symbolic link
	- '|' for FIFOs
	- '=' for sockets
	- '>' for doors"
	^ self exists
		ifTrue: [self isDirectory ifTrue: ['/'] ifFalse: ['']  ]
		ifFalse: ['?']
%

category: 'initialization'
method: AbstractFileReference
initialize
%

category: 'testing'
method: AbstractFileReference
isAbsolute 
	self subclassResponsibility 
%

category: 'testing'
method: AbstractFileReference
isChildOf: anObject
	^ self parent = anObject
%

category: 'comparing'
method: AbstractFileReference
isContainedBy: anObject
	"DoubleDispatch helper for #contains:"
	^ anObject containsReference: self resolve
%

category: 'testing'
method: AbstractFileReference
isDirectory
	^ self resolve isDirectory
%

category: 'testing'
method: AbstractFileReference
isFile
	^ self resolve isFile
%

category: 'testing'
method: AbstractFileReference
isReadable
	^ self resolve isReadable
%

category: 'testing'
method: AbstractFileReference
isRelative 
	self subclassResponsibility 
%

category: 'testing'
method: AbstractFileReference
isRoot
	^ self resolve isRoot
%

category: 'testing'
method: AbstractFileReference
isWritable
	^ self resolve isWritable
%

category: 'accessing'
method: AbstractFileReference
item
	^ self
%

category: 'navigating'
method: AbstractFileReference
makeRelative: anObject
	^ anObject relativeToReference: self resolve
%

category: 'accessing'
method: AbstractFileReference
modificationTime
	^ self resolve modificationTime 
%

category: 'operations'
method: AbstractFileReference
moveTo: aReference
	"Move the receiver in the location passed as argument.
	
	(FileSystem disk workingDirectory / 'paf' ) ensureCreateFile.
	(FileSystem disk workingDirectory / 'fooFolder') ensureCreateDirectory. 
	(FileSystem disk workingDirectory / 'paf' ) moveTo: (FileSystem disk workingDirectory / 'fooFolder' / 'paf')
	"
	^ self resolve moveTo: aReference
%

category: 'utilities'
method: AbstractFileReference
nextVersion
	^ self resolve nextVersion
%

category: 'streams'
method: AbstractFileReference
openWritable: aBoolean
	^ self resolve openWritable: aBoolean
%

category: 'delegated'
method: AbstractFileReference
parent
	^ self withPath: self resolve path parent
%

category: 'delegated'
method: AbstractFileReference
parentUpTo: aParentDirName
	^ self withPath: (self path parentUpTo: aParentDirName)
%

category: 'accessing'
method: AbstractFileReference
pathSegments
	^ self fullPath segments
%

category: 'delegated'
method: AbstractFileReference
pathString
	^ self resolve pathString
%

category: 'accessing'
method: AbstractFileReference
permissions
	"Return the FileSystemPermission for this node"
	^ self resolve permissions
%

category: 'streams'
method: AbstractFileReference
readStream
	self subclassResponsibility
%

category: 'streams'
method: AbstractFileReference
readStreamDo: aBlock
	| stream |
	stream := self readStream.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
%

category: 'streams'
method: AbstractFileReference
readStreamDo: doBlock ifAbsent: absentBlock
	^ self isFile 
		ifTrue: [ self readStreamDo: doBlock ]
		ifFalse: absentBlock
%

category: 'streams'
method: AbstractFileReference
readStreamEncoded: anEncoding

	^ ZnCharacterReadStream
		on: self binaryReadStream
		encoding: anEncoding
%

category: 'streams'
method: AbstractFileReference
readStreamEncoded: anEncoding do: aBlock
	| stream |
	stream := self readStreamEncoded: anEncoding.
	^ [ aBlock value: stream ] 
		ensure: [ stream close ]
%

category: 'streams'
method: AbstractFileReference
readStreamIfAbsent: absentBlock
	^ self isFile
		ifTrue: [ self readStream ]
		ifFalse: absentBlock
%

category: 'navigating'
method: AbstractFileReference
relativeTo: landmark
	"Answer a new path relative to landmark."
	
	"parent/child/grandChild relativeTo: parent returns child/grandChild
	(Filesystem disk / 'griffle' / 'plonk' / 'nurp') relativeTo: (Filesystem disk / 'griffle') 
	returns plonk/nurp"

	^ landmark makeRelative: self resolve
%

category: 'navigating'
method: AbstractFileReference
relativeToPath: landmarkPath 
	
	^ self fullPath relativeTo: landmarkPath
%

category: 'navigating'
method: AbstractFileReference
relativeToReference: landmarkReference
	"Return the path of the receiver relative to landmarkReference."
	
	^ self fullPath relativeTo: landmarkReference path
%

category: 'operations'
method: AbstractFileReference
renameTo: newBasename
	self subclassResponsibility
%

category: 'navigating'
method: AbstractFileReference
resolve
	^ self subclassResponsibility 
%

category: 'navigating'
method: AbstractFileReference
resolve: anObject
	^ anObject asResolvedBy: self
%

category: 'navigating'
method: AbstractFileReference
resolvePath: aPath
	^ self withPath: (self path resolvePath: aPath)
%

category: 'navigating'
method: AbstractFileReference
resolveReference: aReference
	^ aReference isAbsolute
		ifTrue: [ aReference ]
		ifFalse: [ self withPath: aReference path ]
%

category: 'navigating'
method: AbstractFileReference
resolveString: aString 
	self subclassResponsibility
%

category: 'accessing'
method: AbstractFileReference
size
	"Return the size of the file in bytes."
	
	^ self resolve size
%

category: 'streams'
method: AbstractFileReference
streamWritable: writable do: aBlock
	^ writable 
		ifTrue: [ self writeStreamDo: aBlock ]
		ifFalse: [ self readStreamDo: aBlock ]
%

category: 'accessing'
method: AbstractFileReference
uri
	"Convert my path into a file:// type url. For odd characters use %20 notation."

	^ self asUrl
%

category: 'navigating'
method: AbstractFileReference
withExtension: aString
	^ self withPath: (self path withExtension: aString)
%

category: 'copying'
method: AbstractFileReference
withoutExtension
  "Returns the new reference based on receiver with fullname without its extension"

  ^  (self parent / self basenameWithoutExtension)
%

category: 'navigating'
method: AbstractFileReference
withPath: newPath
	^ self path == newPath
		ifTrue: [ self ]
		ifFalse: [ self copyWithPath: newPath ]
%

category: 'streams'
method: AbstractFileReference
writeStream
	self subclassResponsibility
%

category: 'streams'
method: AbstractFileReference
writeStreamDo: aBlock
	| stream |
	stream := self writeStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
%

category: 'streams'
method: AbstractFileReference
writeStreamDo: doBlock ifPresent: presentBlock
	^ self isFile
		ifTrue: presentBlock
		ifFalse: [ self writeStreamDo: doBlock ]
%

category: 'streams'
method: AbstractFileReference
writeStreamEncoded: anEncoding

	^ ZnCharacterWriteStream
		on: self binaryWriteStream
		encoding: anEncoding
%

category: 'streams'
method: AbstractFileReference
writeStreamEncoded: anEncoding do: aBlock
	| stream |
	stream := self writeStreamEncoded: anEncoding.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
%

category: 'streams'
method: AbstractFileReference
writeStreamIfPresent: presentBlock
	^ self isFile 
		ifTrue: presentBlock
		ifFalse: [ self writeStream ]
%

! Class implementation for 'FileLocator'

!		Class methods for 'FileLocator'

category: 'windows-origins'
classmethod: FileLocator
A
	^ self driveNamed: #A 
%

category: 'adding'
classmethod: FileLocator
addResolver: aResolver
	self resolver addResolver: aResolver
%

category: 'windows-origins'
classmethod: FileLocator
B
	^ self driveNamed: #B
%

category: 'windows-origins'
classmethod: FileLocator
C
	^ self driveNamed: #C
%

category: 'origins'
classmethod: FileLocator
cache
	^ self origin: #cache
%

category: 'gemstone-origins'
classmethod: FileLocator
clientWorkingDirectory
	^ FileSystem client referenceTo: RelativePath new
%

category: 'origins'
classmethod: FileLocator
cwd
	^ self workingDirectory
%

category: 'windows-origins'
classmethod: FileLocator
D
	^ self driveNamed: #D
%

category: 'gemstone-origins'
classmethod: FileLocator
dbfScratchDir
	"Answer the path to the DBF_SCRATCH_DIR"

	^ self origin: #dbfScratchDir
%

category: 'origins'
classmethod: FileLocator
desktop
	^ self origin: #desktop
%

category: 'origins'
classmethod: FileLocator
documents
	^ self origin: #documents
%

category: 'windows-origins'
classmethod: FileLocator
driveNamed: driveName
	^ FileReference fileSystem: (FileSystem disk) path: Path / (driveName, ':')
%

category: 'windows-origins'
classmethod: FileLocator
E
	^ self driveNamed: #E
%

category: 'gemstone-origins'
classmethod: FileLocator
extent1
	"Answer the path to the first exent extent"

	^ self origin: #extent1
%

category: 'gemstone-origins'
classmethod: FileLocator
extent1Directory

	^ self origin: #extent1Directory
%

category: 'gemstone-origins'
classmethod: FileLocator
extent: extentIndex
	"Answer the path to the extent at extentIndex in DBF_EXTENT_NAMES: (System stoneConfigurationReport at: #DBF_EXTENT_NAMES) at: extentIndex"

	^ self origin: ('extent', extentIndex printString) asSymbol
%

category: 'windows-origins'
classmethod: FileLocator
F
	^ self driveNamed: #F
%

category: 'flushing'
classmethod: FileLocator
flushCaches
	self resolver flushCaches
%

category: 'windows-origins'
classmethod: FileLocator
G
	^ self driveNamed: #G
%

category: 'gemstone-origins'
classmethod: FileLocator
gemLogDirectory
	"locator for the log directory (parent directory of gemLog)"

	^ self origin: #gemLogDirectory
%

category: 'windows-origins'
classmethod: FileLocator
H
	^ self driveNamed: #H
%

category: 'origins'
classmethod: FileLocator
home
	^ self origin: #home
%

category: 'windows-origins'
classmethod: FileLocator
I
	^ self driveNamed: #I
%

category: 'gemstone-origins'
classmethod: FileLocator
image
	"commonly used, so we need to define it, but the closest that GemStone comes is the #dbfScratchDir"

	^ self dbfScratchDir
%

category: 'gemstone-origins'
classmethod: FileLocator
imageDirectory
	"commonly used, so we need to define it, but the closest that GemStone comes is the #dbfScratchDir"

	^ self dbfScratchDir
%

category: 'windows-origins'
classmethod: FileLocator
J
	^ self driveNamed: #J
%

category: 'windows-origins'
classmethod: FileLocator
K
	^ self driveNamed: #K
%

category: 'windows-origins'
classmethod: FileLocator
L
	^ self driveNamed: #L
%

category: 'windows-origins'
classmethod: FileLocator
M
	^ self driveNamed: #M
%

category: 'windows-origins'
classmethod: FileLocator
N
	^ self driveNamed: #N
%

category: 'windows-origins'
classmethod: FileLocator
O
	^ self driveNamed: #O
%

category: 'instance creation'
classmethod: FileLocator
origin: aSymbol 
	^ self origin: aSymbol path: Path workingDirectory
%

category: 'instance creation'
classmethod: FileLocator
origin: aSymbol path: aPath
	^ self basicNew 
			initializeWithOrigin: aSymbol path: aPath
%

category: 'windows-origins'
classmethod: FileLocator
P
	^ self driveNamed: #P
%

category: 'origins'
classmethod: FileLocator
preferences
	^ self origin: #preferences
%

category: 'windows-origins'
classmethod: FileLocator
Q
	^ self driveNamed: #Q
%

category: 'windows-origins'
classmethod: FileLocator
R
	^ self driveNamed: #R
%

category: 'accessing'
classmethod: FileLocator
resolver

	^ SessionTemps current at: #FileLocator_Resolver ifAbsentPut: [
		InteractiveResolver new
			addResolver: SystemResolver new;
			addResolver: PlatformResolver forCurrentPlatform;
			yourself ]
%

category: 'origins'
classmethod: FileLocator
root
	^ FileSystem disk root
%

category: 'windows-origins'
classmethod: FileLocator
S
	^ self driveNamed: #S
%

category: 'gemstone-origins'
classmethod: FileLocator
serverWorkingDirectory
	^ FileSystem server referenceTo: RelativePath new
%

category: 'accessing'
classmethod: FileLocator
supportedOrigins
	| origins current |
	origins := IdentitySet new.
	current := self resolver.
	[current notNil] whileTrue:
		[origins addAll: current supportedOrigins.
		current := current next].
	^ origins
%

category: 'mac-origins'
classmethod: FileLocator
systemApplicationSupport
	^ self origin: #systemApplicationSupport
%

category: 'mac-origins'
classmethod: FileLocator
systemLibrary
	^ self origin: #systemLibrary
%

category: 'windows-origins'
classmethod: FileLocator
T
	^ self driveNamed: #T
%

category: 'origins'
classmethod: FileLocator
temp
	^ self origin: #temp
%

category: 'gemstone-origins'
classmethod: FileLocator
tranlog
	"Answer the path to the current tranlog directory"

	^ self origin: #tranlog
%

category: 'windows-origins'
classmethod: FileLocator
U
	^ self driveNamed: #U
%

category: 'mac-origins'
classmethod: FileLocator
userApplicationSupport
	^ self origin: #userApplicationSupport
%

category: 'unix-origins'
classmethod: FileLocator
userData
	^ self origin: #userData
%

category: 'mac-origins'
classmethod: FileLocator
userLibrary
	^ self origin: #userLibrary
%

category: 'windows-origins'
classmethod: FileLocator
V
	^ self driveNamed: #V
%

category: 'windows-origins'
classmethod: FileLocator
W
	^ self driveNamed: #W
%

category: 'origins'
classmethod: FileLocator
workingDirectory
	^ FileSystem disk referenceTo: RelativePath new
%

category: 'windows-origins'
classmethod: FileLocator
X
	^ self driveNamed: #X
%

category: 'windows-origins'
classmethod: FileLocator
Y
	^ self driveNamed: #Y
%

category: 'windows-origins'
classmethod: FileLocator
Z
	^ self driveNamed: #Z
%

!		Instance methods for 'FileLocator'

category: 'comparing'
method: FileLocator
= other
	^ self species = other species
		and: [origin = other origin
			and: [path = other path]]
%

category: 'accessing'
method: FileLocator
absolutePath
	"Return the absolute path"
	^ self resolve path
%

category: 'converting'
method: FileLocator
asAbsolute
	^ self 
%

category: 'converting'
method: FileLocator
asFileReference
	^ self resolve
%

category: 'converting'
method: FileLocator
asPath
	"Answer the receiver's path"

	^self resolve asPath
%

category: 'streams-compatibility'
method: FileLocator
binaryReadStream
	^ self resolve binaryReadStream
%

category: 'streams-compatibility'
method: FileLocator
binaryWriteStream
	"Answer a binary write stream on the receiver"

	^ self resolve binaryWriteStream
%

category: 'copying'
method: FileLocator
copyWithPath: newPath
	^ self class origin: origin path: newPath
%

category: 'error handling'
method: FileLocator
doesNotUnderstand: aMessage
	"Redirect message to the resolved version of this FileLocator.
	If FileReference won't understand the message send a normal DNU."

	| resolved |
	
	resolved := self resolve.
	(resolved respondsTo: aMessage selector)
		ifTrue: [ ^ resolved perform: aMessage selector withArguments: aMessage arguments ].
	
	^ super doesNotUnderstand: aMessage.
%

category: 'accessing'
method: FileLocator
fullPath
	^ self resolve path
%

category: 'comparing'
method: FileLocator
hash
	^ origin hash bitXor: path hash
%

category: 'initialize-release'
method: FileLocator
initializeWithOrigin: aSymbol path: aPath
	self initialize.
	origin := aSymbol.
	path := aPath.
%

category: 'testing'
method: FileLocator
isAbsolute
	^ true
%

category: 'testing'
method: FileLocator
isRelative
	^ false
%

category: 'accessing'
method: FileLocator
origin
	^ origin
%

category: 'accessing'
method: FileLocator
path
	^ path
%

category: 'printing'
method: FileLocator
printOn: aStream
	| fs |
	aStream
		nextPut: ${;
		nextPutAll: origin;
		nextPut: $}.
	path isWorkingDirectory
		ifTrue: [ ^ self ].
	fs := self fileSystem.
	aStream nextPut: fs delimiter.
	fs printPath: path on: aStream
%

category: 'streams'
method: FileLocator
readStream
	^ self resolve readStream 
%

category: 'operations'
method: FileLocator
renameTo: newBasename
	
	| result |
	result := self resolve renameTo: newBasename.
	path size > 0
		ifTrue: [ path basename: newBasename ]
		ifFalse: [ path := result path ]
%

category: 'navigating'
method: FileLocator
resolve
	^ (self class resolver resolve: origin) resolve: path
%

category: 'navigating'
method: FileLocator
resolveString: aString
	| filesystem thePath |
	filesystem := (self class resolver resolve: origin) fileSystem.
	thePath := filesystem pathFromString: aString.
	^ self withPath: (path resolvePath: thePath)
%

category: 'streams'
method: FileLocator
writeStream
	^ self resolve writeStream 
%

! Class implementation for 'FileReference'

!		Class methods for 'FileReference'

category: 'cross platform'
classmethod: FileReference
/ aString
	"Answer a reference to the argument resolved against the root of the current disk filesystem."
	
	^ FileSystem disk / aString
%

category: 'instance creation'
classmethod: FileReference
fileSystem: aFilesystem path: aPath
	^ self new setFileSystem: aFilesystem path: aPath
%

category: 'instance creation'
classmethod: FileReference
newTempFilePrefix: prefix suffix: suffix
	| tmpDir random fileName |
	
	tmpDir := FileLocator temp asFileReference.
	[ 
		random := UUID new asInteger asString.
		fileName := prefix , random , suffix.
		(tmpDir / fileName) exists ] whileTrue.
	^ tmpDir / fileName
%

!		Instance methods for 'FileReference'

category: 'navigating'
method: FileReference
, extension
	^ self withPath: self path, extension
%

category: 'comparing'
method: FileReference
= other
	"Two FileReferences are considered equal if they refer to the same file / directory.
	As paths can have multiple relative representations, compare the absolute paths."
	"Perform the path comparison last as conversion to absolute paths is relatively expensive"
	^ self species = other species
		and: [self fileSystem = other fileSystem
			and: [self absolutePath = other absolutePath]]
%

category: 'accessing'
method: FileReference
absolutePath
	"Return the absolute of the receiver"
	^ self path isRelative
		ifFalse: [ self path ]
		ifTrue: [ filesystem resolve: self path ]
%

category: 'converting'
method: FileReference
asAbsolute
	"Return the receiver as an absolute file reference."
	
	^ self isAbsolute
		ifTrue: [ self ]
		ifFalse: [ filesystem referenceTo: (filesystem resolve: path) ]
%

category: 'converting'
method: FileReference
asFileReference
	^ self
%

category: 'converting'
method: FileReference
asPath
	"Answer the receivers path"

	^path
%

category: 'printing'
method: FileReference
asString
  "needed for informative topaz stack display"
  ^ path asString 
%

category: 'streams'
method: FileReference
binaryReadStream
	
	^ filesystem binaryReadStreamOn: self path
%

category: 'streams'
method: FileReference
binaryWriteStream
	"Answer a binary write stream on the receiver"

	^ ZnBufferedWriteStream on: (filesystem binaryWriteStreamOn: self path)
%

category: 'comparing'
method: FileReference
containsReference: aReference
	^  aReference fileSystem = filesystem and: [path contains: aReference path]
%

category: 'operations'
method: FileReference
copyTo: aReference
	self isDirectory
		ifTrue: [ aReference ensureCreateDirectory ]
		ifFalse: [ filesystem copy: path toReference: aReference ]
%

category: 'copying'
method: FileReference
copyWithPath: newPath
	^ filesystem referenceTo: newPath
%

category: 'accessing'
method: FileReference
creationTime 
	^ filesystem creationTimeOf: self path
%

category: 'operations'
method: FileReference
delete
	"Deletes the referenced file or directory. If the directory is not empty, 
	raises an error. Use #deleteAll to delete with the children."

	(self isDirectory and:[self hasChildren]) 
		ifTrue:[DirectoryIsNotEmpty signalWith: self].
	filesystem delete: path
%

category: 'operations'
method: FileReference
deleteIfAbsent: aBlock
	self exists 
		ifTrue: [ self delete ]
		ifFalse: aBlock
%

category: 'operations'
method: FileReference
ensureCreateDirectory
	"Create if necessary a directory for the receiver."
	filesystem ensureCreateDirectory: path
	
%

category: 'navigating'
method: FileReference
entries
	"Return the entries (meta data - file description) of the direct children of the receiver"
	^ self fileSystem entriesAt: self path 
%

category: 'accessing'
method: FileReference
entry
	"Return the entry (meta data) describing the receiver."
	^ filesystem entryAt: path
%

category: 'testing'
method: FileReference
exists
	^ filesystem exists: path
%

category: 'accessing'
method: FileReference
fileSystem
	"Return the filesystem to which the receiver belong."
	^ filesystem
%

category: 'accessing'
method: FileReference
fullName
	"Return the full path name of the receiver."
	^ filesystem stringFromPath: (filesystem resolve: path)
%

category: 'accessing'
method: FileReference
fullPath
	^ self path
%

category: 'testing'
method: FileReference
hasChildren
	"Return whether the receiver has any children."
	"FileSystem workingDirectory hasChildren"
	
	^ filesystem hasChildren: path
%

category: 'testing'
method: FileReference
hasDirectories
	"Return whether the receiver has children that are directories."
	"FileSystem workingDirectory hasDirectories"
	
	^ filesystem hasDirectories: path
%

category: 'testing'
method: FileReference
hasFiles
	"Return whether the receiver has children that are files."
	"FileSystem workingDirectory hasFiles"
	
	^ filesystem hasFiles: path
%

category: 'comparing'
method: FileReference
hash
	^ path hash bitXor: filesystem hash
%

category: 'testing'
method: FileReference
isAbsolute
	^ path isAbsolute
%

category: 'testing'
method: FileReference
isDirectory
	^ filesystem isDirectory: path
%

category: 'testing'
method: FileReference
isFile
	^ filesystem isFile: path
%

category: 'testing'
method: FileReference
isReadable
	^ filesystem isReadable: path
%

category: 'testing'
method: FileReference
isRelative
	^ path isRelative
%

category: 'testing'
method: FileReference
isRoot
	^ path isRoot
%

category: 'testing'
method: FileReference
isSymlink 
	^ filesystem isSymlink: path
%

category: 'testing'
method: FileReference
isWritable
	^ filesystem isWritable: path
%

category: 'utilities'
method: FileReference
lastFileFor: baseFileName extension: extension
  "Assumes a file is named using a version number encoded as '.' followed by digits
  preceding the file extension, e.g., games.22.ston
  Answer the file name with the largest number.
  If a version number is not found, raises an error"

  "FileSystem workingDirectory lastFileFor: 'games' extension: 'ston'"

    | files |
    files := self childrenMatching: baseFileName , '.*.' , extension.
    files ifEmpty: [ ^ self error: 'No file with number pattern' ].
    ^ (files asSortedCollection: [ :a :b | a basename < b basename ]) last
%

category: 'accessing'
method: FileReference
modificationTime 
	"Returns the last date of modification of self"
	^ filesystem modificationTimeOf: self path
%

category: 'operations'
method: FileReference
moveTo: aReference
	
	| result |
	result := self fileSystem 
		move: self path
		to: aReference resolve.
	result ifNotNil: [
		self setFileSystem: result fileSystem path: result path ].		
%

category: 'utilities'
method: FileReference
nextNameFor: baseFileName extension: extension
  "Assumes a file name includes a version number encoded as '.' followed by digits 
   preceding the file extension, e.g., games.22.ston
   Increment the version number (of the largest one) and answer the new file name, e.g., games23.ston
   If a version number is not found, set the version to 1 and answer a new file name"

	"(FileSystem workingDirectory nextNameFor: 'games' extension: 'ston') asFileReference ensureCreateFile"

	| files splits version |
	files := self childrenMatching: baseFileName , '.*.' , extension.
	files ifEmpty: [ ^ baseFileName , '.1.' , extension ].
	splits := files
		collect: [ :filename | filename basename splitOn: $. ]
		thenSelect: [ :split | (split at: 1) = baseFileName and: [ split size = 3 ] ].
	splits := splits asSortedCollection: [ :a :b | (a at: 2) asNumber < (b at: 2) asNumber ].
	version := splits isEmpty
		ifTrue: [ 1 ]
		ifFalse: [ (splits last at: 2) asNumber + 1 ].
	^ baseFileName , '.' , version asString , '.' , extension
%

category: 'utilities'
method: FileReference
nextVersion
	"Assumes a file (or folder) name includes a version number encoded as '.' followed by digits 
	preceding the file extension.  Increment the version number and answer the new file name.
	If a version number is not found, return just the file"

	| parent version versionNumbers nameWithoutExtension |
	
	self exists
		ifFalse: [ ^ self ].
	
	parent := self parent.
	nameWithoutExtension := self basename copyUpTo: $..
	
	versionNumbers := parent children 
				select: [ :f| 
					(f basename beginsWith: nameWithoutExtension) ]
				thenCollect: [ :f| 
					Number squeezeNumberOutOfString: (f basename copyLast: (f basename size - nameWithoutExtension size))   ifFail: [ 0 ]].
	
	versionNumbers ifEmpty: [ ^self ].
	
	version := versionNumbers max + 1.
	^ parent / (nameWithoutExtension , '.', version asString) , self extension
%

category: 'streams'
method: FileReference
openWritable: aBoolean 
	^ filesystem open: path writable: aBoolean
%

category: 'accessing'
method: FileReference
path
	"Return the path internal representation that denotes the receiver in the context of its filesystem. "
	^ path
%

category: 'printing'
method: FileReference
pathString
	"Return the full path name of the receiver."
	
	^ filesystem stringFromPath: (filesystem resolve: path)
%

category: 'accessing'
method: FileReference
permissions
	^ filesystem permissions: self path
%

category: 'printing'
method: FileReference
printOn: aStream
	filesystem forReferencePrintOn: aStream.
	filesystem printPath: path on: aStream
%

category: 'streams'
method: FileReference
readStream

	^ self readStreamEncoded: 'utf8'
%

category: 'operations'
method: FileReference
renameTo: newBasename
	
	| destinationPath |
	destinationPath := self fileSystem 
		rename: self 
		to: self parent / newBasename.
	
	destinationPath ifNotNil: [
		self 
			setFileSystem: filesystem 
			path: destinationPath ].
	^ self
		
%

category: 'accessing'
method: FileReference
resolve
	^ self
%

category: 'resolving'
method: FileReference
resolvePath: anObject
	^ self withPath: (path resolve: anObject)
%

category: 'resolving'
method: FileReference
resolveReference: aReference
	
	^ (filesystem = aReference fileSystem or: [aReference isRelative])
		ifTrue: [filesystem referenceTo: (path resolvePath: aReference path)]
		ifFalse: [aReference]
%

category: 'resolving'
method: FileReference
resolveString: aString 
	| thePath |
	thePath := filesystem pathFromString: aString.
	^ filesystem referenceTo: (path resolve: thePath)
%

category: 'initialize-release'
method: FileReference
setFileSystem: aFilesystem path: aPath
	filesystem := aFilesystem.
	path := aPath
%

category: 'accessing'
method: FileReference
size
	^ filesystem sizeOf: path
%

category: 'streams'
method: FileReference
writeStream
	
	^ self writeStreamEncoded: 'utf8'
%

! Class implementation for 'FastUUIDGenerator'

!		Class methods for 'FastUUIDGenerator'

category: 'accessing'
classmethod: FastUUIDGenerator
default
	^(SessionTemps current at: self _defaultKey ifAbsent: [])
		ifNil: [
			| default |
			default := self new initialize.
      SessionTemps current at: self _defaultKey put: default ]
%

category: 'initialization'
classmethod: FastUUIDGenerator
initialize

	SessionTemps current removeKey: self _defaultKey ifAbsent: []
%

category: 'accessing'
classmethod: FastUUIDGenerator
next
	"Return a UUID using the shared generator"
	
	"self next"
	
	"[ UUIDGenerator next ] bench"
	
	^ self default next
%

category: 'private'
classmethod: FastUUIDGenerator
_defaultKey

	^ #'UUID_DEFAULT'
%

!		Instance methods for 'FastUUIDGenerator'

category: 'instance creation'
method: FastUUIDGenerator
generateBytes: aPlaceHolder forVersion: aVersion
	aVersion = 4 ifTrue: [self generateFieldsVersion4]
		ifFalse: [self error: 'Unsupported version'].
	self placeFields: aPlaceHolder.
%

category: 'instance creation'
method: FastUUIDGenerator
generateFieldsVersion4

	semaphoreForGenerator critical: 
			[bits1 := random integer.
			bits2 := random integer.
			bits3 := random integer.
			bits4 := random integer].
	bits2 := (bits2 bitAnd: 16rFFFF0FFF) bitOr: 16r4000.
	bits3 := (bits3 bitAnd: 16r3FFFFFFF) bitOr: 16r80000000
%

category: 'instance creation'
method: FastUUIDGenerator
initialize

	random := Random new.
	semaphoreForGenerator := Semaphore forMutualExclusion
%

category: 'accessing'
method: FastUUIDGenerator
next

	"Return the next UUID"
	
	| uuid |
	uuid := UUID nilUUID.
	self generateBytes: uuid forVersion: 4.
	^ uuid
%

category: 'instance creation'
method: FastUUIDGenerator
placeFields: aPlaceHolder

	aPlaceHolder
		unsigned32At: 1 put: bits1;
		unsigned32At: 5 put: bits2;
		unsigned32At: 9 put: bits3;
		unsigned32At: 13 put: bits4
%

! Class implementation for 'File'

!		Class methods for 'File'

category: 'primitives-file'
classmethod: File
atEnd: aGsFile
	"Answer true if the file position is at the end of the file."

	^ aGsFile atEnd
%

category: 'primitives-file'
classmethod: File
close: aGsFile
	"Close this file."

	aGsFile ifNil: [ ^ self ].
	aGsFile close
%

category: 'primitives-file'
classmethod: File
closed: aGsFile
	^ aGsFile isNil or: [ (self sizeOrNil: aGsFile) isNil ]
%

category: 'primitives-file'
classmethod: File
connectToFile: filePointer writable: writableFlag
	"Open the file with the supplied FILE* pointer, and return the file ID obtained.
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened.
	It is the responsibility of the caller to coordinate closing the file."

"
	<primitive: 'primitiveConnectToFile' module: 'FilePlugin' error: error>
	error = #'bad argument' ifTrue: [ 
		(filePointer isKindOf: ByteArray) ifFalse: 
			[ ^self error: 'filePointer must be a ByteArray' ].
		(writableFlag isKindOf: Boolean) ifFalse: 
			[ ^self error: 'writableFlag must be a boolean' ] ].
	^ self primitiveFailed 
"
self error: 'not yet implmented'
%

category: 'primitives-file'
classmethod: File
connectToFileDescriptor: fileDescriptor writable: writableFlag
	"Connect to the file with fileDescriptor number, and return the file ID obtained.
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened.
	It is the responsibility of the caller to coordinate closing the file."

"
	<primitive: 'primitiveConnectToFileDescriptor' module: 'FilePlugin' error: error>
	error = #'bad argument' ifTrue: [ 
		fileDescriptor isInteger ifFalse: 
			[ ^self error: 'fileDescriptor must be an integer' ].
		(writableFlag isKindOf: Boolean) ifFalse: 
			[ ^self error: 'writableFlag must be a boolean' ] ].
	^ self primitiveFailed 
"
self error: 'not yet implmented'

%

category: 'primitives-path'
classmethod: File
createDirectory: fullPath
	"Create a directory named by the given path. 
	Fail if the path is bad or if a file or directory by that name already exists."

	^ GsFile createServerDirectory: fullPath
%

category: 'primitives-path'
classmethod: File
deleteDirectory: fullPath
	"Delete the directory named by the given path. 
	Fail if the path is bad or if a directory by that name does not exist."

	(GsFile removeServerDirectory: fullPath)
		ifNil: [ self error: 'Error deleting directory ', fullPath printString, ' :: ', GsFile serverErrorString ]
%

category: 'primitives-path'
classmethod: File
deleteFile: aFileName
	"Delete the file of the given name. 
	Return self if the primitive succeeds, nil otherwise."

	^ GsFile _removeFile: aFileName onClient: false
%

category: 'primitives-path'
classmethod: File
delimiter
	"Return the path delimiter for the underlying platform's file system."

	"Was a primitive ... only needed to resolve whether or not we're on Windows"

	^ $/
%

category: 'primitives-file'
classmethod: File
fileDescriptorIsAvailable: anInteger
	"Answer a boolean indicating whether the supplied file descriptor (anInteger) is available.
	A file descriptor is considered available if it is connected to a console / terminal, pipe or a file.  cygwin terminals are currently not supported (the pipe is created, but nothing appears in the terminal)"

	^ (self fileDescriptorType: anInteger) between: 1 and: 3
%

category: 'primitives-file'
classmethod: File
fileDescriptorType: fdNum
	"Allow to test if the standard input/output files are from a console or not
	Return values:
	* -1 - Error
	* 0 - no console (windows only)
	* 1 - normal terminal (unix terminal / windows console)
	* 2 - pipe
	* 3 - file
	* 4 - cygwin terminal (windows only)"

"
	<primitive: 'primitiveFileDescriptorType' module: 'FilePlugin' error: error>
	error = #'bad argument'
		ifTrue: [ fdNum isInteger
				ifFalse: [ ^ self error: 'File Descriptor must be an integer!' ] ].
	^ self primitiveFailed
"
self error: 'not yet implmented'
%

category: 'primitives-file'
classmethod: File
flush: aGsFile
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their
	own internal buffer to minimize write() syscalls. This flushes that buffer.
	On Windows this and primSync: do the same thing."

	aGsFile flush
%

category: 'primitives-path'
classmethod: File
getMacFile: fileName type: typeString creator: creatorString
	"Get the Macintosh file type and creator info for the file with the given name. Fails if the file does not exist or if the type and creator type arguments are not strings of length 4. This primitive is Mac specific; it is a noop on other platforms."

"
 	<primitive: 'primitiveDirectoryGetMacTypeAndCreator' module: 'FilePlugin'>
"
self error: 'not yet implmented'

%

category: 'primitives-file'
classmethod: File
getPosition: aGsFile
	"Get this files current position."

	^ aGsFile position
%

category: 'primitives-path'
classmethod: File
lookupDirectory: fullPath filename: fileName

	"Look up <fileName> (a simple file name) in the directory identified by <fullPath> and 
   return entry array or nil "

	^ self lookupPath: fullPath, '/', fileName
%

category: 'primitives-path'
classmethod: File
lookupPath: fullPath

	"Look up the file or directory identified by <fullPath> and return an array containing:

	<fileName> 			
	<creationTime> 			(in seconds since the start of the Linux time epoch)
	<modificationTime> 	(in seconds since the start of the Linux time epoch)
	<dirFlag> 				DirFlag is true if the entry is a directory
	<fileSize> 				FileSize the file size in bytes or zero for directories
	<posixPermissions> 	Numeric Notation 
	<symLinkFlag>			seemingly, symLinkFlag is true if the entry is a symLink

	On Unix, the empty path denotes '/'. 
   On Macs and PCs, it is the container of the system volumes."

	| gsFileStat |
	(GsFile existsOnServer: fullPath) ifFalse: [^ nil ].
	gsFileStat := GsFile stat: fullPath isLstat: true.
	gsFileStat _isSmallInteger ifTrue: [ ^ nil ].
	^	{
		(Path from: fullPath) basename.
		gsFileStat ctimeUtcSeconds .
		gsFileStat mtimeUtcSeconds .
		gsFileStat isDirectory .
		gsFileStat size.
		gsFileStat mode bitAnd: 8r777. "https://github.com/GemTalk/FileSystemGs/issues/11"
		(gsFileStat mode bitAnd: 8r0120000) = 8r0120000. "https://github.com/GemTalk/FileSystemGs/issues/10"
	}
%

category: 'file creation'
classmethod: File
named: fileName
	"Open a file with the given name for reading and writing. If the name has no directory part, then the file will be created in the default directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	^ self new named: fileName
%

category: 'primitives-file'
classmethod: File
open: fileName writable: writeMode
	"Open a file of the given name, and return the GsFile obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	"writeMode - #read, #append, #write"

	| mode |
	mode := writeMode == #write 
		ifTrue: [ 'w+' ] 
		ifFalse: [ 
			writeMode = #read
				ifTrue: [ 'r' ]
				ifFalse: [ 
           writeMode == #append 
             ifTrue:[ 'a+' ] 
             ifFalse:[ Error signal:'invalid mode']]].
	^ GsFile
		open: fileName 
		mode: mode 
		onClient: false
%

category: 'file creation'
classmethod: File
openAndTruncateForWriteFileNamed: aName

	^ (self named: aName)
		delete;
		openForWrite
%

category: 'file creation'
classmethod: File
openForReadFileNamed: aName

	^ (self named: aName)
		openForRead
%

category: 'file creation'
classmethod: File
openForWriteFileNamed: aName

	^ (self named: aName) openForWrite
%

category: 'primitives-file'
classmethod: File
read: aGsFile into: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. 
		Answer the number of bytes actually read."

	| buf cnt |
	buf := byteArray species new: count.
	cnt := aGsFile next: count into: buf.
	cnt ifNil: [ 
		(self closed: aGsFile)
			ifTrue: [ ^ self error: 'File is closed' ].
		self error: 'File read failed: ' , (GsFile serverErrorString ifNil: [ '']) ].
	byteArray replaceFrom: startIndex to: startIndex + cnt - 1 with: buf.
	^cnt
%

category: 'primitives-path'
classmethod: File
rename: oldFileFullName to: newFileFullName 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	(GsFile renameFileOnServer: oldFileFullName to: newFileFullName) = 0
		ifFalse: [ self error: 'Rename file failed'].
%

category: 'primitives-file'
classmethod: File
setPosition: aGsFile to: anInteger
	"Set this file to the given position."

	| pos |
	pos := aGsFile position: anInteger.
	pos ifNil: [ self error: 'position error' ].
%

category: 'primitives-file'
classmethod: File
sizeOf: aGsFile
	"Answer the size of this file."

	^ aGsFile fileSize
%

category: 'primitives-file'
classmethod: File
sizeOrNil: aGsFile
	"Answer the size of this file."

	aGsFile ifNil: [ ^ nil ].
	^ aGsFile fileSize
%

category: 'primitives-file'
classmethod: File
stdioHandles
"
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed
"
self error: 'not yet implmented'
%

category: 'primitives-file'
classmethod: File
stdioIsAvailable
	"Answer a boolean indicating whether stdio is available on the current platform.
	stdio is considered available if any one of the three files (stdin, stdout, stderr) is available."

	^ (0 to: 2)
		anySatisfy: [ :fdNum | self fileDescriptorIsAvailable: fdNum ]
%

category: 'primitives-file'
classmethod: File
sync: id
	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing"

"
	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

"
self error: 'not yet implmented'

	"fsync() failing cannot be ignored"
	self primitiveFailed
%

category: 'primitives-file'
classmethod: File
truncate: id to: anInteger
	"Truncate this file to the given position."
"
	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed
"
self error: 'not yet implmented'
%

category: 'primitives-file'
classmethod: File
write: aGsFile from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. 	Answer the number of bytes written."

	| written |
	written := (startIndex = 1 and: [ count = stringOrByteArray size])
		ifTrue: [ aGsFile nextPutAll: stringOrByteArray ]
		ifFalse: [ aGsFile nextPutAll: (stringOrByteArray copyFrom: startIndex to: startIndex + count - 1) ].
	written ifNil: [ self error: 'failed write' ].
	^ written
%

!		Instance methods for 'File'

category: 'printing'
method: File
asString
  "Needed for topaz debugging"
  ^ name
%

category: 'accessing'
method: File
basename
	^self name
%

category: 'open/close'
method: File
basicOpenForWrite: writeMode 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."

	"writeMode - #read, #append, #write"

	^ self class open: name writable: writeMode
%

category: 'testing'
method: File
checkDoesNotExist
	"This method implements a backwards compatible logic to #newFileNamed:
	
	If the file does not exist, this method has no effect, and returns self.
	
	If the file exists, it will throw a FileAlreadyExistsException.
	If unhandled, this will open a pop-up asking the user to enter a new name or to override the existing file.
	In this case, this method returns a new file with the options selected by the user.
	"
	self exists ifTrue: [
		^ FileAlreadyExistsException signalOnFile: self ]
%

category: 'open/close'
method: File
delete
	"We retries with GC because in some platforms open files cannot be open"
	self class deleteFile: name.
	self exists
		ifTrue: [ (CannotDeleteFileException new messageText: 'Could not delete file ' , name,'. Check the file is not open.') signal ].
%

category: 'testing'
method: File
exists

	| handle |
	"We open it for read. If the returned handle is nil, the file could not be opened"
	handle := self basicOpenForWrite: #read.
	handle ifNil: [^ false].
	self class close: handle.
	^ true
%

category: 'instance creation'
method: File
name

	^ name
%

category: 'instance creation'
method: File
named: fileName

	name := fileName
%

category: 'open/close'
method: File
openForAppend

	| stream |
	stream := self openForWrite: #append.
	^ stream setToEnd; yourself
%

category: 'open/close'
method: File
openForRead

	^ self openForWrite: #read
%

category: 'open/close'
method: File
openForWrite

	^ self openForWrite: #write
%

category: 'open/close'
method: File
openForWrite: writeMode
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."

	"writeMode - #read, #append, #write"

	| fileHandle |
	fileHandle := self basicOpenForWrite: writeMode.
	fileHandle ifNil: [
		"Opening the file failed.
		If the file does not exist, we throw an explicit FileDoesNotExistException.
		Otherwise, we throw a generic FileException."
		self exists
			ifFalse: [ ^ FileDoesNotExistException signalWithFile: self writeMode: writeMode == #write ].
		CannotDeleteFileException signal: name
	].

	^ BinaryFileStream handle: fileHandle file: self forWrite: writeMode ~~ #read
%

category: 'streaming'
method: File
readStream
	
	^ self openForRead
%

category: 'streaming'
method: File
readStreamDo: aBlock
	| stream |
	stream := self readStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
%

category: 'accessing'
method: File
size

	^ self readStreamDo: [ :stream | stream size ]
%

category: 'streaming'
method: File
writeStream

	^ self openForWrite
%

category: 'streaming'
method: File
writeStreamDo: aBlock
	| stream |
	stream := self writeStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
%

! Class implementation for 'FileSystem'

!		Class methods for 'FileSystem'

category: 'instance creation'
classmethod: FileSystem
store: aStore
	^ self basicNew
		initializeWithStore: aStore;
		yourself
%

!		Instance methods for 'FileSystem'

category: 'navigating'
method: FileSystem
* anObject
	"Return a relative reference."
	
	^ self referenceTo:( Path * anObject)
%

category: 'navigating'
method: FileSystem
/ anObject
	"Return the absolute reference obtained by resolving anObject against the
	root of this filesystem."
	
	^ self root / anObject
%

category: 'comparing'
method: FileSystem
= other
	^ self species = other species and: [self store = other store]
%

category: 'public'
method: FileSystem
binaryReadStreamOn: aResolvable
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable writable: false) binaryReadStream.
%

category: 'public'
method: FileSystem
binaryWriteStreamOn: aResolvable
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a binary write stream for reading the file."

	^ (self open: aResolvable writable: true) binaryWriteStream.
%

category: 'public'
method: FileSystem
checkName: aString fixErrors: fixErrors
	^ store checkName: aString fixErrors: fixErrors
%

category: 'public-enumerating'
method: FileSystem
childNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self childNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
childNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
%

category: 'public-enumerating'
method: FileSystem
childrenAt: aResolvable
	^ Array streamContents: [ :out | 
		self childrenAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
childrenAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
%

category: 'public'
method: FileSystem
close
	store close
%

category: 'public'
method: FileSystem
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock
	"Copy the file referenced as sourcePath to the destination referred as destPath. Perform associate actions in case of problems."

	store
		copy: (self resolve: sourcePath)
		ifAbsent: absentBlock
		to: (self resolve: destinationPath)
		ifPresent: presentBlock
		fileSystem: self
%

category: 'public'
method: FileSystem
copy: sourcePath to: destPath
	"Copy the file referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists."
	
	self
		copy: sourcePath
		ifAbsent: [ store signalFileDoesNotExist: sourcePath ]
		to: destPath
		ifPresent: [ store signalFileExists: destPath ]
%

category: 'public'
method: FileSystem
copy: aPath toReference: destinationReference

	^self = destinationReference fileSystem
		ifTrue: [ self copy: aPath to: destinationReference resolve path ]
		ifFalse: [ self copy: aPath toRemote: destinationReference ]
%

category: 'public'
method: FileSystem
copy: aPath toRemote: destRef
	| inputStream path |
	path := self resolve: aPath.
	[ inputStream := self binaryReadStreamOn: path.
	inputStream ifNil: [ store signalFileDoesNotExist: path ].
	destRef fileSystem copyFrom: inputStream to: destRef path ]
		ensure: [ inputStream ifNotNil: [ inputStream close ] ]
%

category: 'public'
method: FileSystem
copyAndDelete: sourcePath to: destination
	"Copy the file referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If an error occurs during the operation, try and roll back to the original state."
	
	[
		self copy: sourcePath toReference: destination.
		self delete: sourcePath.
	] on: Error do: [ :error |
		destination delete.
		error signal.
	]
%

category: 'private'
method: FileSystem
copyFrom: inputStream to: destPath
	| buffer out |
	out := nil.
	(self exists: destPath)
		ifTrue: [ store signalFileExists: destPath ].
	^ [ 
	out := self binaryWriteStreamOn: destPath.
	buffer := ByteArray new: 1024.
	[ inputStream atEnd ]
		whileFalse: [ 
			buffer := inputStream nextInto: buffer.
			out nextPutAll: buffer ]] ensure: [ out ifNotNil: [ out close ]]
%

category: 'public'
method: FileSystem
createDirectory: aResolvable
	"Resolve aResolvable into an absolute path, then as the store to create a directory there. 
	The store is expected to raise an exception if it cannot do so."

	^ store createDirectory: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
creationTimeOf: aResolvable
	"Returns the creation date of aResolvable"

	^ store creationTimeOf: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
delete: aResolvable
	store delete: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
delimiter
	"Return path delimiter used by this filesystem."

	^ store delimiter
%

category: 'public-enumerating'
method: FileSystem
directoriesAt: aResolvable
	^ Array streamContents: [ :out | 
		self directoriesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
directoriesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		directoryNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
%

category: 'public-enumerating'
method: FileSystem
directoryNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self directoryNamesAt: aResolvable do: [ :name|
			out nextPut: name ]].
%

category: 'public-enumerating'
method: FileSystem
directoryNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		directoryNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
%

category: 'public'
method: FileSystem
ensureCreateDirectory: aResolvable
	"Resolve the argument to an absolute path, then ask the store to make
	sure that all the directories contained in the argument path exist or are created."
	
	store ensureCreateDirectory: (self resolve: aResolvable)
%

category: 'public-enumerating'
method: FileSystem
entriesAt: aResolvable
	^ Array
		streamContents: [ :out | 
			self entriesAt: aResolvable do: [ :entry | out nextPut: entry ]
		]
%

category: 'public-enumerating'
method: FileSystem
entriesAt: aResolvable do: aBlock
	^ self 
		entriesAt: aResolvable 
		ifAbsent: [ store signalDirectoryDoesNotExist: ( self resolve: aResolvable) ]
		do: aBlock 
%

category: 'public-enumerating'
method: FileSystem
entriesAt: aResolvable ifAbsent: absentBlock do: aBlock
	| path entry aFilesystem |
	path := self resolve: aResolvable.
	aFilesystem := self.
	store
		directoryAt: path
		ifAbsent: [ ^ absentBlock value ]
		nodesDo: [ :node | 
			entry := store entryFromNode: node path: path for: aFilesystem.
			aBlock value: entry ]
%

category: 'public'
method: FileSystem
entryAt: aResolvable
	| path |
	path := self resolve: aResolvable.
	^ store
		nodeAt: path
		ifPresent: [ :node | store entryFromNode: node fileSystem: self path: path ]
		ifAbsent: [ store signalFileDoesNotExist: path ]
%

category: 'public'
method: FileSystem
exists: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory at that path, false if there is not."
	
	^ store exists: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
extensionDelimiter
	^ $.
%

category: 'public-enumerating'
method: FileSystem
fileNamesAt: aResolvable
	^ Array streamContents: [ :out | 
		self fileNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
fileNamesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		fileNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
%

category: 'public-enumerating'
method: FileSystem
filesAt: aResolvable
	^ Array streamContents: [ :out | 
		self filesAt: aResolvable do: [ :path|
			out nextPut: path ]].
%

category: 'public-enumerating'
method: FileSystem
filesAt: aResolvable do: aBlock
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		fileNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
%

category: 'printing'
method: FileSystem
forReferencePrintOn: aStream
	store forReferencePrintOn: aStream
%

category: 'public-testing'
method: FileSystem
hasChildren: aResolvable
	"Returns whether aResolvable has children."
	store directoryAt: (self resolve: aResolvable)
			ifAbsent: [ ^false ]
			nodesDo:  [ :node | ^true ].
	^false
%

category: 'public-testing'
method: FileSystem
hasDirectories: aResolvable
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | 
			entry isDirectory ifTrue: [ ^true ] ].
	^false
%

category: 'public-testing'
method: FileSystem
hasFiles: aResolvable
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | entry isFile ifTrue: [ ^true ] ].
	^false
%

category: 'comparing'
method: FileSystem
hash
	^ store hash
%

category: 'initialize-release'
method: FileSystem
initializeWithStore: aStore
	store := aStore
%

category: 'public-testing'
method: FileSystem
isCaseSensitive
	^ self store isCaseSensitive
%

category: 'public-testing'
method: FileSystem
isDirectory: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isDirectory: (self resolve: aResolvable)
%

category: 'public-testing'
method: FileSystem
isFile: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a file, false if it refers to a directory or doesn't exist."

	^ store isFile: (self resolve: aResolvable)
%

category: 'public-testing'
method: FileSystem
isReadable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ (self permissions: aResolvable) isReadable
%

category: 'public-testing'
method: FileSystem
isSymlink: aResolvable
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isSymlink: (self resolve: aResolvable)
%

category: 'public-testing'
method: FileSystem
isWritable: aResolvable
	"Resolve the argument, and answer true if the there is
	a file that can be written to or directory that can be changed."
	
	^  (self permissions: aResolvable) isWritable
%

category: 'public'
method: FileSystem
modificationTime: aResolvable
	"Returns the date of the last modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
modificationTimeOf: aResolvable
	"Returns the last date of modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)
%

category: 'public'
method: FileSystem
move: sourcePath to: destination
	"Move the file /directory referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If destPath is a directory, move the sourcePath in to the directory"

	| fullDestination |

	destination isFile ifTrue: [ FileExists signalWith: destination ].
	destination isDirectory
		ifTrue: [ fullDestination := destination / sourcePath basename ]
		ifFalse: [ fullDestination := destination ].
	self = destination fileSystem ifTrue: 
	[
		"Ideally we would test whether the source and destination are on the same filesystem from the OSs perspective.
		Since we can't do that, just try rename, and if that fails, copy and delete."
		[ self rename: sourcePath to: fullDestination resolve path ]
			on: Error
			do: [ :error | self copyAndDelete: sourcePath to: fullDestination ].
	] ifFalse:
		[ self copyAndDelete: sourcePath to: fullDestination ].
	^fullDestination
%

category: 'public'
method: FileSystem
open
	"Some kinds of filesystems need to open connections to external resources. Does nothing by default."
	
	store open
%

category: 'public'
method: FileSystem
open: aResolvable writable: aBoolean 
	"Resolve aResolvable into an absolute path, then ask the store to open the file at
	that path using the specified access mode."
	
	| path |
	path := self resolve: aResolvable.
	^ store handleClass 
		open: (FileReference fileSystem: self path: path) 
		writable: aBoolean
		
%

category: 'delegated'
method: FileSystem
openFileStream: aResolvable writable: aBoolean
	^ store 
		openFileStream: (self resolve: aResolvable) 
		writable: aBoolean
%

category: 'private'
method: FileSystem
openStreamDescription: aResolvable writable: writeMode
	"I am  a helper method to delegate basicOpen:writable: to the store.
	 I am called from FileSystemHandle implementations."
	
	"writeMode - #read, #append, #write"

	| path |
	
	path := self resolve: aResolvable.
	^ store basicOpen: path writable: writeMode
%

category: 'converting'
method: FileSystem
pathFromObject: anObject 
	^ anObject asPathWith: self
%

category: 'converting'
method: FileSystem
pathFromString: aString
	^ store pathFromString: aString
%

category: 'public'
method: FileSystem
permissions: aResolvable
	"Resolve the argument and return the Permissions for this file or directory "

	^ store permissions: (self resolve: aResolvable)
%

category: 'printing'
method: FileSystem
printPath: aPath on: aStream
	store printPath: aPath on: aStream
%

category: 'public'
method: FileSystem
readStreamOn: aResolvable
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable writable: false) readStream.
%

category: 'public'
method: FileSystem
referenceTo: aResolvable 
	"Answer a reference to the argument from the context of the receiver filesystem.  	
		Example: Filesystem disk referenceTo: 'plonk.taz'"

	^ FileReference 
		fileSystem: self
		path: (self pathFromObject: aResolvable)
%

category: 'public'
method: FileSystem
rename: sourcePath ifAbsent: aBlock to: destPath ifPresent: pBlock
	"Rename the file referenced as sourcePath to the destination referred as destPath. 
	Perform associate actions in case of problems."
	
	| source destination |
	source := self resolve: sourcePath.
	destination := self resolve: destPath.
	store
		rename: source
		ifAbsent: aBlock
		to: destination
		ifPresent: pBlock
		fileSystem: self.
	^ destination
%

category: 'public'
method: FileSystem
rename: sourcePath to: destName
	"Rename the file referenced as sourcePath to destPath.  Raise exceptions 
	FileExists or FileDoesNotExist  if the operation fails"
	
	^ self
		rename: sourcePath
		ifAbsent: [store signalFileDoesNotExist: sourcePath]
		to: destName
		ifPresent: [store signalFileExists: destName]
%

category: 'public'
method: FileSystem
resolve: aResolvable
	^ aResolvable asResolvedBy: self
%

category: 'navigating'
method: FileSystem
resolvePath: aPath
	"Return a path where the argument is resolved in the context of the
	receiver. The behavior is similar to the one of a command line.
		> cd /a/b/c
		> cd b
		The shell will attempt to make /a/b/c/b the current directory. "

	^ self workingDirectoryPath resolve: aPath
%

category: 'navigating'
method: FileSystem
resolveReference: aReference
	^ aReference fileSystem = self ifTrue:
		[self workingDirectoryPath resolvePath: aReference path]
%

category: 'navigating'
method: FileSystem
resolveString: aString
	"Returns the root of the receiver filesystem, i.e. / on unix"
	
	^ self workingDirectoryPath resolvePath: (store pathFromString: aString)
%

category: 'accessing'
method: FileSystem
root
	"Returns the root of the receiver filesystem, i.e. / on unix"
	^ self referenceTo: Path root
%

category: 'accessing'
method: FileSystem
separator
	"Return path separator used by this filesystem."

	^ store separator
%

category: 'public'
method: FileSystem
sizeOf: aResolvable
	"Resolve the argument and return the size for this file or directory "

	^ store sizeOf: (self resolve: aResolvable)
%

category: 'accessing'
method: FileSystem
store
	^ store
%

category: 'converting'
method: FileSystem
stringFromPath: aPath
	^ store stringFromPath: aPath
%

category: 'accessing'
method: FileSystem
workingDirectory
	"Returns a reference to the directory from where the image was launched"

	^ self referenceTo: self workingDirectoryPath
%

category: 'accessing'
method: FileSystem
workingDirectoryPath
	^ store defaultWorkingDirectory
%

category: 'public'
method: FileSystem
writeStreamOn: aResolvable
	"Open a write stream on the file referred by the argument. It can be a string or a path"

	^ (self open: aResolvable writable: true) writeStream.
%

! Class implementation for 'FileSystemDirectoryEntry'

!		Class methods for 'FileSystemDirectoryEntry'

category: 'accessing'
classmethod: FileSystemDirectoryEntry
allPosixPermissions 
	^8r777
%

category: 'instance creation'
classmethod: FileSystemDirectoryEntry
fileSystem: aFilesystem path: aPath creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBooleam
	"Create a directory entry given a filesystem and a path in such filesystem. In addition, the creation and modification time are required as well as a boolean that indicates whether the entry is a folder or a file and its size."

	^ self 
		reference: (aFilesystem referenceTo: aPath)
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBooleam
%

category: 'instance creation'
classmethod: FileSystemDirectoryEntry
reference: ref creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBoolean
	"Create a directory entry for the file reference ref, with the creation time, cTime, the modification time, mTime. aBoolean indicates if the entry represents a directory or a file of size given by anInteger"
	
	^ self basicNew
		initializeWithRef: ref
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBoolean
%

!		Instance methods for 'FileSystemDirectoryEntry'

category: 'converting'
method: FileSystemDirectoryEntry
asFileReference
	^ reference
%

category: 'accessing'
method: FileSystemDirectoryEntry
basename

	^ reference basename
%

category: 'accessing'
method: FileSystemDirectoryEntry
creation
	^ self creationTime 
%

category: 'accessing'
method: FileSystemDirectoryEntry
creationSeconds
	"Return the creation date and time of the entry receiver in seconds."
		
	^ creation asSeconds
%

category: 'accessing'
method: FileSystemDirectoryEntry
creationTime
	"Return the creation date and time of the entry receiver."
	
	^ creation
%

category: 'delegate'
method: FileSystemDirectoryEntry
extension
	^ reference extension
%

category: 'delegate'
method: FileSystemDirectoryEntry
fullName
	^ reference fullName
%

category: 'initialize-release'
method: FileSystemDirectoryEntry
initializeWithRef: ref 
	creation: cTime 
	modification: mTime 
	isDir: directoryBoolean
	size: bytes 
	posixPermissions: posixNumber 
	isSymlink: symlinkBoolean
	
	reference := ref.
	creation := cTime.
	modification := mTime.
	isDirectory := directoryBoolean.
	size := bytes.
	posixPermissions := posixNumber.
	isSymlink := symlinkBoolean.
%

category: 'testing'
method: FileSystemDirectoryEntry
isDirectory
	"Return whether the receiver is a directory"
	
	^ isDirectory
%

category: 'testing'
method: FileSystemDirectoryEntry
isFile
	"Return whether the receiver is a file"
	
	^ isDirectory not
%

category: 'testing'
method: FileSystemDirectoryEntry
isSymlink 
	^isSymlink
%

category: 'accessing'
method: FileSystemDirectoryEntry
modification
	^ self modificationTime 
%

category: 'accessing'
method: FileSystemDirectoryEntry
modificationSeconds
	"Return the modification date and time of the entry receiver in seconds."
	^ modification
%

category: 'accessing'
method: FileSystemDirectoryEntry
modificationTime
	"Return the modification date and time of the entry receiver."

	^  modification
%

category: 'accessing'
method: FileSystemDirectoryEntry
name
    ^ self basename
%

category: 'delegate'
method: FileSystemDirectoryEntry
pathSegments
	^ reference pathSegments
%

category: 'accessing'
method: FileSystemDirectoryEntry
permissions 
	^self posixPermissions
		ifNotNil: [ FileSystemPermission posixPermissions: self posixPermissions ]
%

category: 'accessing'
method: FileSystemDirectoryEntry
posixPermissions 
	^posixPermissions
%

category: 'printing'
method: FileSystemDirectoryEntry
printOn: aStream

	aStream nextPutAll: 'DirectoryEntry: '.
	reference ifNotNil: [:ref | aStream nextPutAll: reference printString].
%

category: 'delegate'
method: FileSystemDirectoryEntry
readStream
	^ reference readStreamPortable
%

category: 'delegate'
method: FileSystemDirectoryEntry
readStreamPortable
	^ reference readStreamPortable
%

category: 'accessing'
method: FileSystemDirectoryEntry
reference
	^ reference
%

category: 'accessing'
method: FileSystemDirectoryEntry
size
	"Returns the receiver size"
	^ size
%

category: 'delegate'
method: FileSystemDirectoryEntry
writeStream
	^ reference writeStream
%

! Class implementation for 'FileSystemGuide'

!		Class methods for 'FileSystemGuide'

category: 'instance creation'
classmethod: FileSystemGuide
for: aVisitor
	^ self basicNew initializeWithVisitor: aVisitor
%

category: 'instance creation'
classmethod: FileSystemGuide
show: aReference to: aVisitor
	^ (self for: aVisitor) show: aReference
%

category: 'instance creation'
classmethod: FileSystemGuide
show: aReference to: aVisitor selecting: aBlock
	^ (self for: aVisitor)
		selectChildren:  aBlock;
		show: aReference
%

!		Instance methods for 'FileSystemGuide'

category: 'initialization'
method: FileSystemGuide
initialize

	work := OrderedCollection new
%

category: 'initialize-release'
method: FileSystemGuide
initializeWithVisitor: aVisitor
	self initialize.
	visitor := aVisitor.
	
%

category: 'removing'
method: FileSystemGuide
pop
	^ work removeLast
%

category: 'adding'
method: FileSystemGuide
push: anObject
	work add: anObject
%

category: 'showing'
method: FileSystemGuide
pushAll: aCollection 
	aCollection do: [ :ea | self push: ea ]
%

category: 'accessing'
method: FileSystemGuide
selectChildren
	^ selectChildren
%

category: 'accessing'
method: FileSystemGuide
selectChildren: aBlock
	"With this block you can control how the guide spreads over directories.
	Example: 
		self selectChildren: [ :parentEntry | parentEntry isSymlink not ].
		This will prevent the outer visitor to see any children of symlinked directories.
		
	Since the guides essentially rearrange the files visited controlling which children
	you see is the main concern of the guide. All the other visiting aspects can be 
	controlled in the visitor."
	selectChildren := aBlock
%

category: 'testing'
method: FileSystemGuide
shouldVisitChildrenOf: anEntry
	^ selectChildren 
		ifNil: [ true ]
		ifNotNil: [ selectChildren cull: anEntry ]
%

category: 'showing'
method: FileSystemGuide
show: aReference
	self subclassResponsibility
%

category: 'accessing'
method: FileSystemGuide
top
	^ work removeFirst
%

category: 'showing'
method: FileSystemGuide
whileNotDoneDo: aBlock
	[ work isEmpty ] whileFalse: [ aBlock value ]
%

! Class implementation for 'BreadthFirstGuide'

!		Instance methods for 'BreadthFirstGuide'

category: 'showing'
method: BreadthFirstGuide
show: aReference 
	self push: aReference entry.
	self whileNotDoneDo: [ self visitNextEntry: self top ]
%

category: 'showing'
method: BreadthFirstGuide
visitNextEntry: entry
		
	entry isFile 
		ifTrue: [ ^ visitor visitFile: entry ].
	
	visitor visitDirectory: entry.
	
	(self shouldVisitChildrenOf: entry)
		ifTrue: [ self pushAll: entry reference entries ].
%

! Class implementation for 'PostorderGuide'

!		Instance methods for 'PostorderGuide'

category: 'showing'
method: PostorderGuide
pushTraverse: aReference 
	self push: (Message 
				selector: #traverse:
				argument: aReference)
%

category: 'showing'
method: PostorderGuide
pushVisit: aReference 
	self push: (Message 
				selector: #visit:
				argument: aReference)
%

category: 'showing'
method: PostorderGuide
show: aReference 
	self pushTraverse: aReference entry.
	self whileNotDoneDo: [ self pop sendTo: self ]
%

category: 'showing'
method: PostorderGuide
traverse: anEntry 
		
	self pushVisit: anEntry.
	
	anEntry isDirectory ifTrue: [ 
		(self shouldVisitChildrenOf: anEntry)
			ifFalse: [ ^ self ].
		anEntry reference entries reverseDo: [ :each | 
			self pushTraverse: each ]]
%

category: 'showing'
method: PostorderGuide
visit: anEntry
	anEntry isDirectory
		ifTrue: [ visitor visitDirectory: anEntry ]
		ifFalse: [ visitor visitFile: anEntry ]
%

! Class implementation for 'PreorderGuide'

!		Instance methods for 'PreorderGuide'

category: 'showing'
method: PreorderGuide
pushAll: aCollection 
	aCollection reverseDo: [ :each | work add: each ]
%

category: 'showing'
method: PreorderGuide
show: aReference
	self push: aReference entry.
	self whileNotDoneDo: [ self visitNextEntry: self pop ]
%

category: 'showing'
method: PreorderGuide
visitNextEntry: entry
	
	entry isFile
		ifTrue: [ visitor visitFile: entry ]
		ifFalse: [ 
			visitor visitDirectory: entry.
			(self shouldVisitChildrenOf: entry)
				ifTrue: [ self pushAll: entry reference entries ]]
%

! Class implementation for 'FileSystemHandle'

!		Class methods for 'FileSystemHandle'

category: 'instance creation'
classmethod: FileSystemHandle
on: aReference writable: aBoolean
	^ self new setReference: aReference writable: aBoolean
%

category: 'instance creation'
classmethod: FileSystemHandle
open: aReference writable: aBoolean
	^ (self on: aReference writable: aBoolean) open
%

!		Instance methods for 'FileSystemHandle'

category: 'public'
method: FileSystemHandle
at: index
	| buffer |
	buffer := ByteArray new: 1.
	self at: index read: buffer startingAt: 1 count: 1.
	^ buffer at: 1
%

category: 'public'
method: FileSystemHandle
at: index put: anObject
	| buffer |
	buffer := ByteArray with: (anObject isCharacter
		ifTrue: [ anObject codePoint ]
		ifFalse: [ anObject ]).
	self at: index write: buffer startingAt: 1 count: 1.
	
%

category: 'public'
method: FileSystemHandle
at: offset read: buffer startingAt: start count: count
	self subclassResponsibility
%

category: 'public'
method: FileSystemHandle
at: offset write: buffer startingAt: start count: count
	self subclassResponsibility
%

category: 'private'
method: FileSystemHandle
basicOpen
	"get the raw stream description from the filesystem's store"
	^ self fileSystem 
		openStreamDescription: reference path
		writable: (writable ifTrue: [ #write ] ifFalse: [ #read ])
%

category: 'streams'
method: FileSystemHandle
binaryReadStream
	
	self subclassResponsibility 
%

category: 'streams'
method: FileSystemHandle
binaryWriteStream
	
	self subclassResponsibility 
%

category: 'public'
method: FileSystemHandle
close
	self subclassResponsibility
%

category: 'public'
method: FileSystemHandle
ensureClosed
	reference exists ifTrue: [self close]
%

category: 'accessing'
method: FileSystemHandle
fileSystem
	^ reference fileSystem 
%

category: 'public'
method: FileSystemHandle
flush
	self subclassResponsibility
%

category: 'accessing'
method: FileSystemHandle
fullName
	^ reference fullName
%

category: 'testing'
method: FileSystemHandle
isOpen
	self subclassResponsibility
%

category: 'testing'
method: FileSystemHandle
isWritable
	^ writable
%

category: 'public'
method: FileSystemHandle
open
	self subclassResponsibility
%

category: 'accessing'
method: FileSystemHandle
reference
	^ reference
%

category: 'public'
method: FileSystemHandle
reopen
	self close.
	self open
%

category: 'initialize-release'
method: FileSystemHandle
setReference: aReference writable: aBoolean
	reference := aReference resolve.
	writable := aBoolean
%

category: 'public'
method: FileSystemHandle
sync
	self subclassResponsibility
%

category: 'public'
method: FileSystemHandle
truncateTo: anInteger
	self subclassResponsibility
%

! Class implementation for 'FileHandle'

!		Class methods for 'FileHandle'

category: 'class initialization'
classmethod: FileHandle
startUp: resuming
	"This functionality is disabled for now, to avoid doing a lot of processing at
	image start up. To reenable, add this class to the start up list."
	
	resuming ifTrue: [self allInstancesDo: [:ea | ea startUp]]
%

!		Instance methods for 'FileHandle'

category: 'public'
method: FileHandle
assureOpen
	"Compatibility method to make the FileHandle Tests pass"
	self isOpen ifFalse: [ id := self basicOpen ].
%

category: 'public'
method: FileHandle
at: index read: buffer startingAt: start count: count
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ File 
		setPosition: id to: index - 1;
		read: id into: buffer startingAt: start count: count
%

category: 'public'
method: FileHandle
at: index write: buffer startingAt: start count: count
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	File 
		setPosition: id to: index - 1;
		write: id from: buffer startingAt: start count: count
%

category: 'public'
method: FileHandle
binaryReadStream
	^ ZnBufferedReadStream on: (File named: reference fullName) readStream
%

category: 'public'
method: FileHandle
binaryWriteStream
	^ (File named: reference fullName) writeStream
%

category: 'public'
method: FileHandle
close
	File close: id.
	id := nil
%

category: 'finalization'
method: FileHandle
finalize
	File close: id
%

category: 'public'
method: FileHandle
flush
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	File flush: id
%

category: 'testing'
method: FileHandle
isOpen
	^ (File sizeOrNil: id) notNil
%

category: 'public'
method: FileHandle
open
	self flag: 'TODO: for now we solely rely on the old FileStreams'
	"id := self basicOpen.
	id ifNil: 	[
		reference exists ifFalse: [FileDoesNotExist signalWith: reference].
		self error: 'Unable to open file ' , reference printString]"
%

category: 'public'
method: FileHandle
size
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ File sizeOf: id
%

category: 'private'
method: FileHandle
startUp
	"This functionality is disabled for now, to avoid doing lots of processing
	on start up."
	
	"We're starting up in a new OS process, so the file id will be invalid.
	Try to reopen the file, but fail silently: just leave the id as nil. #isOpen will
	answer false, and we'll raise an error if anyone tries to do IO."
	
	self basicOpen
%

category: 'public'
method: FileHandle
streamError
	reference exists
		ifFalse: [FileDoesNotExistException signalWith: reference].
	self error: 'Unable to open file ' , reference printString
%

category: 'public'
method: FileHandle
sync
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	File sync: id
%

category: 'public'
method: FileHandle
truncateTo: anInteger
	File setPosition: id to: anInteger.
	File truncate: id to: anInteger.
	self reopen
%

! Class implementation for 'MemoryHandle'

!		Instance methods for 'MemoryHandle'

category: 'public'
method: MemoryHandle
at: index
	^ entry at: index
%

category: 'public'
method: MemoryHandle
at: index put: anObject
	^ entry at: index put: anObject
%

category: 'public'
method: MemoryHandle
at: index read: aCollection startingAt: start count: count 
	^ entry at: index read: aCollection startingAt: start count: count 
%

category: 'public'
method: MemoryHandle
at: first write: aCollection startingAt: start count: count 
	writable ifFalse: [ self primitiveFailed ].
	entry at: first write: aCollection startingAt: start count: count.
%

category: 'streams'
method: MemoryHandle
binaryReadStream

	^ entry binaryReadStream
%

category: 'streams'
method: MemoryHandle
binaryWriteStream
	
	^ entry binaryWriteStream
%

category: 'public'
method: MemoryHandle
close
	self isOpen ifFalse: [ ^ self ].
	self truncate.
	entry := nil.
%

category: 'stream-protocol'
method: MemoryHandle
copyFrom: from to: position
	^ entry copyFrom: from to: position
%

category: 'public'
method: MemoryHandle
flush
	self truncate
%

category: 'private'
method: MemoryHandle
grow
	entry grow
%

category: 'stream-protocol'
method: MemoryHandle
grownBy: length
	entry grownBy: length
%

category: 'testing'
method: MemoryHandle
isOpen
	^ entry notNil
%

category: 'public'
method: MemoryHandle
open
	entry := self basicOpen.
%

category: 'accessing'
method: MemoryHandle
size
	"return the size for the interna"
	^ entry internalSize
%

category: 'public'
method: MemoryHandle
sync
	self flush
%

category: 'public'
method: MemoryHandle
truncate
	entry truncate
%

category: 'public'
method: MemoryHandle
truncateTo: anInteger
	entry truncateTo: anInteger
%

! Class implementation for 'FileSystemPermission'

!		Class methods for 'FileSystemPermission'

category: 'instance creation'
classmethod: FileSystemPermission
default
	^ self posixPermissions: 8r777
%

category: 'instance creation'
classmethod: FileSystemPermission
new 
	self error: 'Should not be called. Use #posixPermission: instead'
%

category: 'instance creation'
classmethod: FileSystemPermission
posixPermissions: aNumber
	^self basicNew 
		initialize: aNumber;
		yourself
%

!		Instance methods for 'FileSystemPermission'

category: 'comparing'
method: FileSystemPermission
< other
	^ posixPermission < other posixPermission
%

category: 'comparing'
method: FileSystemPermission
<= other
	^ (posixPermission > other posixPermission) not
%

category: 'comparing'
method: FileSystemPermission
> other
	^ other posixPermission < posixPermission
%

category: 'comparing'
method: FileSystemPermission
>= other
	^ other posixPermission <= posixPermission
%

category: 'accessing'
method: FileSystemPermission
groupExecute
	^ self permissionBitAt: 4
%

category: 'accessing'
method: FileSystemPermission
groupRead
	^ self permissionBitAt: 6
%

category: 'accessing'
method: FileSystemPermission
groupWrite
	^ self permissionBitAt: 5
%

category: 'initialization'
method: FileSystemPermission
initialize
%

category: 'initialization'
method: FileSystemPermission
initialize: aNumber
	posixPermission := aNumber.
	self initialize.	
%

category: 'testing'
method: FileSystemPermission
isReadable
	^ self ownerRead
%

category: 'testing'
method: FileSystemPermission
isWritable
	^ self ownerWrite
%

category: 'accessing'
method: FileSystemPermission
otherExecute
	^ self permissionBitAt: 1
%

category: 'accessing'
method: FileSystemPermission
otherRead
	^ self permissionBitAt: 3
%

category: 'accessing'
method: FileSystemPermission
otherWrite
	^ self permissionBitAt: 2
%

category: 'accessing'
method: FileSystemPermission
ownerExecute
	^ self permissionBitAt: 7
%

category: 'accessing'
method: FileSystemPermission
ownerRead
	^ self permissionBitAt: 9
%

category: 'accessing'
method: FileSystemPermission
ownerWrite
	^ self permissionBitAt: 8
%

category: 'accessing'
method: FileSystemPermission
permissionBitAt: bitIndex
	^ (posixPermission bitAt: bitIndex) == 1
%

category: 'private'
method: FileSystemPermission
posixPermission
	^ posixPermission
%

category: 'printing'
method: FileSystemPermission
printOn: aStream
	aStream 
		"Owner"
		nextPut: (self ownerRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self ownerWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self ownerExecute ifTrue: [ $x ] ifFalse: [ $- ]);
		"Group"
		nextPut: (self groupRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self groupWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self groupExecute ifTrue: [ $x ] ifFalse: [ $- ]);
		"Other"
		nextPut: (self otherRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self otherWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self otherExecute ifTrue: [ $x ] ifFalse: [ $- ])
		
%

! Class implementation for 'FileSystemResolver'

!		Class methods for 'FileSystemResolver'

category: 'instance creation'
classmethod: FileSystemResolver
new

	^ self basicNew
		initialize;
		yourself
%

!		Instance methods for 'FileSystemResolver'

category: 'accessing'
method: FileSystemResolver
addResolver: aResolver
	next
		ifNil: [next := aResolver]
		ifNotNil: [next addResolver: aResolver]
%

category: 'resolving'
method: FileSystemResolver
canResolve: aSymbol
	^ self supportedOrigins includes: aSymbol
%

category: 'accessing'
method: FileSystemResolver
flushCaches
	self flushLocalCache.
	next ifNotNil: [next flushCaches]
%

category: 'accessing'
method: FileSystemResolver
flushLocalCache
%

category: 'initialization'
method: FileSystemResolver
initialize
%

category: 'accessing'
method: FileSystemResolver
next
	^ next
%

category: 'resolving'
method: FileSystemResolver
resolve: aSymbol
	^ (self canResolve: aSymbol)
		ifTrue: [self perform: aSymbol]
		ifFalse: [self unknownOrigin: aSymbol]
%

category: 'resolving'
method: FileSystemResolver
resolveString: aString
	| decoded fs |
	"The argument string is actually a byte array encoded differently on each platform.
	We are transforming it to an image string.
	We assume for now that the string is utf8 encoded."
	decoded := aString decodeFromUTF8 asString.
	fs := FileSystem disk.
	^ FileReference 
		fileSystem: fs 
		path: (fs pathFromString: decoded)
%

category: 'resolving'
method: FileSystemResolver
supportedOrigins
	^ #()
%

category: 'resolving'
method: FileSystemResolver
unknownOrigin: aSymbol
	^ next ifNotNil: [next resolve: aSymbol]
%

! Class implementation for 'InteractiveResolver'

!		Instance methods for 'InteractiveResolver'

category: 'accessing'
method: InteractiveResolver
flushLocalCache
	cache := IdentityDictionary new
%

category: 'initialization'
method: InteractiveResolver
initialize
	self flushLocalCache
%

category: 'resolving'
method: InteractiveResolver
resolve: origin
	^ cache at: origin ifAbsent: [self unknownOrigin: origin]
	
%

category: 'resolving'
method: InteractiveResolver
unknownOrigin: origin
	| reference |
	^ (next ifNotNil: [next resolve: origin]) ifNil: 
			[reference := ResolutionRequest for: origin.
			reference ifNotNil: [cache at: origin put: reference]]
%

! Class implementation for 'PlatformResolver'

!		Class methods for 'PlatformResolver'

category: 'instance creation'
classmethod: PlatformResolver
forCurrentPlatform
	| platformName |
	platformName :=  System gemVersionAt: 'osName' .
	^ (self allSubclasses detect: [:ea | ea platformName = platformName]) new
%

category: 'accessing'
classmethod: PlatformResolver
platformName
	^ nil
%

!		Instance methods for 'PlatformResolver'

category: 'origins'
method: PlatformResolver
cache
	"Operating Systems often define standard locations for a personal cache directory. The cache directory is a user-specific non-essential (cached) place where data should be written."
	self subclassResponsibility
%

category: 'private'
method: PlatformResolver
cantFindOriginError
	^ Error signal: 'Can''t find the requested origin' 
%

category: 'origins'
method: PlatformResolver
desktop
	^ self subclassResponsibility
%

category: 'private'
method: PlatformResolver
directoryFromEnvVariableNamed: aString
	^ self directoryFromEnvVariableNamed: aString or: [ self cantFindOriginError ]
%

category: 'private'
method: PlatformResolver
directoryFromEnvVariableNamed: aString or: aBlock
	| envValue |
	envValue := [ System gemEnvironmentVariable: aString ]
		on: Error
		do: [ ^ aBlock value ].
	^ envValue isEmptyOrNil
		ifTrue: [ aBlock value ]
		ifFalse: [ self resolveString: envValue ]
%

category: 'origins'
method: PlatformResolver
documents
	^ self subclassResponsibility
%

category: 'origins'
method: PlatformResolver
home
	^ self subclassResponsibility
%

category: 'origins'
method: PlatformResolver
preferences
	^ self subclassResponsibility
%

category: 'resolving'
method: PlatformResolver
supportedOrigins
	^ #(home desktop documents preferences cache temp)
%

category: 'origins'
method: PlatformResolver
temp
	"Where to put files that are not supposed to last long"
	^ self subclassResponsibility 
%

! Class implementation for 'MacOSResolver'

!		Class methods for 'MacOSResolver'

category: 'accessing'
classmethod: MacOSResolver
platformName
	^  'Mac OS'
%

!		Instance methods for 'MacOSResolver'

category: 'origins'
method: MacOSResolver
cache
	^ self library / 'Caches'
%

category: 'origins'
method: MacOSResolver
desktop
	^ self home / 'Desktop'
%

category: 'origins'
method: MacOSResolver
documents
	^ self home / 'Documents'
%

category: 'origins'
method: MacOSResolver
home
	^ self directoryFromEnvVariableNamed: 'HOME'
%

category: 'origins'
method: MacOSResolver
library
	^ self userLibrary
%

category: 'origins'
method: MacOSResolver
preferences 
	^ self home / 'Library' / 'Preferences'
%

category: 'resolving'
method: MacOSResolver
supportedOrigins
	^ super supportedOrigins , #(userApplicationSupport systemApplicationSupport systemLibrary userLibrary)
%

category: 'origins'
method: MacOSResolver
systemApplicationSupport
	^ self systemLibrary / 'Application Support'
%

category: 'origins'
method: MacOSResolver
systemLibrary
	^  FileSystem disk root / 'Library'
%

category: 'origins'
method: MacOSResolver
temp
	^ '/tmp' asFileReference
%

category: 'origins'
method: MacOSResolver
userApplicationSupport
	^self userLibrary / 'Application Support'
%

category: 'origins'
method: MacOSResolver
userLibrary
	^  self home / 'Library'
%

! Class implementation for 'UnixResolver'

!		Class methods for 'UnixResolver'

category: 'accessing'
classmethod: UnixResolver
platformName
	^  'Linux'
%

!		Instance methods for 'UnixResolver'

category: 'origins'
method: UnixResolver
cache
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CACHE_HOME' or: [ self home / '.cache' ]
%

category: 'origins'
method: UnixResolver
desktop
	^ (self xdgUserDir: 'DESKTOP') ifNil: [ self home / 'Desktop' ]
%

category: 'origins'
method: UnixResolver
documents
	^ (self xdgUserDir: 'DOCUMENTS') ifNil: [ self home / 'Documents' ]
%

category: 'origins'
method: UnixResolver
home
	^ self directoryFromEnvVariableNamed: 'HOME'
%

category: 'origins'
method: UnixResolver
preferences
	"http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html"

	^ self directoryFromEnvVariableNamed: 'XDG_CONFIG_HOME' or: [ self home / '.config' ]
%

category: 'resolving'
method: UnixResolver
supportedOrigins
	^ super supportedOrigins , #( userData )
%

category: 'origins'
method: UnixResolver
temp
	^ '/tmp' asFileReference
%

category: 'origins'
method: UnixResolver
userData
	^ self directoryFromEnvVariableNamed: 'XDG_DATA_HOME' or: [ self home / '.local' / 'share' ]
%

category: 'helpers'
method: UnixResolver
xdgParseUserDirLine: aStream
	"Format is XDG_xxx_DIR=""$HOME/yyy"", where yyy is a shell-escaped homedir-relative path, or XDG_xxx_DIR=""/yyy"", where /yyy is an absolute path. No other format is supported."
	| path firstChar |
	(aStream next = $") ifFalse: [ ^ nil ].
	firstChar := aStream next.
	(#($$ $/) includes: firstChar) ifFalse: [ ^ nil ].
	path := firstChar = $$
				ifTrue: [ (aStream next: 5) = 'HOME/' ifFalse: [ ^ nil ].
					       self home / (aStream upTo: $") ]
				ifFalse: [ self resolveString: '/', (aStream upTo: $") ].
	^ path
%

category: 'helpers'
method: UnixResolver
xdgUserDir: userDirName
	"Read ~/.config/user-dirs.dirs to find the directory of userDirName (e.g., 'DESKTOP')"
	"http://freedesktop.org/wiki/Software/xdg-user-dirs"
	"This file is written by xdg-user-dirs-update If you want to change or add directories, just edit the line you're interested in. All local changes will be retained on the next run Format is XDG_xxx_DIR=""$HOME/yyy"", where yyy is a shell-escaped homedir-relative path, or XDG_xxx_DIR=""/yyy"", where /yyy is an absolute path. No other format is supported."
	| configFile |
	configFile := self preferences / 'user-dirs.dirs'.
	(configFile isFile and: [ configFile isReadable ]) ifFalse: [ ^ nil ].
	configFile readStreamDo: [ :stream | 
		[ stream atEnd ]
			whileFalse: [ 
				((stream peek ~= $#) and: [ (stream upTo: $=) = ('XDG_', userDirName, '_DIR') ])
							ifTrue: [ ^ self xdgParseUserDirLine: stream ]
							ifFalse: [ stream nextLine ] ] ].
	^ nil
%

! Class implementation for 'WindowsResolver'

!		Class methods for 'WindowsResolver'

category: 'accessing'
classmethod: WindowsResolver
platformName
	^ 'Win32'
%

!		Instance methods for 'WindowsResolver'

category: 'origins'
method: WindowsResolver
cache
	"Because Windows does not define any location for putting cache files, we chose to use the preference directory. http://libertyboy.free.fr/computing/reference/envariables/"
	^ self preferences
%

category: 'origins'
method: WindowsResolver
desktop
	^ self home / 'Desktop'
%

category: 'origins'
method: WindowsResolver
documents
	^ self home / 'Documents'
%

category: 'origins'
method: WindowsResolver
home
	"Value of the $USERPROFILE environment variable."

	^ self directoryFromEnvVariableNamed: 'USERPROFILE'
%

category: 'origins'
method: WindowsResolver
preferences
	^ self directoryFromEnvVariableNamed: 'APPDATA' or: [ self home ]
%

category: 'origins'
method: WindowsResolver
temp
	^ self directoryFromEnvVariableNamed: 'TEMP' or: [ FileLocator C / 'windows' / 'temp' ]
%

! Class implementation for 'SystemResolver'

!		Class methods for 'SystemResolver'

category: 'accessing'
classmethod: SystemResolver
defaultLocalDirectoryName
	^ 'pharo-local'
%

category: 'accessing'
classmethod: SystemResolver
userLocalDirectory
	^ UserLocalDirectory ifNil: [ 
		(FileLocator imageDirectory / self defaultLocalDirectoryName) resolve  ]
%

category: 'accessing'
classmethod: SystemResolver
userLocalDirectory: aFileReference
	UserLocalDirectory := aFileReference
%

!		Instance methods for 'SystemResolver'

category: 'origins'
method: SystemResolver
dbfScratchDir
	"Answer the path to the DBF_SCRATCH_DIR"

	^ self  resolveString: (System stoneConfigurationReport at: #DBF_SCRATCH_DIR)
%

category: 'origins'
method: SystemResolver
extent1

	^ self _extent: 1
%

category: 'origins'
method: SystemResolver
extent1Directory

	^ self extent1 parent
%

category: 'origins'
method: SystemResolver
gemLogDirectory
	"Answer the path to the gem log directory"

	^ self  resolveString: (System gemLogPath)
%

category: 'origins'
method: SystemResolver
image

	^ self dbfScratchDir
%

category: 'origins'
method: SystemResolver
imageDirectory

	^ self dbfScratchDir
%

category: 'origins'
method: SystemResolver
rowanProjectsHome

	"Answer the path to $ROWAN_PROJECTS_HOME"

	^ (System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME')
		ifNil: [ self error: '$ROWAN_PROJECTS_HOME not defined' ]
		ifNotNil: [:str | self  resolveString: str ]
%

category: 'resolving'
method: SystemResolver
supportedOrigins
	^ #(image imageDirectory extent1 extent1Directory tranlog dbfScratchDir workingDirectory rowanProjectsHome )
%

category: 'origins'
method: SystemResolver
tranlog
	"Answer the path to the current tranlog"

	^ self  resolveString: (System stoneConfigurationReport at: #StnCurrentTranLogNames)
%

category: 'private'
method: SystemResolver
_extent: extentIndex
	"Answer the path to the extent at extentIndex in DBF_EXTENT_NAMES: (System stoneConfigurationReport at: #DBF_EXTENT_NAMES) at: extentIndex"

	| extentNames extentName |
	extentNames := System stoneConfigurationReport at: #DBF_EXTENT_NAMES.
	extentNames _isArray
		ifTrue: [  
			extentIndex > extentNames size ifTrue: [  self error: 'Requested extent index (', extentIndex printString, ') exceeds number of extents present in system (', extentNames size printString, ')'  ].
			extentName := extentNames ]
		ifFalse: [ 
			extentIndex ~= 1 ifTrue: [  self error: 'Requested extent index (', extentIndex printString, ') exceeds number of extents present in system (1)'  ].
			extentName := extentNames ].
	^ self  resolveString: extentName
%

! Class implementation for 'FileSystemStore'

!		Class methods for 'FileSystemStore'

category: 'accessing'
classmethod: FileSystemStore
delimiter
	^ self shouldBeImplemented 
%

category: 'accessing'
classmethod: FileSystemStore
isCaseSensitive
	^ self shouldBeImplemented
%

category: 'instance creation'
classmethod: FileSystemStore
new

	^self basicNew
		initialize;
		yourself
%

category: 'accessing'
classmethod: FileSystemStore
separator
	self shouldBeImplemented
%

!		Instance methods for 'FileSystemStore'

category: 'public'
method: FileSystemStore
basenameFromEntry: aNode
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self subclassResponsibility
%

category: 'abstract'
method: FileSystemStore
basicCreationTimeOf: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicEntry: directoryEntry path: aPath nodesDo: aBlock
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicIsDirectory: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a directory.
	This private message should only be called form within the store."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicIsFile: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a file.
	This private message should only be called form within the store."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicIsSymlink: aNode
	^self subclassResponsibility 
	
%

category: 'abstract'
method: FileSystemStore
basicIsWritable: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicModificationTime: aNode
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self subclassResponsibility 
%

category: 'error signalling'
method: FileSystemStore
basicOpen: aPath writable: aBoolean
	"open the file at the given path and return an identifier"
	self subclassResponsibility
%

category: 'abstract'
method: FileSystemStore
basicPosixPermissions: aNode
	"Used to get the posix permissions from a low-level filesystem entry / node"
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
basicSizeOf: aNode
	"Used to get the size of the low-level representation (node / entry) "
	self subclassResponsibility 
%

category: 'public'
method: FileSystemStore
checkName: aString fixErrors: fixErrors
	^ self subclassResponsibility
%

category: 'abstract'
method: FileSystemStore
close
	"Some kinds of filesystems need to open connections to external resources"
%

category: 'private'
method: FileSystemStore
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem

	| buffer out in |
	
	in := nil.
	out := nil.
	buffer := nil.
	[
		in := aFilesystem binaryReadStreamOn: sourcePath.
		in ifNil: [ ^ absentBlock value ].
		
		(self exists: destinationPath)
			ifTrue: [ "cannot overwrite destination"
				^ presentBlock value ].
			
		out := aFilesystem binaryWriteStreamOn: destinationPath.
		buffer := ByteArray new: 1024.
		
		[ in atEnd ]
			whileFalse: [ 
				buffer := in nextInto: buffer.
				out nextPutAll: buffer ]]
	ensure: [ 
		in ifNotNil: [ in close ].
		out ifNotNil: [ out close ]]
%

category: 'abstract'
method: FileSystemStore
createDirectory: aPath
	self subclassResponsibility 
%

category: 'public'
method: FileSystemStore
creationTimeOf: aPath
	"Return the date of creation of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicCreationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	
%

category: 'accessing'
method: FileSystemStore
defaultWorkingDirectory
	^ Path root
%

category: 'abstract'
method: FileSystemStore
delete: aPath
	self subclassResponsibility 
%

category: 'accessing'
method: FileSystemStore
delimiter
	^ self class delimiter
%

category: 'public'
method: FileSystemStore
directoryAt: aPath ifAbsent: absentBlock directoryNodesDo: aBlock
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifTrue: [ aBlock value: entry ]].
%

category: 'public'
method: FileSystemStore
directoryAt: aPath ifAbsent: absentBlock fileNodesDo: aBlock
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifFalse: [ aBlock value: entry ]].
%

category: 'public'
method: FileSystemStore
directoryAt: aPath ifAbsent: absentBlock nodesDo: aBlock
	^ self
		nodeAt: aPath
		ifPresent: [ :entry | 
			(self basicIsDirectory: entry)
				ifTrue: [ self basicEntry: entry path: aPath nodesDo: aBlock ]
				ifFalse: [ DirectoryDoesNotExist signalWith: aPath ] ]
		ifAbsent: absentBlock
%

category: 'public'
method: FileSystemStore
ensureCreateDirectory: aPath
	(self isDirectory: aPath) ifTrue: [ ^ self ].
	self ensureCreateDirectory: aPath parent.
	self createDirectory: aPath
%

category: 'private'
method: FileSystemStore
entryFromNode: aNode fileSystem: aFilesystem path: aPath
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath
		creation: (self basicCreationTimeOf: aNode)
		modification:(self basicModificationTimeOf: aNode)
		isDir: (self basicIsDirectory: aNode)
		size: (self basicSizeOf: aNode)
		posixPermissions: (self basicPosixPermissions: aNode)
		isSymlink: (self basicIsSymlink: aNode)
%

category: 'public'
method: FileSystemStore
entryFromNode: node path: path for: aFileystem
	| entryPath |
	entryPath := path / (self basenameFromEntry: node).
	^ self entryFromNode: node fileSystem: aFileystem path: entryPath
%

category: 'public'
method: FileSystemStore
exists: aPath
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ true ] 
		ifAbsent: [ ^ false ].
	
%

category: 'private'
method: FileSystemStore
filename: aByteString matches: aByteString2
	^ aByteString = aByteString2
%

category: 'initialization'
method: FileSystemStore
initialize
%

category: 'accessing'
method: FileSystemStore
isCaseSensitive
	^ self class isCaseSensitive
%

category: 'public'
method: FileSystemStore
isDirectory: aPath
	aPath isRoot ifTrue: [ ^ true ].
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsDirectory: entry ] 
		ifAbsent: [ ^ false ].
	
%

category: 'public'
method: FileSystemStore
isFile: aPath
	"slow solution for big directories! "
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsFile: entry ] 
		ifAbsent: [ ^ false ]
	
%

category: 'public'
method: FileSystemStore
isSymlink: aPath
	aPath isRoot ifTrue: [ ^ true ].
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsSymlink: entry ] 
		ifAbsent: [ ^ false ].
	
%

category: 'public'
method: FileSystemStore
isWritable: aPath
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsWritable: entry ] 
		ifAbsent: [ ^ false ].
	
%

category: 'public'
method: FileSystemStore
modificationTimeOf: aPath
	"Returns the last date of modification of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicModificationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	
%

category: 'public'
method: FileSystemStore
nodeAt: aPath
	^ self 
		nodeAt: aPath 
		ifPresent: [ :node| node ]
		ifAbsent: [ self error: 'the node ', aPath printString, ' not found.' ]
%

category: 'abstract'
method: FileSystemStore
nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	self subclassResponsibility 
%

category: 'abstract'
method: FileSystemStore
open
	"Some kinds of filesystems need to open connections to external resources"
%

category: 'public'
method: FileSystemStore
openFileStream: path writable: writable
	self subclassResponsibility
%

category: 'converting'
method: FileSystemStore
pathFromString: aString
	"Use the unix convention by default, since many filesystems are based on it."
	
	^ Path from: aString delimiter: self delimiter
%

category: 'public'
method: FileSystemStore
permissions: aPath
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ FileSystemPermission posixPermissions: (self basicPosixPermissions: entry) ] 
		ifAbsent: [ ^ FileSystemPermission default ].
	
%

category: 'converting'
method: FileSystemStore
printPath: aPath on: out
	"Use the unix convention by default, since it's the most common."
	
	aPath isAbsolute ifTrue: [ out nextPut: self delimiter ].
	^ aPath printOn: out delimiter: self delimiter
%

category: 'private'
method: FileSystemStore
rename: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: anFSFilesystem

	| result |
	(self exists: destinationPath) ifTrue: [ ^ presentBlock value ].
	(self exists: sourcePath) ifFalse: [ ^ absentBlock value ].
	result := self rename: sourcePath to: destinationPath.
	result ifNil: [ self primitiveFailed ].
	^ self.
%

category: 'abstract'
method: FileSystemStore
rename: sourcePath to: destinationPath
	self subclassResponsibility 
%

category: 'accessing'
method: FileSystemStore
separator
	^ self class separator
%

category: 'error signalling'
method: FileSystemStore
signalDirectoryDoesNotExist: aPath
	^ DirectoryDoesNotExist signalWith: aPath
%

category: 'error signalling'
method: FileSystemStore
signalDirectoryExists: aPath
	^ DirectoryExists signalWith: aPath
%

category: 'error signalling'
method: FileSystemStore
signalFileDoesNotExist: aPath
	^ FileDoesNotExistException
		signalWithFile: (File named: aPath asPath pathString)
		writeMode: false
%

category: 'error signalling'
method: FileSystemStore
signalFileExists: aPath
	^ FileExists signalWith: aPath
%

category: 'public'
method: FileSystemStore
sizeOf: aPath
	"Return the size of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicSizeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ]
	
%

category: 'converting'
method: FileSystemStore
stringFromPath: aPath
	^ String streamContents: [ :out | 
		self printPath: aPath on: out ]
%

! Class implementation for 'DiskStore'

!		Class methods for 'DiskStore'

category: 'current'
classmethod: DiskStore
activeClass
	self allSubclasses do: [:ea | 
		ea isActiveClass ifTrue: [^ ea]].
  "Various methods go into infinite recursion if we return self."
	Error signal:'Cannot find an active subclass of DiskStore' .
%

category: 'current'
classmethod: DiskStore
createDefault
	^ self new
%

category: 'current'
classmethod: DiskStore
current
	^ self currentFileSystem store
%

category: 'current'
classmethod: DiskStore
currentFileSystem
	^ CurrentFS ifNil: [
		CurrentFS := FileSystem store: self activeClass createDefault]
%

category: 'accessing'
classmethod: DiskStore
defaultWorkingDirectory
	"Ask the VM for the default working directory.
	Clients should normally use the instance side method which caches this value."
	| pathString |

	pathString := (GsFile _expandEnvVariable: 'PWD' isClient:false).
	^(Path from: pathString delimiter: self delimiter).
%

category: 'public'
classmethod: DiskStore
delimiter
	^ self current delimiter
%

category: 'current'
classmethod: DiskStore
isActiveClass
	^ self delimiter = File delimiter
%

category: 'public'
classmethod: DiskStore
maxFileNameLength
	self subclassResponsibility 
%

category: 'current'
classmethod: DiskStore
reset
	DefaultWorkingDirectory := nil.
	CurrentFS := nil
%

category: 'system startup'
classmethod: DiskStore
shutDown: quitting
	"Force to detect filesystem after image restart"
	self reset
%

category: 'system startup'
classmethod: DiskStore
startUp: resuming
	resuming 
		ifTrue: [ self reset ].
	DefaultWorkingDirectory := self defaultWorkingDirectory.
%

!		Instance methods for 'DiskStore'

category: 'comparing'
method: DiskStore
= other
	^ self species = other species
%

category: 'private'
method: DiskStore
basenameFromEntry: entry
	^ entry at: 1
%

category: 'private'
method: DiskStore
basicCreationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"
	| dt |
	dt := DateAndTime posixSeconds:  (anEntry at: 2)  offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
%

category: 'private'
method: DiskStore
basicEntry: ignored path: aPath nodesDo: aBlock
	| pathString intOrArray |
	pathString := self stringFromPath: aPath.
	intOrArray := GsFile _contentsOfServerDirectory: pathString expandPath: true.
	intOrArray _isArray ifFalse: [ ^ self signalDirectoryDoesNotExist: aPath ].
	intOrArray
		do: [:entryPathString |
			((entryPathString endsWith: '.')  or: [ entryPathString endsWith: '..' ])
				ifFalse: [ | aFile |
          aFile := File lookupPath: entryPathString .
          "For now, ignore symLinks which reference a non-existant file."
          aFile ifNil:[ 
             (GsFile isSymbolicLink: entryPathString onClient: false) ifFalse:[
                self signalFileDoesNotExist: entryPathString 
             ]
          ] ifNotNil:[
            aBlock value: aFile 
          ]]]
%

category: 'public'
method: DiskStore
basicEntryAt: aPath
	| path basename |
	
	path := self stringFromPath: aPath parent.
	basename := aPath basename.
	
	^ (File lookupDirectory: path filename: basename)
		ifNil: [ #badDirectoryPath ].
%

category: 'private'
method: DiskStore
basicIsDirectory: anEntry
	^ anEntry at: 4
%

category: 'private'
method: DiskStore
basicIsFile: anEntry
	^ (anEntry at: 4) not
%

category: 'private'
method: DiskStore
basicIsSymlink: anEntry
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]
%

category: 'private'
method: DiskStore
basicModificationTimeOf: anEntry
	" the entry contains the seconds since the squeak epoch in local time"

	| dt |
	dt := DateAndTime posixSeconds:  (anEntry at: 3) offset: (Duration seconds: 0).
	dt offset: (Duration seconds: (dt currentTimeZone transitionAtUTC: dt) offsetFromUTC).
	^ dt
%

category: 'public'
method: DiskStore
basicOpen: aPath writable: writeMode

	"writeMode - #read, #append, #write"

	| string |
	string := self stringFromPath: aPath.
	^ File open: string writable: writeMode
%

category: 'private'
method: DiskStore
basicPosixPermissions: anEntry
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].
%

category: 'private'
method: DiskStore
basicSizeOf: anEntry
	^ (anEntry at: 5)
%

category: 'public'
method: DiskStore
checkName: aFileName fixErrors: fixErrors
	"Check a string aFileName for validity as a file name. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is just to truncate the name to the maximum length for this platform. Subclasses can do any kind of checking and correction appropriate for their platform."
	
	| maxLength |
	aFileName size = 0 ifTrue: [self error: 'zero length file name'].
	maxLength := self maxFileNameLength.
	aFileName size > maxLength ifTrue: [
		fixErrors
			ifTrue: [^ aFileName contractTo: maxLength]
			ifFalse: [self error: 'file name is too long']].
	^ aFileName
%

category: 'public'
method: DiskStore
createDirectory: path
	"Create a directory for the argument path. 
	If the path refers to an existing file, raise FileExists.
	If the path refers to an existing directory, raise DirectoryExists.
	If the parent directory of the path does not exist, raise DirectoryDoesNotExist"

	| parent pathString result |
	pathString := self stringFromPath: path.
	result := File createDirectory: pathString.
	result
		ifNil: [ 
			parent := path parent.
			(self exists: path)
				ifTrue: [ 
					(self isFile: path)
						ifTrue: [ self signalFileExists: path ]
						ifFalse: [ self signalDirectoryExists: path ] ].
			(self isDirectory: parent)
				ifFalse: [ ^ self signalDirectoryDoesNotExist: parent ].
			self primitiveFailed ].
	^ self
%

category: 'accessing'
method: DiskStore
defaultWorkingDirectory
	"Answer the default working directory, which is defined as the directory where the image resides."

	^ DefaultWorkingDirectory
		ifNil: [ self class defaultWorkingDirectory ]
%

category: 'public'
method: DiskStore
delete: path
	| pathString |
	
	(self exists: path)
		ifFalse: [ ^ FileDoesNotExistException signalWith: path ].
		
	pathString := self stringFromPath: path.
	
	(self isDirectory: path)
		ifTrue: [ File deleteDirectory: pathString ]
		ifFalse: [ 
			(File named: pathString) delete ]
%

category: 'printing'
method: DiskStore
forReferencePrintOn: aStream
	aStream nextPutAll: 'File @ '
%

category: 'accessing'
method: DiskStore
handleClass
	^ FileHandle
%

category: 'comparing'
method: DiskStore
hash
	^ self species hash
%

category: 'initialization'
method: DiskStore
initialize
	super initialize.
	maxFileNameLength := 255.
%

category: 'public'
method: DiskStore
isDirectory: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^ true ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsDirectory: entry ].
	
%

category: 'testing'
method: DiskStore
isDiskFileSystem
	^ true
%

category: 'public'
method: DiskStore
isFile: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^ false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsFile: entry ].
	
%

category: 'public'
method: DiskStore
isReadable: aPath
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: #read) 
		ifNotNil: [ :id|
			File close: id.
			^ true].
	^ false
%

category: 'public'
method: DiskStore
isSymlink: aPath
	| entry |
	
	aPath isRoot ifTrue: [ ^false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsSymlink: entry ].
	
%

category: 'public'
method: DiskStore
isWritable: aPath
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: #write) 
		ifNotNil: [ :id|
			File close: id.
			^ true].
	^ false
%

category: 'public'
method: DiskStore
maxFileNameLength
	^ maxFileNameLength
%

category: 'private'
method: DiskStore
nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	
	| entry |
	
	aPath isRoot ifTrue: [ ^ presentBlock value: self rootNode ].
	
	entry := self basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: absentBlock
		ifFalse: [
			entry at: 1 put: aPath basename.
			presentBlock value: entry ].
%

category: 'public'
method: DiskStore
openFileStream: path writable: writable
	| fullPath |
	fullPath := self stringFromPath: path.
	"redirect over the default implementation"
	^ writable 
		ifFalse: [ FileStreamPortable readOnlyFileNamed: fullPath ]
		ifTrue: [ FileStreamPortable fileNamed: fullPath ]
%

category: 'public'
method: DiskStore
rename: sourcePath to: destinationPath

	| sourcePathString targetPathString |
	sourcePathString := self stringFromPath: sourcePath.
	targetPathString := self stringFromPath: destinationPath.
	^ File rename: sourcePathString to: targetPathString.
%

category: 'private'
method: DiskStore
rootNode
	^ #('' 0 0 true 0 8r555)
%

! Class implementation for 'UnixStore'

!		Class methods for 'UnixStore'

category: 'public'
classmethod: UnixStore
delimiter
	^ $/
%

category: 'current'
classmethod: UnixStore
isActiveClass
  | osNam |
  osNam := System gemVersionAt: 'osName'.
  ^ (#( 'Linux' 'Darwin' 'SunOS' 'AIX') includes: osNam) and:[ super isActiveClass ]
%

category: 'public'
classmethod: UnixStore
isCaseSensitive

	^ true
%

category: 'public'
classmethod: UnixStore
maxFileNameLength

	^ 255
%

category: 'public'
classmethod: UnixStore
separator 
	^ $:
%

!		Instance methods for 'UnixStore'

category: 'public'
method: UnixStore
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	| fName |
	fName := super checkName: aFileName fixErrors: fixing.
	
	(fName includes: self delimiter) ifFalse:
		[^fName].
	
	fixing ifFalse: [self error:'Invalid file name'].
	
	^ fName copyReplaceAll: self delimiter asString with: '#'
%

! Class implementation for 'MacStore'

!		Class methods for 'MacStore'

category: 'current'
classmethod: MacStore
isActiveClass
	^ ((System gemVersionAt: 'osName') = 'Darwin') and: [ super isActiveClass ]
%

category: 'public'
classmethod: MacStore
isCaseSensitive

	^ true
%

! Class implementation for 'WindowsStore'

!		Class methods for 'WindowsStore'

category: 'accessing'
classmethod: WindowsStore
delimiter
	^ $\
%

category: 'accessing'
classmethod: WindowsStore
isCaseSensitive
	^ false
%

category: 'accessing'
classmethod: WindowsStore
maxFileNameLength
	self flag: 'TODO: more tests needed here!'.
	^ 255
%

category: 'accessing'
classmethod: WindowsStore
separator 
	^ $\
%

!		Instance methods for 'WindowsStore'

category: 'public'
method: WindowsStore
checkName: aFileName fixErrors: fixing
	"Check if the file name contains any invalid characters"
	
	| fName badChars |
	fName := super checkName: aFileName fixErrors: fixing.
	badChars := (#( $: $< $> $| $/ $\ $? $* $"), ((0 to: 31) collect: [:n | n asCharacter])) asSet.
	
	(fName includesAnyOf: badChars)
		ifFalse: [^ fName].
	
	fixing ifFalse: [^self error: 'filename contains invalid characters'].
	
	^ fName collect:
		[:char | (badChars includes: char) 
				ifTrue: [$#] 
				ifFalse: [char]]
%

category: 'converting'
method: WindowsStore
currentDisk
	^ disk ifNil: [  disk := FileSystem workingDirectory path segments first ]
%

category: 'converting'
method: WindowsStore
pathFromString: aString
	"Need to distinguish '' and '/' , so tag latter with invalid character ':'  "
	| normalized pathClass pathElements |
	normalized := aString copy replaceAll: UnixStore delimiter with: self delimiter.
	pathElements := self delimiter split: normalized.
	pathClass := (Path isAbsoluteWindowsPath: normalized)
		ifTrue: [ (normalized = self delimiter asString) ifTrue: [ pathElements := { ':' } ].
			AbsolutePath ]
		ifFalse: [ self stripDrive: pathElements.
			RelativePath ].
	^pathClass withAll: pathElements
%

category: 'converting'
method: WindowsStore
printPath: aPath on: aStream
	| hasDrive |
	aPath isRoot
		ifTrue: [ ^ self ].	"effectively Windows root is empty string"
	aPath isWorkingDirectory
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath isRelative
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath segments first first = $:
		ifTrue: [ ^ aStream nextPut: self delimiter ].	"as tagged in #pathFromString:  "
	hasDrive := aPath segments first second = $:.
	(hasDrive not )
		ifTrue: [ aStream nextPut: self delimiter ].
	aPath printOn: aStream delimiter: self delimiter.
	(hasDrive and: [ aPath segments size = 1 ])
		ifTrue: [ aStream nextPut: self delimiter ]
%

category: 'converting'
method: WindowsStore
stripDrive: pathElements
	pathElements ifNotEmpty: [ pathElements at: 1 put: ( ($: split: pathElements first) last)  ]
%

! Class implementation for 'MemoryStore'

!		Class methods for 'MemoryStore'

category: 'current'
classmethod: MemoryStore
currentFileSystem
	^ CurrentFS ifNil: [ CurrentFS := FileSystem store: MemoryStore new ]
%

category: 'public'
classmethod: MemoryStore
delimiter
	^ $/
%

category: 'public'
classmethod: MemoryStore
isCaseSensitive
	^ true
%

category: 'class initialization'
classmethod: MemoryStore
reset
	CurrentFS := nil
%

category: 'public'
classmethod: MemoryStore
separator 
	^ $:
%

category: 'system startup'
classmethod: MemoryStore
startUp
	self reset
%

!		Instance methods for 'MemoryStore'

category: 'public'
method: MemoryStore
basenameFromEntry: aMemoryFileSystemEntry
	^ aMemoryFileSystemEntry basename
%

category: 'private'
method: MemoryStore
basicCreationTimeOf: aMemoryFileSystemEntry
	"Returns the creation date of aMemoryFileSystemEntry"
	^ aMemoryFileSystemEntry creationTime
%

category: 'private'
method: MemoryStore
basicEntry: entry nodesDo: aBlock
	entry fileEntriesDo: aBlock
%

category: 'private'
method: MemoryStore
basicEntry: directoryEntry path: aPath nodesDo: aBlock
	directoryEntry fileEntriesDo: aBlock
%

category: 'private'
method: MemoryStore
basicIsDirectory: aMemoryFileSystemEntry
	^ aMemoryFileSystemEntry isDirectory
%

category: 'private'
method: MemoryStore
basicIsFile: aMemoryFileSystemEntry
	^ aMemoryFileSystemEntry isFile
%

category: 'private'
method: MemoryStore
basicIsSymlink: aNode
	^false
%

category: 'private'
method: MemoryStore
basicModificationTimeOf: aMemoryFileSystemEntry
	"Return the basic modification time of aMemoryFileSystemEntry"
	^ aMemoryFileSystemEntry modificationTime
%

category: 'private'
method: MemoryStore
basicOpen: path writable: writeMode
	^ self
		nodeAt: path
		ifPresent: [ :aMemoryFileSystemEntry | 
			aMemoryFileSystemEntry
				basicOpen;
				yourself ]
		ifAbsent: [ writeMode == #write
				ifFalse: [ self signalFileDoesNotExist: path ]
				ifTrue: [ self createFile: path ] ]
%

category: 'private'
method: MemoryStore
basicPosixPermissions: anEntry
	^ 8r777
%

category: 'private'
method: MemoryStore
basicSizeOf: aMemoryFileSystemEntry
	"Return the basic size of aMemoryFileSystemEntry"
	^ aMemoryFileSystemEntry fileSize
%

category: 'public'
method: MemoryStore
checkName: aString fixErrors: fixErrors
	aString ifEmpty: [ self error: 'zero length file name' ].
	^ aString
%

category: 'private'
method: MemoryStore
copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem
        | sourceNode destinationNode |

        sourceNode := self
                nodeAt: sourcePath
                ifPresent: [ :source | source ]
                ifAbsent: [ ^ absentBlock value].

        sourceNode isDirectory
                ifTrue: [ ^ absentBlock value ].

        destinationNode := self
                nodeAt: destinationPath parent
                ifPresent: [ :destination |  destination ]
                ifAbsent: [ ^ self signalDirectoryDoesNotExist: destinationPath parent ].

        destinationNode isFile
                ifTrue: [ self signalDirectoryDoesNotExist: destinationPath parent ].

        (destinationNode fileEntriesIncludes: destinationPath basename)
                ifTrue: [ "cannot overwrite existing file"^ presentBlock value ].

        destinationNode
                fileEntryAt: destinationPath basename
                put: (sourceNode copy
                                        basename: destinationPath basename;
                                        yourself)
%

category: 'public'
method: MemoryStore
createDirectory: path
	| parent |
	parent := path parent.
	^ self
		nodeAt: parent
		ifPresent: [ :entry | 
			entry
				fileEntryAt: path basename
				ifPresent: [ :node | 
					node isDirectory
						ifTrue: [ self signalDirectoryExists: path ]
						ifFalse: [ self signalFileExists: path ] ].
			entry ensureCreateDirectory: path basename  ]
		ifAbsent: [ self signalDirectoryDoesNotExist: parent ]
%

category: 'private'
method: MemoryStore
createFile: aPath
	^ self
		nodeAt: aPath parent
		ifPresent: [ :entry | 
			entry isDirectory
				ifTrue: [ entry ensureCreateFile: aPath basename ]]
		ifAbsent: [ self signalDirectoryDoesNotExist: aPath parent ]
%

category: 'public'
method: MemoryStore
delete: path
	self 
		nodeAt: path parent
		ifPresent: [ :dict | 
			dict fileEntryRemove: path basename ifAbsent: [ FileDoesNotExistException signalWith: path ]] 
		ifAbsent: [ DirectoryDoesNotExist signalWith: path parent ]
%

category: 'printing'
method: MemoryStore
forReferencePrintOn: aStream
	aStream nextPutAll: 'memory://'
%

category: 'accessing'
method: MemoryStore
handleClass
	^ MemoryHandle 
%

category: 'initialization'
method: MemoryStore
initialize 
	root := MemoryFileSystemDirectory new
%

category: 'testing'
method: MemoryStore
isMemoryFileSystem
	^ true
%

category: 'private'
method: MemoryStore
nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	| current |
	aPath isRoot
		ifTrue: [ ^ presentBlock value: self root ].
	current := self root.
	aPath
		do: [ :segment | 
			current isDirectory
				ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ absentBlock value ] ]
				ifFalse: [ ^ absentBlock value ] ].
	^ presentBlock value: current
%

category: 'public'
method: MemoryStore
openFileStream: path writable: writeMode

	"writeMode - #read, #append, #write"

	| entry |
	entry := self basicOpen: path writable: writeMode.
	^ writeMode == #write
		ifTrue: [ entry writeStream ]
		ifFalse: [ entry readStreamPortable ]
%

category: 'public'
method: MemoryStore
rename: sourcePath to: destinationPath
	| sourceEntry destinationParentEntry newName |
	
	sourceEntry := self nodeAt: sourcePath.
	newName := destinationPath basename.
	
	destinationParentEntry := self nodeAt: destinationPath parent.
	
	destinationParentEntry isDirectory
		ifFalse: [ Error signal: 'Copy destination has to be a directory' ].
	destinationParentEntry 
		fileEntryAt: newName
		ifPresent: [ Error signal: 'Destination file exists already' ].
		
	destinationParentEntry 
		fileEntryAt: newName
		put: sourceEntry.
	sourceEntry basename: newName.
	
	
	(self nodeAt: sourcePath parent)
		fileEntryRemove: sourcePath basename 
	
%

category: 'private'
method: MemoryStore
replaceFile: path in: aBlock
	^ self
		nodeAt: path parent
		ifPresent: [ :entry | | old new |
			entry isDirectory
				ifFalse: [ self signalFileDoesNotExist: path ].
			old := entry fileEntryAt: path basename ifAbsent: [ self signalFileDoesNotExist: path ].
			new := aBlock value: old.
			entry fileEntryAt: path basename put: new ]
		ifAbsent: [ self signalFileDoesNotExist: path ]
%

category: 'accessing'
method: MemoryStore
root
	^ root
%

! Class implementation for 'FileSystemVisitor'

!		Instance methods for 'FileSystemVisitor'

category: 'initialization'
method: FileSystemVisitor
initialize
%

category: 'visiting'
method: FileSystemVisitor
visitDirectory: aReference
	^ self visitReference: aReference
%

category: 'visiting'
method: FileSystemVisitor
visitFile: aReference
	^ self visitReference: aReference
%

category: 'visiting'
method: FileSystemVisitor
visitReference: aReference
%

! Class implementation for 'AbstractEnumerationVisitor'

!		Instance methods for 'AbstractEnumerationVisitor'

category: 'visiting'
method: AbstractEnumerationVisitor
breadthFirst: aReference
	^ self visit: aReference with: (BreadthFirstGuide for: self)
%

category: 'initialization'
method: AbstractEnumerationVisitor
initializeWithBlock: aBlock
	self initialize.
	block := aBlock
%

category: 'visiting'
method: AbstractEnumerationVisitor
postorder: aReference
	^ self visit: aReference with: (PostorderGuide for: self)
%

category: 'visiting'
method: AbstractEnumerationVisitor
preorder: aReference
	^ self visit: aReference with: (PreorderGuide for: self)
%

category: 'visiting'
method: AbstractEnumerationVisitor
visit: aReference with: aGuide
	out := (Array new: 10) writeStreamPortable.
	aGuide show: aReference.
	^ out contents
%

category: 'visiting'
method: AbstractEnumerationVisitor
visitReference: anEntry
	self subclassResponsibility
%

! Class implementation for 'CollectVisitor'

!		Class methods for 'CollectVisitor'

category: 'instance creation'
classmethod: CollectVisitor
breadthFirst: aReference
	^ self breadthFirst: aReference collect: [:entry | entry]
%

category: 'instance creation'
classmethod: CollectVisitor
breadthFirst: aReference collect: aBlock
	^ (self collect: aBlock) breadthFirst: aReference
%

category: 'instance creation'
classmethod: CollectVisitor
collect: aBlock
	^ self basicNew initializeWithBlock: aBlock
%

category: 'instance creation'
classmethod: CollectVisitor
postorder: aReference
	^ self postorder: aReference collect: [:entry | entry]
%

category: 'instance creation'
classmethod: CollectVisitor
postorder: aReference collect: aBlock
	^ (self collect: aBlock) postorder: aReference
%

category: 'instance creation'
classmethod: CollectVisitor
preorder: aReference
	^ self preorder: aReference collect: [:entry | entry]
%

category: 'instance creation'
classmethod: CollectVisitor
preorder: aReference collect: aBlock
	^ (self collect: aBlock) preorder: aReference
%

!		Instance methods for 'CollectVisitor'

category: 'visiting'
method: CollectVisitor
visitReference: anEntry
	out nextPut: (block value: anEntry)
%

! Class implementation for 'SelectVisitor'

!		Class methods for 'SelectVisitor'

category: 'instance creation'
classmethod: SelectVisitor
breadthFirst: aReference
	^ self breadthFirst: aReference select: [:entry | true]
%

category: 'instance creation'
classmethod: SelectVisitor
breadthFirst: aReference select: aBlock
	^ (self select: aBlock) breadthFirst: aReference
%

category: 'instance creation'
classmethod: SelectVisitor
postorder: aReference
	^ self postorder: aReference select: [:entry | true]
%

category: 'instance creation'
classmethod: SelectVisitor
postorder: aReference select: aBlock
	^ (self select: aBlock) postorder: aReference
%

category: 'instance creation'
classmethod: SelectVisitor
preorder: aReference
	^ self preorder: aReference select: [:entry | true]
%

category: 'instance creation'
classmethod: SelectVisitor
preorder: aReference select: aBlock
	^ (self select: aBlock) preorder: aReference
%

category: 'instance creation'
classmethod: SelectVisitor
select: aBlock
	^ self basicNew initializeWithBlock: aBlock
%

!		Instance methods for 'SelectVisitor'

category: 'visiting'
method: SelectVisitor
visitReference: anEntry
	(block value: anEntry) 
		ifTrue: [ out nextPut: anEntry ]
%

! Class implementation for 'CopyVisitor'

!		Class methods for 'CopyVisitor'

category: 'instance creation'
classmethod: CopyVisitor
copy: source to: dest
	(self from: source to: dest) visit
%

category: 'instance creation'
classmethod: CopyVisitor
from: srcReference to: dstReference
	^ self basicNew
		initializeWithSource: srcReference 
		dest: dstReference
%

!		Instance methods for 'CopyVisitor'

category: 'visiting'
method: CopyVisitor
copyDirectory: aReference
	| directory |
	directory := dest resolve: (aReference relativeTo: source).
	directory createDirectory
%

category: 'visiting'
method: CopyVisitor
copyFile: aReference
	| copy |
	copy := dest resolve: (aReference relativeTo: source).
	aReference copyTo: copy
%

category: 'initialize-release'
method: CopyVisitor
initializeWithSource: srcReference dest: dstReference
	self initialize.
	source := srcReference.
	dest := dstReference
%

category: 'visiting'
method: CopyVisitor
visit
	(PreorderGuide for: self) 
		show: source
%

category: 'visiting'
method: CopyVisitor
visitDirectory: anEntry
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [dest ensureCreateDirectory]
		ifFalse: [self copyDirectory: reference]
%

category: 'visiting'
method: CopyVisitor
visitFile: anEntry
	| reference |
	reference := anEntry reference.
	reference = source
		ifTrue: [source copyTo: dest]
		ifFalse: [self copyFile: reference]
%

! Class implementation for 'DeleteVisitor'

!		Class methods for 'DeleteVisitor'

category: 'instance creation'
classmethod: DeleteVisitor
delete: aReference
	^ self new visit: aReference
%

!		Instance methods for 'DeleteVisitor'

category: 'visiting'
method: DeleteVisitor
visit: aReference
	PostorderGuide 
		show: aReference 
		to: self 
		selecting: [ :entry | entry isSymlink not ]
%

category: 'visiting'
method: DeleteVisitor
visitReference: anEntry
	anEntry reference delete
%

! Class implementation for 'GsTonelOrderedDictionary'

!		Class methods for 'GsTonelOrderedDictionary'

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new
	^ self new: 3
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new: anInteger
	^ self basicNew initialize: anInteger; yourself
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
withAll: aDictionary
	^ (self new: aDictionary size)
		addAll: aDictionary;
		yourself
%

!		Instance methods for 'GsTonelOrderedDictionary'

category: 'accessing'
method: GsTonelOrderedDictionary
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
%

category: 'adding'
method: GsTonelOrderedDictionary
addAll: aDictionary
	aDictionary keysAndValuesDo: [ :key :value | self at: key put: value ].
	^ aDictionary
%

category: 'accessing'
method: GsTonelOrderedDictionary
associations
	"Answer a Collection containing the receiver's associations."

	| result |
	result := WriteStream on: (Array new: self size).
	self associationsDo: [ :assoc | result nextPut: assoc ].
	^ result contents
%

category: 'enumerating'
method: GsTonelOrderedDictionary
associationsDo: aBlock
	self keysAndValuesDo: [ :key :value | aBlock value: key -> value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey
	"Answer the value associated with aKey. Raise an exception, if no such key is defined."

	^ self at: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsent: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsentPut: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined and store the return value."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ self privateAt: aKey put: aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifPresent: aBlock
	"Lookup aKey in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0 ifFalse: [ aBlock value: (values at: index) ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey put: aValue
	"Set the value of aKey to be aValue."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index put: aValue ]
		ifTrue: [ self privateAt: aKey put: aValue ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
do: aBlock
	1 to: size do: [ :index | aBlock value: (values at: index) ]
%

category: 'private'
method: GsTonelOrderedDictionary
errorKeyNotFound
	self error: 'Key not found'
%

category: 'private'
method: GsTonelOrderedDictionary
findIndexFor: aKey
	1 to: size do: [ :index |
		(keys at: index) = aKey
			ifTrue: [ ^ index ] ].
	^ 0
%

category: 'private'
method: GsTonelOrderedDictionary
grow
	| newKeys newValues |
	newKeys := Array new: 2 * size.
	newValues := Array new: 2 * size.
	1 to: size do: [ :index |
		newKeys at: index put: (keys at: index).
		newValues at: index put: (values at: index) ].
	keys := newKeys.
	values := newValues
%

category: 'testing'
method: GsTonelOrderedDictionary
includesKey: aKey
	"Answer whether the receiver has a key equal to aKey."

	^ (self findIndexFor: aKey) ~= 0
%

category: 'initialization'
method: GsTonelOrderedDictionary
initialize: anInteger
  size := 0.
  keys := Array new: anInteger.
  values := Array new: anInteger
%

category: 'testing'
method: GsTonelOrderedDictionary
isCollection
	^ true
%

category: 'testing'
method: GsTonelOrderedDictionary
isEmpty
	^ size = 0
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keys
	^ keys copyFrom: 1 to: size
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysAndValuesDo: aBlock
	1 to: size do: [ :index | aBlock value: (keys at: index) value: (values at: index) ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysDo: aBlock
	1 to: size do: [ :each | aBlock value: (keys at: each) ]
%

category: 'copying'
method: GsTonelOrderedDictionary
postCopy
	super postCopy.
	keys := keys copy.
	values := values copy
%

category: 'printing'
method: GsTonelOrderedDictionary
printOn: aStream
	super printOn: aStream.
	
	aStream nextPut: $(.
	self size <= 100
		ifTrue: [
			| first |
			first := true.
			self keysAndValuesDo: [ :key :value |
				"keysAndValuesDo:separatedBy: would be nice"
				first
					ifTrue: [ first := false ]
					ifFalse: [ aStream space ].
				aStream
					print: key;
					nextPutAll: '->';				
					print: value ] ]
		ifFalse: [
			aStream
				nextPutAll: 'size ';
				print: self size ].
	aStream nextPut: $)	
%

category: 'private'
method: GsTonelOrderedDictionary
privateAt: aKey put: aValue
	size = keys size ifTrue: [ self grow ].
	keys at: (size := size + 1) put: aKey.
	^ values at: size put: aValue
%

category: 'private'
method: GsTonelOrderedDictionary
removeIndex: index
	| value |
	value := values at: index.
	index to: size - 1 do:
			[ :i | 
			keys at: i put: (keys at: i + 1).
			values at: i put: (values at: i + 1) ].
	keys at: size put: nil.
	values at: size put: nil.
	size := size - 1.
	^ value
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey
	"Remove aKey from the receiver, raise an exception if the element is missing."

	^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey ifAbsent: aBlock
	"Remove aKey from the receiver, evaluate aBlock if the element is missing."

	| index |
	index := self findIndexFor: aKey.
	index = 0 ifTrue: [ ^ aBlock value ].
	^ self removeIndex: index
%

category: 'accessing'
method: GsTonelOrderedDictionary
size
	^ size
%

category: 'ston'
method: GsTonelOrderedDictionary
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
    stonWriter encodeMap: self
%

category: 'enumerating'
method: GsTonelOrderedDictionary
values
	^ values copyFrom: 1 to: size
%

category: 'filetree'
method: GsTonelOrderedDictionary
_writeCypressJsonOn: fileStream
	"Private method which may be removed in a future GemStone version."

	self _writeCypressJsonOn: fileStream indent: 0
%

category: 'filetree'
method: GsTonelOrderedDictionary
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	| indent cnt |
	indent := startIndent.
	aStream
		nextPutAll: '{';
		lf.
	cnt := 0.
	indent := indent + 1.
	self keys do: 
			[:key |
			| value |
			value := self at: key.
			cnt := cnt + 1.
			indent timesRepeat: [aStream tab].
			key _writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value _writeCypressJsonOn: aStream indent: indent.
			cnt < size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	size = 0 ifTrue: [indent timesRepeat: [aStream tab]].
	aStream nextPutAll: ' }'
%

! Class implementation for 'MemoryFileSystemEntry'

!		Class methods for 'MemoryFileSystemEntry'

category: 'instance creation'
classmethod: MemoryFileSystemEntry
named: aFileName
	^ self new
		basename: aFileName;
		yourself
%

category: 'instance creation'
classmethod: MemoryFileSystemEntry
new

	^ self basicNew
		initialize;
		yourself
%

!		Instance methods for 'MemoryFileSystemEntry'

category: 'accessing'
method: MemoryFileSystemEntry
basename
	^ basename
%

category: 'accessing'
method: MemoryFileSystemEntry
basename: aString
	basename := aString
%

category: 'accessing'
method: MemoryFileSystemEntry
creationTime
	
	^ creationTime
%

category: 'accessing'
method: MemoryFileSystemEntry
fileSize
	self subclassResponsibility 
%

category: 'initialization'
method: MemoryFileSystemEntry
initialize 
	creationTime := modificationTime := DateAndTime now.
%

category: 'testing'
method: MemoryFileSystemEntry
isDirectory
	self subclassResponsibility
%

category: 'testing'
method: MemoryFileSystemEntry
isFile
	^ self isDirectory not
%

category: 'accessing'
method: MemoryFileSystemEntry
modificationTime
	^ modificationTime
%

category: 'accessing'
method: MemoryFileSystemEntry
modificationTime: anObject
	
	modificationTime := anObject
%

category: 'private'
method: MemoryFileSystemEntry
modified
	modificationTime := DateAndTime now.
%

! Class implementation for 'MemoryFileSystemDirectory'

!		Instance methods for 'MemoryFileSystemDirectory'

category: 'creation'
method: MemoryFileSystemDirectory
ensureCreateDirectory: aDirectoryName
	^ self 
		fileEntryAt: aDirectoryName
		put: (self class named: aDirectoryName)
%

category: 'creation'
method: MemoryFileSystemDirectory
ensureCreateFile: aFileName
	^ self 
		fileEntryAt: aFileName 
		put: (MemoryFileSystemFile named: aFileName)
%

category: 'accessing'
method: MemoryFileSystemDirectory
entries
	^ entries
%

category: 'enumeration'
method: MemoryFileSystemDirectory
fileEntriesDo: aBlock
	
	entries keys sort do: [ :fileName|
		aBlock value: (entries at: fileName)].
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntriesIncludes: aFileName
	^ entries includesKey: aFileName
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName
	^ entries at: aFileName
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName ifAbsent: aBlock
	^ entries at: aFileName ifAbsent: aBlock
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName ifPresent: aBlock
	^ (entries at: aFileName ifAbsent: [])
			ifNotNil: [:entry | aBlock value: entry ]
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryAt: aFileName put: anEntry
	^ entries 
		at: aFileName 
		ifAbsentPut: [
			self modified.
			anEntry ]
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryRemove: aFileName
	^ self fileEntryRemove: aFileName ifAbsent: [ FileDoesNotExistException signalWith: aFileName ]
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileEntryRemove: aFileName ifAbsent: absentBlock
	| deletedEntry |
	deletedEntry := entries removeKey: aFileName ifAbsent: [ ^ absentBlock value ].
	modificationTime := DateAndTime now.
	^ deletedEntry
%

category: 'accessing'
method: MemoryFileSystemDirectory
fileSize
	^ 0
%

category: 'initialization'
method: MemoryFileSystemDirectory
initialize
	super initialize.
	entries := Dictionary new.
%

category: 'testing'
method: MemoryFileSystemDirectory
isDirectory
	^ true
%

! Class implementation for 'MemoryFileSystemFile'

!		Instance methods for 'MemoryFileSystemFile'

category: 'stream-protocol'
method: MemoryFileSystemFile
at: index
	^ bytes at: index
%

category: 'stream-protocol'
method: MemoryFileSystemFile
at: index put: anObject
	index > bytes size
		ifTrue: [ self grow ].
	bytes
		at: index
		put:
			(anObject isCharacter
				ifTrue: [ anObject codePoint ]
				ifFalse: [ anObject ]).
	size := size max: index.
	self modified
%

category: 'stream-protocol'
method: MemoryFileSystemFile
at: index read: aCollection startingAt: start count: count 
	| max stop |
	max := size - index + 1 min: count.
	stop := start + max - 1.
	aCollection 
		replaceFrom: start
		to: stop
		with: bytes
		startingAt: index.
	^ stop - start + 1
%

category: 'stream-protocol'
method: MemoryFileSystemFile
at: first write: aCollection startingAt: start count: count
	| last |
	last := first + count - 1.
	last > bytes size
		ifTrue: [ self grownBy: last - size ].
	bytes
		replaceFrom: first
		to: last
		with: aCollection
		startingAt: start.
	size := last.
	self modified
%

category: 'open/close'
method: MemoryFileSystemFile
basicOpen
	closed := false
%

category: 'streams-compatibility'
method: MemoryFileSystemFile
binaryReadStream
	^ ReadStreamPortable on: self bytes from: 1 to: size
%

category: 'streams'
method: MemoryFileSystemFile
binaryWriteStream
	^ MemoryFileWriteStream on: self
%

category: 'accessing'
method: MemoryFileSystemFile
bytes
	^ bytes
%

category: 'open/close'
method: MemoryFileSystemFile
close
	 closed := true
%

category: 'testing'
method: MemoryFileSystemFile
closed
	^ closed
%

category: 'stream-protocol'
method: MemoryFileSystemFile
copyFrom: from to: position
	^ bytes copyFrom: from to: position
%

category: 'accessing'
method: MemoryFileSystemFile
fileSize
	^ size
%

category: 'stream-protocol'
method: MemoryFileSystemFile
grow
	self grownBy: self sizeIncrement
%

category: 'accessing'
method: MemoryFileSystemFile
grownBy: length
	bytes := bytes grownBy: length.
	self modified
%

category: 'initialization'
method: MemoryFileSystemFile
initialize
	super initialize.
	bytes := ByteArray new.
	size := 0.
	closed := false
%

category: 'accessing'
method: MemoryFileSystemFile
internalSize
	^ bytes size
%

category: 'testing'
method: MemoryFileSystemFile
isDirectory
	^ false
%

category: 'stream-protocol'
method: MemoryFileSystemFile
readStream
	^ ReadStream on: self bytes asString from: 1 to: size
%

category: 'accessing'
method: MemoryFileSystemFile
size

	^ size
%

category: 'accessing'
method: MemoryFileSystemFile
sizeIncrement
	^ (bytes size min: 20) max: 1024
%

category: 'accessing'
method: MemoryFileSystemFile
truncate
	self truncateTo: size
%

category: 'accessing'
method: MemoryFileSystemFile
truncateTo: aSize
	bytes size = aSize
		ifFalse: [ bytes := bytes size < aSize
				ifTrue: [ (ByteArray new: aSize)
						replaceFrom: 1
						to: bytes size
						with: bytes
						startingAt: 1 ]
				ifFalse: [ bytes copyFrom: 1 to: aSize ] ].
	size := bytes size.
	self modified
%

category: 'private'
method: MemoryFileSystemFile
updateContents: aCollection
	bytes := aCollection.
	self updateSize: aCollection size
%

category: 'private'
method: MemoryFileSystemFile
updateSize: newSize
	size := newSize.
	self modified
%

category: 'stream-protocol'
method: MemoryFileSystemFile
writeStreamDo: aBlock
	ByteArray
		streamContents: [ :aStream | 
			aBlock value: aStream.
			bytes := aStream contents ].
	self updateSize: bytes size
%

! Class implementation for 'MemoryFileWriteStream'

!		Class methods for 'MemoryFileWriteStream'

category: 'instance creation'
classmethod: MemoryFileWriteStream
on: aFile

	^ self new
		file: aFile;
		yourself
%

!		Instance methods for 'MemoryFileWriteStream'

category: 'opening-closing'
method: MemoryFileWriteStream
close
	self stream close.
	file close
%

category: 'opening-closing'
method: MemoryFileWriteStream
closed
	^ file closed
%

category: 'accessing'
method: MemoryFileWriteStream
file: aMemoryFileSystemFile 
	file := aMemoryFileSystemFile
%

category: 'writing'
method: MemoryFileWriteStream
flush
	file updateContents: self stream contents
%

category: 'testing'
method: MemoryFileWriteStream
isBinary
	^ self stream isBinary
%

category: 'writing'
method: MemoryFileWriteStream
nextPut: aCollection
	^ self stream nextPut: aCollection
%

category: 'writing'
method: MemoryFileWriteStream
nextPutAll: aCollection
	^ self stream nextPutAll: aCollection
%

category: 'positioning'
method: MemoryFileWriteStream
position
	^ self stream position
%

category: 'positioning'
method: MemoryFileWriteStream
setToEnd
	^ self stream setToEnd
%

category: 'accessing'
method: MemoryFileWriteStream
size
	^ file size
%

category: 'accessing'
method: MemoryFileWriteStream
stream
	^ stream ifNil: [ stream := WriteStreamPortable on: file bytes from: 1 to: file size ]
%

! Class implementation for 'Path'

!		Class methods for 'Path'

category: 'instance creation'
classmethod: Path
* aString
	"Answer a relative path with aString as its sole segment. For example,
	Path * 'griffle' will produce the same result as ./griffle in a unix shell.
	The selector #* was chosen for it's visual similarity to $."
	
	"Note: aString is not parsed, so supplying a string like '/griffle/plonk'
	will not create an absolute path."
	
	^ RelativePath with: aString
%

category: 'instance creation'
classmethod: Path
/ aString
	"Answer an absolute path with aString as it's sole segment. The selector
	was chosen to allow path construction with Smalltalk syntax, which 
	neverthelesss resembles paths as they appear in a unix shell. Eg.
	Path / 'griffle' / 'plonk'."
	
	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].
	^ AbsolutePath with: aString
%

category: 'private'
classmethod: Path
addElement: element to: result
	element = '..'
		ifTrue: [ ^ self addParentElementTo: result ].
	element = ''
		ifTrue: [ ^ self addEmptyElementTo: result ].
	element = '.'
		ifFalse: [ result add: element ]
%

category: 'private'
classmethod: Path
addEmptyElementTo: result
	result isEmpty ifTrue: [result add: '']
		
%

category: 'private'
classmethod: Path
addParentElementTo: result
	(result isEmpty or: [ result last = '..' ])
		ifTrue: [ result add: '..' ]
		ifFalse: [ result removeLast ]
		
%

category: 'private'
classmethod: Path
canonicalizeElements: aCollection
	| result |
	result := OrderedCollection new.
	aCollection do: [ :element |
		self addElement: element to: result].
	^ result
%

category: 'encodings'
classmethod: Path
delimeter
	"Internal path delimeter"
	
	^$/
%

category: 'encodings'
classmethod: Path
extensionDelimiter
	"Return the extension delimiter character."
	^ $.
%

category: 'instance creation'
classmethod: Path
from: aString
	"Answer an instance of the receiver with the supplied path using the default delimiter"

	^ self from: aString delimiter: self delimeter
%

category: 'instance creation'
classmethod: Path
from: aString delimiter: aDelimiterCharacter 
	"Answer a path composed of several elements delimited by aCharacter"
	| pathClass splitPathElements |
	aString isEmpty
		ifTrue: [ ^ self root ].

	aString first = $$
		ifTrue: [
			| pathElements envVarString envVarElement eVar |
			"GemStone paths are allowed to start with an environment variable"
			pathElements := aDelimiterCharacter split: aString.
			envVarElement := (pathElements at: 1) .
			envVarString := System gemEnvironmentVariable: (eVar := envVarElement copyFrom: 2 to: envVarElement size). 
      envVarString ifNil:[ Error signal:'environment variable ' , eVar ,' not defined']. 
      envVarString := envVarString decodeFromUTF8 asString .
			pathClass :=  ((self isAbsolutePath: envVarString delimiter: aDelimiterCharacter) or: 
									[self isAbsoluteWindowsPath: envVarString]) 
				ifTrue: [ AbsolutePath ]
				ifFalse:[ RelativePath ].
			splitPathElements :=  (aDelimiterCharacter split: envVarString) , (pathElements copyFrom: 2 to: pathElements size) ]
		ifFalse: [ 	
			pathClass :=  ((self isAbsolutePath: aString delimiter: aDelimiterCharacter) or: 
									[self isAbsoluteWindowsPath: aString]) 
				ifTrue: [ AbsolutePath ]
				ifFalse:[ RelativePath ] .
			splitPathElements := aDelimiterCharacter split: aString. ].
	
	^ pathClass withAll: splitPathElements
%

category: 'ston'
classmethod: Path
fromSton: stonReader
	| elements |
	elements := Array streamContents: [ :out |
		stonReader parseListDo: [ :each | out nextPut: each ] ].
	^ self withAll: elements
%

category: 'private'
classmethod: Path
isAbsolutePath: aString delimiter: aCharacter
	"Answer a boolean indicating whether the supplied path is considered absolute"

	^aString first = aCharacter
%

category: 'private'
classmethod: Path
isAbsoluteUnixPath: aString
	^aString first = $/ 
%

category: 'private'
classmethod: Path
isAbsoluteWindowsPath: aString
	aString ifEmpty: [ ^ false ].
	(aString first = $\) ifTrue: [ ^ true ]. "e.g. \file"
	^ ((aString size > 2) and: [ aString first isLetter ])
		ifTrue: [ (aString second = $:) and: [aString third = $\] ] "e.g. C:\..."
		ifFalse: [ false ]
%

category: 'instance creation'
classmethod: Path
parent
	"Answer a path that resolves to the parent of the current
	working directory. This is similar to .. in unix, but doesn't
	rely on actual hardlinks being present in the filesystem."

	^ RelativePath with: '..'
%

category: 'instance creation'
classmethod: Path
parents: anInteger
	| path |
	path := self new: anInteger.
	1 to: anInteger do: [:i | path at: i put: '..'].
	^ path
%

category: 'private'
classmethod: Path
removeRedundantSegments: segments
	"Remove redundant elements ('.' and '') from the supplied segments"

	^segments select:
		[ :segment | segment notEmpty and: [ segment ~= '.' ] ]
%

category: 'instance creation'
classmethod: Path
root
	"Answer the root path - ie, / on unix"
	
	^ AbsolutePath new
%

category: 'private'
classmethod: Path
with: aString
	"Answer an instance of the receiver representing the supplied string.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := self delimeter split: aString.
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst
%

category: 'private'
classmethod: Path
withAll: aCollection
	"Answer an instance of the receiver representing the supplied collection of strings.
	This should only be sent to one of the concrete subclasses, e.g. AbsolutePath and RelativePath"
	| inst parsedCollection |

	parsedCollection := OrderedCollection new: (aCollection size max: 10).
	aCollection do: [ :each |
		parsedCollection addAll: (self delimeter split: each) ].
	parsedCollection := self removeRedundantSegments: parsedCollection.
	inst := self new: parsedCollection size.
	parsedCollection withIndexDo: [:segment :index |
		inst at: index put: segment].
	^ inst
%

category: 'instance creation'
classmethod: Path
workingDirectory
	"Answer a path that will always resolve to the current working directory."
	
	^ RelativePath new
%

!		Instance methods for 'Path'

category: 'navigating'
method: Path
, extension 
	^ self withName: self basename extension: extension
%

category: 'navigating'
method: Path
/ aString
	"aString is either a file or path.  If aString is relative, it is appended to the receiver, if it is absolute, an instance of the receiver with the path is answered"

	aString isEmptyOrNil 
		ifTrue: [ Error signal: 'Path element cannot be empty or nil'].

	^self resolvePath: (self class from: aString)
%

category: 'comparing'
method: Path
<= other
	^ self fullName <= other fullName
%

category: 'comparing'
method: Path
= other
	^ self species = other species
		and: [self size = other size
			and: [(1 to: self size) allSatisfy: [:i | (self at: i) = (other at: i)]]]
%

category: 'converting'
method: Path
asPath
	^ self
%

category: 'converting'
method: Path
asPathWith: anObject
	^ self
%

category: 'navigating'
method: Path
asResolvedBy: anObject
	^ anObject resolvePath: self
%

category: 'accessing'
method: Path
base
	^self basenameWithoutExtension 
%

category: 'accessing'
method: Path
basename
	"Returns the base of the basename, 
		i.e. 
		/foo/gloops.taz basename is 'gloops.taz'
		If empty, it is the emptyPathString"

	self isEmpty ifTrue: 
		[ ^ self emptyPathString ].
	^ self at: self size
%

category: 'accessing'
method: Path
basename: newBasename
	"change the basename"
	self size == 0
		"the root node"
		ifTrue: [ ^ Error signal: '0 length Path, cannot change basename'].
	self at: self size put: newBasename
%

category: 'accessing'
method: Path
basenameWithoutExtension
	"Returns the base of the basename but without its extension, 
		i.e. 
		/foo/gloops.taz basenameWithoutExtension is 'gloops'
		/ basenameWithoutExtension is '/'"
	
	^ self basename copyUpToLast: self extensionDelimiter
%

category: 'accessing'
method: Path
basenameWithoutExtension: anExtension
	"Returns the basename without specified extension (if any)
	('/foo/gloops.taz' asPath basenameWithoutExtension: 'taz') = 'gloops'
	"
	| extensionWithDelimiter |

	extensionWithDelimiter := anExtension copyWithFirst: self extensionDelimiter.
	(self basename endsWith: extensionWithDelimiter)
		ifTrue: [^ self basename allButLast: extensionWithDelimiter size]
		ifFalse: [ ^ self basename ]
%

category: 'navigating'
method: Path
canonicalize
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"
	
	^self class withAll: (self class canonicalizeElements: self segments).
%

category: 'comparing'
method: Path
contains: anObject
	"Return true if anObject is in a subfolder of me"
	^ anObject isContainedBy: self
%

category: 'comparing'
method: Path
containsPath: aPath
	self size < aPath size ifFalse: [^ false].
	1	to: self size 
		do: [:i | (self at: i) = (aPath at: i) ifFalse: [^ false]].
	^ true
%

category: 'comparing'
method: Path
containsReference: aReference
	^ false
%

category: 'private'
method: Path
copyFrom: aPath
	| size |
	size := aPath size min: self size.
	1 to: size do: [:i | self at: i put: (aPath at: i)].
	
%

category: 'accessing'
method: Path
delimiter
	^ $/
%

category: 'enumerating'
method: Path
do: aBlock 
	1 
		to: self size
		do: 
			[ :index || segment |
			segment := self at: index.
			segment isEmpty ifFalse: [ aBlock value: segment ] ]
%

category: 'accessing'
method: Path
emptyPathString
	"Answer the string representing an empty (size = 0) instance of the receiver"

	^self delimiter asString
%

category: 'accessing'
method: Path
extension
	"Return the extension of path basename i.e., /foo/gloops.taz extension is 'taz'"
	
	^ self basename copyAfterLast: self extensionDelimiter
%

category: 'accessing'
method: Path
extensionDelimiter
	^ self class extensionDelimiter
%

category: 'accessing'
method: Path
extensions
	"Return the extensions of the receiver in order of appearance"
	"(Path from: '/foo/bar.tar.gz') extensions"
	^ (self extensionDelimiter split: self basename) allButFirst
%

category: 'accessing'
method: Path
fullName
	"Return the fullName of the receiver."
	
	^ self pathString
%

category: 'comparing'
method: Path
hash

"Returns a numeric hash key for the receiver."

| mySize interval hashValue |

(mySize := self size) == 0
  ifTrue: [ ^15243 ].

"Choose an interval so that we sample at most 5 elements of the receiver"
interval := ((mySize - 1) // 4) max: 1.

hashValue := 4459.
1 to: mySize by: interval do: [ :i | | anElement |
  anElement := self at: i.
  hashValue := (hashValue bitShift: -1) bitXor: anElement hash.
  ].

^ hashValue abs
%

category: 'testing'
method: Path
isAbsolute
	self subclassResponsibility 
%

category: 'private'
method: Path
isAllParents
	1 to: self size do: [:i | (self at: i) = '..' ifFalse: [^ false]].
	^ true
%

category: 'comparing'
method: Path
isChildOf: anObject
	^ self parent = anObject
%

category: 'comparing'
method: Path
isContainedBy: anObject
	"DoubleDispatch helper for #contains:"
	^ anObject containsPath: self
%

category: 'testing'
method: Path
isEmpty
	^ self size = 0
%

category: 'testing'
method: Path
isRelative
	^ self isAbsolute not
%

category: 'testing'
method: Path
isRoot
	self subclassResponsibility 
%

category: 'testing'
method: Path
isWorkingDirectory
	^ self size = 0
%

category: 'private'
method: Path
lengthOfStemWith: aPath
	| limit index |
	limit := self size min: aPath size.
	index := 1.
	[index <= limit and: [(self at: index) = (aPath at: index)]] 
		whileTrue: [index := index + 1].
	^ index - 1
%

category: 'navigating'
method: Path
makeRelative: anObject
	^ anObject relativeToPath: self
%

category: 'navigating'
method: Path
parent
	| size parent |
	self isRoot ifTrue: [^ self].
	self isAllParents ifTrue: [^ self / '..'].
	
	size := self size - 1.
	parent := self class new: size.
	1 to: size do: [:i | parent at: i put: (self at: i)].
	^ parent
%

category: 'navigating'
method: Path
parentUpTo: aParentDirName
	"Answers the path of the parent dir with name aParentDirName or root if not found."

	self withParents
		reverseDo: [ :dir | 
			dir basename = aParentDirName
				ifTrue: [ ^ dir ] ].
	^ Path root
%

category: 'printing'
method: Path
pathString
	"Return a string containing the path elements of the receiver, without the 'Path *' part"

	"((FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
		relativeToReference: FileSystem workingDirectory) pathString
	>>> 'book-result/W01-Welcome'"

	^String streamContents: [ :stream | 
		self printPathOn: stream delimiter: self delimiter ]
%

category: 'printing'
method: Path
printOn: aStream 
	self printOn: aStream delimiter: self delimiter.
%

category: 'printing'
method: Path
printOn: aStream delimiter: aCharacter
	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]
%

category: 'printing'
method: Path
printPathOn: aStream
	"Print the receiver's path on aStream (without 'Path' prepended) using the default delimiter"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome') 
			relativeToReference: FileSystem workingDirectory) printPathOn: str]
	>>> 'book-result/W01-Welcome'"

	self printPathOn: aStream delimiter: self delimiter.
%

category: 'printing'
method: Path
printPathOn: aStream delimiter: aCharacter
	"Print the receiver's path on aStream (without 'Path' prepended)"
	"String streamContents: [ :str| 
		((FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
			relativeToReference: FileSystem workingDirectory) printPathOn: str delimiter: $|]
	>>> 'book-result|W01-Welcome'"

	(1 to: self size)
		do: [:index | aStream nextPutAll: (self at: index)]
		separatedBy: [aStream nextPut: aCharacter]
%

category: 'printing'
method: Path
printWithDelimiter: aCharacter
	^ String streamContents: [:out | self printOn: out delimiter: aCharacter]
%

category: 'navigating'
method: Path
relativeTo: anObject
	^ anObject makeRelative: self
%

category: 'navigating'
method: Path
relativeToPath: aPath
	"Return the receiver as relative to the argument aPath"

	"(Path / 'griffle' / 'plonk' / 'nurp') 
		relativeToPath: (Path / 'griffle') 
			returns  plonk/nurp"

	| prefix relative |
	prefix := self lengthOfStemWith: aPath.
	relative := RelativePath parents: aPath size - prefix.
	prefix + 1 to: self size do: [ :i | relative := relative / (self at: i) ].
	^ relative
%

category: 'navigating'
method: Path
resolve
	^ self
%

category: 'navigating'
method: Path
resolve: anObject
	"Return a path in which the argument has been interpreted in the context of the receiver. Different 
	argument types have different resolution semantics, so we use double dispatch to resolve them correctly."
	
	^ anObject asResolvedBy: self
%

category: 'navigating'
method: Path
resolvePath: aPath
	"Answers a path created by resolving the argument against the receiver.
	If the argument is abolute answer the argument itself. Otherwise, concatenate the
	two paths."

	| elements |

	aPath isAbsolute ifTrue: [^ aPath].
	elements := Array new: self size + aPath size.

	1 to: self size do: [:i | elements at: i put: (self at: i)].
	1 to: aPath size do: [:i | elements at: self size + i put: (aPath at: i)].
	
	^ self class withAll: elements
%

category: 'navigating'
method: Path
resolveReference: aReference
	^ aReference
%

category: 'navigating'
method: Path
resolveString: aString
	"Treat strings as relative paths with a single element."
	
	^ self / aString
%

category: 'accessing'
method: Path
segments
	"return an array with all the path segements separated"
	| segments |
	
	segments := OrderedCollection new.
	
	self do: [ :part|
		segments add: part
	].
	
	^ segments asArray 
%

category: 'ston'
method: Path
stonOn: stonWriter
	stonWriter 
		writeObject: self 
		streamShortList: [ :listWriter | 
			self do: [ :each | listWriter add: each ] ]
%

category: 'navigating'
method: Path
withExtension: extension 
	| basename name |
	basename := self basename.
	^ (basename endsWith: extension) 
		ifTrue: [ self ]
		ifFalse: 
			[name := basename copyUpToLast: self extensionDelimiter.
			self withName: name extension: extension]
%

category: 'private'
method: Path
withName: name extension: extension
	| basename |
	basename :=String streamContents:
		[:out |
		out nextPutAll: name.
		out nextPut: self extensionDelimiter.
		out nextPutAll: extension].
	^ self copy 
		at: self size put: basename;
		yourself
%

category: 'enumerating'
method: Path
withParents
	| paths |
	paths := OrderedCollection new.
	1 to: self size -1 do: [ :index | paths add: ((self class new: index) copyFrom: self) ].
	paths add: self.
	
	^ paths
%

! Class implementation for 'AbsolutePath'

!		Class methods for 'AbsolutePath'

category: 'as yet unclassified'
classmethod: AbsolutePath
addEmptyElementTo: result
%

category: 'as yet unclassified'
classmethod: AbsolutePath
from: aString delimiter: aDelimiterCharater
	aString = '/'
		ifTrue: [ ^ self root ].
		
	^ super from: aString delimiter: aDelimiterCharater
%

!		Instance methods for 'AbsolutePath'

category: 'printing'
method: AbsolutePath
asString
  "used by topaz stack display"
  | str sz |
  str := '/' copy .
  1 to: (sz := self size) - 1 do:[:j | str addAll: (self at: j) ; add: $/ ].
  str add: (self at: sz ).
  ^ str
%

category: 'testing'
method: AbsolutePath
isAbsolute
	^ true
%

category: 'testing'
method: AbsolutePath
isRoot
	^ self size = 0
%

category: 'printing'
method: AbsolutePath
printOn: aStream
	aStream nextPutAll: 'Path'.
	self isRoot
		ifTrue: [aStream nextPutAll: ' root']
		ifFalse:
			[1 to: self size do:
				[:i |
				aStream 
					nextPutAll: ' / ''';
				 	nextPutAll: (self at: i);
					nextPut: $']]
%

category: 'printing'
method: AbsolutePath
printPathOn: aStream delimiter: aCharacter
	"Print the path elements of the receiver, without the 'Path *' part"

	aStream nextPut: aCharacter.
	super printPathOn: aStream delimiter: aCharacter
%

category: 'enumerating'
method: AbsolutePath
withParents
	^ super withParents addFirst: (Path root); yourself
%

! Class implementation for 'RelativePath'

!		Instance methods for 'RelativePath'

category: 'accessing'
method: RelativePath
emptyPathString
	"Answer the string representing an empty (size = 0) instance of the receiver.
	For a relative path, this is the current directory"

	^'.'
%

category: 'testing'
method: RelativePath
isAbsolute
	^ false
%

category: 'testing'
method: RelativePath
isRoot
	^ false
%

category: 'printing'
method: RelativePath
printOn: aStream
	aStream nextPutAll: 'Path '.
	self isWorkingDirectory
		ifTrue: [aStream nextPutAll: 'workingDirectory']
		ifFalse: 
			[aStream 
				nextPutAll: '* ''';
				nextPutAll: (self at: 1) asString;
				nextPut: $'.
			2 to: self size do:
				[:i |
				aStream
					nextPutAll: ' / ''';
					nextPutAll: (self at: i);
					nextPut: $']]
					
			
%

category: 'printing'
method: RelativePath
printOn: aStream delimiter: aCharacter
	self isWorkingDirectory ifTrue: [aStream nextPut: $.. ^ self].
	super printOn: aStream delimiter: aCharacter
%

! Class implementation for 'RwAbstractConfigurationPlatformAttributeMatcher'

!		Instance methods for 'RwAbstractConfigurationPlatformAttributeMatcher'

category: 'matching'
method: RwAbstractConfigurationPlatformAttributeMatcher
match: anObject

	self subclassResponsibility: #match:
%

category: 'matching'
method: RwAbstractConfigurationPlatformAttributeMatcher
matchString: aString

	self subclassResponsibility: #matchString:
%

category: 'matching'
method: RwAbstractConfigurationPlatformAttributeMatcher
matchVersion: anRwGemStoneVersionNumber

	self subclassResponsibility: #matchVersion:
%

category: 'accessing'
method: RwAbstractConfigurationPlatformAttributeMatcher
pattern: anObject
	pattern := anObject
%

category: 'accessing'
method: RwAbstractConfigurationPlatformAttributeMatcher
patternMatchBlock: aBlock
	patternMatchBlock := aBlock
%

! Class implementation for 'RwGemStoneVersionConfigurationPlatformAttributeMatcher'

!		Instance methods for 'RwGemStoneVersionConfigurationPlatformAttributeMatcher'

category: 'matching'
method: RwGemStoneVersionConfigurationPlatformAttributeMatcher
match: anObject

	^ anObject rwPlatformAttributeMatchForGemStoneVersion: self
%

category: 'matching'
method: RwGemStoneVersionConfigurationPlatformAttributeMatcher
matchString: aString

	^ false
%

category: 'matching'
method: RwGemStoneVersionConfigurationPlatformAttributeMatcher
matchVersion: aGemStoneVersion

	^ patternMatchBlock cull: pattern cull: aGemStoneVersion
%

! Class implementation for 'RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher'

!		Instance methods for 'RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher'

category: 'matching'
method: RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher
matchVersion: aGemStoneVersion

	^ patternMatchBlock cull: pattern cull: aGemStoneVersion cull: pattern2
%

category: 'accessing'
method: RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher
pattern2: anObject
	pattern2 := anObject
%

! Class implementation for 'RwStringConfigurationPlatformAttributeMatcher'

!		Instance methods for 'RwStringConfigurationPlatformAttributeMatcher'

category: 'matching'
method: RwStringConfigurationPlatformAttributeMatcher
match: anObject

	^ anObject rwPlatformAttributeMatchForString: self
%

category: 'matching'
method: RwStringConfigurationPlatformAttributeMatcher
matchString: aString

	^ patternMatchBlock cull: pattern cull: aString
%

category: 'matching'
method: RwStringConfigurationPlatformAttributeMatcher
matchVersion: aGemStoneVersion

	^ false
%

! Class implementation for 'RwAbstractProjectComponentVisitorV2'

!		Class methods for 'RwAbstractProjectComponentVisitorV2'

category: 'instance creation'
classmethod: RwAbstractProjectComponentVisitorV2
new

	^super new initialize
%

!		Instance methods for 'RwAbstractProjectComponentVisitorV2'

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
componentNames

	^ componentNames
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
componentsPath
	^ self subclassResponsibility: #'componentsPath'
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
groupNames

	^ groupNames
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
groupNames: aColl

	groupNames := aColl copy
%

category: 'initialization'
method: RwAbstractProjectComponentVisitorV2
initialize
	visitedComponentNames := Set new.
	projectNames := Set new.
	componentNames := Set new.
	platformConditionalAttributes := #().
	groupNames := Set new.
	projectLoadSpecs := Set new.
	visitedComponents := Dictionary new
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
packageNames

	self subclassResponsibility: #packageNames
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
platformConditionalAttributes

	^ platformConditionalAttributes
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
platformConditionalAttributes: aColl

	platformConditionalAttributes := aColl
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
projectLoadSpecs

	^ projectLoadSpecs
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
projectNames

	^ projectNames
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
projectsPath
	^ self subclassResponsibility: #'projectsPath'
%

category: 'visiting'
method: RwAbstractProjectComponentVisitorV2
visit: aProjectLoadComponent

	^aProjectLoadComponent acceptVisitor: self
%

category: 'accessing'
method: RwAbstractProjectComponentVisitorV2
visitedComponents

	^ visitedComponents
%

category: 'visiting'
method: RwAbstractProjectComponentVisitorV2
visitLoadSpecification: aLoadSpecification

	self projectLoadSpecs add: aLoadSpecification
%

category: 'visiting'
method: RwAbstractProjectComponentVisitorV2
visitNested: aProjectLoadComponent

	^aProjectLoadComponent acceptNestedVisitor: self
%

category: 'visiting'
method: RwAbstractProjectComponentVisitorV2
visitProjectLoadComponent: aProjectLoadComponent

	(visitedComponentNames includes: aProjectLoadComponent name)
		ifTrue: [ ^ self ].

	self _visited: aProjectLoadComponent.

	definedGroupNames := aProjectLoadComponent definedGroupNames.
	self _processGroupNames.

	self _processConditionalProperties: aProjectLoadComponent.

	(self
		_components: self componentsPath
		forProject: aProjectLoadComponent projectName)
		do: [ :component | component acceptNestedVisitor: self ].

	(self
		_projects: self projectsPath
		forProject: aProjectLoadComponent projectName)
		do: [ :projectSpec | projectSpec acceptVisitor: self ]
%

category: 'private'
method: RwAbstractProjectComponentVisitorV2
_addPackageNames: somePackageNames for: aComponent

	self subclassResponsibility: #_addPackageNames:for:
%

category: 'private'
method: RwAbstractProjectComponentVisitorV2
_matchPlatformAttributes: platformPatternMatcher

	self platformConditionalAttributes do: [:anObject |
		(platformPatternMatcher match: anObject) ifTrue: [ ^true ] ].
	^false
%

category: 'private'
method: RwAbstractProjectComponentVisitorV2
_platformAttributeMatchIn: platformMatchersList

	platformMatchersList do: [:platformPatternMatcher |
		(self _matchPlatformAttributes: platformPatternMatcher) 
			ifTrue: [ ^true ] ].
	^false
%

category: 'private'
method: RwAbstractProjectComponentVisitorV2
_processConditionalProperties: aProjectLoadConfiguration
	aProjectLoadConfiguration conditionalPropertyMatchers
		keysAndValuesDo: [ :platformMatchers :groupMap | 
			(self _platformAttributeMatchIn: platformMatchers)
				ifTrue: [ 
					groupMap
						keysAndValuesDo: [ :group :map | 
							(self groupNames includes: group)
								ifTrue: [ 
									self
										_addPackageNames: (map at: #'packageNames' ifAbsent: [ #() ])
										for: aProjectLoadConfiguration.
									self componentNames
										addAll: (map at: #'componentNames' ifAbsent: [ #() ]).
									self projectNames addAll: (map at: #'projectNames' ifAbsent: [ #() ]) ] ] ] ]
%

category: 'private'
method: RwAbstractProjectComponentVisitorV2
_processGroupNames

	| seedGroupNames seeded |
	seedGroupNames := groupNames asSet copy.
	seeded := Set new.
	[seedGroupNames isEmpty ]
		whileFalse: [ 
			seedGroupNames copy do: [:groupName |
				| referencedGroupNames |
				"make sure that required groups are included in group names, recursively"
				seedGroupNames remove: groupName.
				referencedGroupNames := definedGroupNames at: groupName ifAbsent: [ #() ].
				groupNames addAll: referencedGroupNames.
				referencedGroupNames do: [:refGroupName |
					(seeded includes: refGroupName)
						ifFalse: [
						"ensure that we seed each group only once"
						seeded add: refGroupName.
						seedGroupNames add: refGroupName ] ] ] ].
%

category: 'private'
method: RwAbstractProjectComponentVisitorV2
_visited: aComponent

	visitedComponentNames add:  aComponent name.
	visitedComponents at: aComponent name put: aComponent.
%

! Class implementation for 'RwIndependentComponentVisitorV2'

!		Instance methods for 'RwIndependentComponentVisitorV2'

category: 'accessing'
method: RwIndependentComponentVisitorV2
componentsPath
	^ componentsPath
%

category: 'accessing'
method: RwIndependentComponentVisitorV2
componentsPath: aString
	componentsPath := aString
%

category: 'initialization'
method: RwIndependentComponentVisitorV2
initialize
	super initialize.
	packageNames := Set new.
%

category: 'accessing'
method: RwIndependentComponentVisitorV2
packageNames

	^ packageNames
%

category: 'accessing'
method: RwIndependentComponentVisitorV2
projectsPath
	^ projectsPath
%

category: 'accessing'
method: RwIndependentComponentVisitorV2
projectsPath: aString
	projectsPath := aString
%

category: 'private'
method: RwIndependentComponentVisitorV2
_addPackageNames: somePackageNames for: aComponent

	self packageNames addAll: somePackageNames
%

! Class implementation for 'RwResolvedProjectComponentVisitorV2'

!		Class methods for 'RwResolvedProjectComponentVisitorV2'

category: 'instance creation'
classmethod: RwResolvedProjectComponentVisitorV2
resolvedProject: resolvedProject platformConditionalAttributes: platformConditionalAttributes groupNames: groupNames
	^ self new
		platformConditionalAttributes: platformConditionalAttributes;
		groupNames: groupNames;
		resolvedProject: resolvedProject;
		yourself
%

!		Instance methods for 'RwResolvedProjectComponentVisitorV2'

category: 'accessing'
method: RwResolvedProjectComponentVisitorV2
componentsPath

	^ self resolvedProject componentsRoot
%

category: 'accessing'
method: RwResolvedProjectComponentVisitorV2
packageNames
	^ self resolvedProject packageNames
%

category: 'accessing'
method: RwResolvedProjectComponentVisitorV2
projectDefinition
	^ self resolvedProject _projectDefinition
%

category: 'accessing'
method: RwResolvedProjectComponentVisitorV2
projectsPath

	^ self resolvedProject projectsRoot
%

category: 'accessing'
method: RwResolvedProjectComponentVisitorV2
resolvedProject
	^ resolvedProject
%

category: 'accessing'
method: RwResolvedProjectComponentVisitorV2
resolvedProject: aResolvedProject
	resolvedProject := aResolvedProject
%

category: 'private'
method: RwResolvedProjectComponentVisitorV2
_addPackageNames: somePackageNames for: aComponent

	self projectDefinition addPackages: somePackageNames forComponent: aComponent
%

! Class implementation for 'RwAbstractProjectLoadComponentV2'

!		Class methods for 'RwAbstractProjectLoadComponentV2'

category: 'instance creation'
classmethod: RwAbstractProjectLoadComponentV2
fromUrl: specNameOrUrl

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ ^ self fromFile: url pathForFile ].
	url scheme asString = 'https'
		ifTrue: [ 
self error: 'not yet supported'.
"
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ self _readStonFrom: response decodeFromUTF8
" ].
	self error: 'Unknown scheme: ' , url scheme printString
%

category: 'instance creation'
classmethod: RwAbstractProjectLoadComponentV2
new

	^self basicNew initialize
%

category: 'instance creation'
classmethod: RwAbstractProjectLoadComponentV2
newNamed: aName for: projectName

	^ self new
		name: aName;
		projectName: projectName;
		yourself
%

category: 'private'
classmethod: RwAbstractProjectLoadComponentV2
_gemstoneSupportedPackagePropertyNames
	^ #('methodEnv' 'symbolDictName' 'useSessionMethodsForExtensions')
%

category: 'private'
classmethod: RwAbstractProjectLoadComponentV2
_readStonFrom: stream
	| reader component |
	(reader := STONReader on: stream) allowComplexMapKeys: true.
	component := reader next
		initializeForImport;
		yourself.
	component validate.	"validate when reading from disk, since hand editting could create inconsistencies"
	^ component
%

!		Instance methods for 'RwAbstractProjectLoadComponentV2'

category: 'visiting'
method: RwAbstractProjectLoadComponentV2
acceptNestedVisitor: aVisitor

	^ self acceptVisitor: aVisitor
%

category: 'visiting'
method: RwAbstractProjectLoadComponentV2
acceptVisitor: aVisitor

	^ self subclassResponsibility: #acceptVisitor:
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
comment

   ^comment
%

category: 'initialization'
method: RwAbstractProjectLoadComponentV2
comment: anObject

   comment := anObject
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalComponentsAtConditions: conditions andGroup: groupName
	^ self
		conditionalPropertiesAt: #'componentNames'
		conditions: conditions
		andGroup: groupName
%

category: 'private'
method: RwAbstractProjectLoadComponentV2
conditionalPackageMapSpecMatchers

	conditionalPackageMapSpecMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMapSpecMatchers := Dictionary new.
	self conditionalPackageMapSpecs keysAndValuesDo: [:platformPattern :packageMapSpecsMap |
		conditionalPackageMapSpecMatchers
			at: (self _platformPatternMatcherFor: platformPattern)
			put: packageMapSpecsMap ].
	^ conditionalPackageMapSpecMatchers
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPackageMapSpecs

	^ conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPackageMapSpecsAtGemStoneUserId: userId 

	^ ((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsent: [ ^ Dictionary new ])
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName

	^ (((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsentPut: [ ^ Dictionary new ])
			at: #packageNameToPlatformPropertiesMap ifAbsent: [ ^ Dictionary new ])
				at: packageName ifAbsent: [ ^ Dictionary new ]
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setSymbolDictNameTo: symbolDictName
	| dict |
	dict := (((self conditionalPackageMapSpecs
		at: 'gemstone'
		ifAbsentPut: [ Dictionary new ]) at: userId ifAbsentPut: [ Dictionary new ])
		at: #'packageNameToPlatformPropertiesMap'
		ifAbsentPut: [ Dictionary new ])
		at: packageName
		ifAbsentPut: [ Dictionary new ].
	symbolDictName
		ifNil: [ dict removeKey: 'symbolDictName' ifAbsent: [  ] ]
		ifNotNil: [ dict at: 'symbolDictName' put: symbolDictName asString ]
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPackagesAtConditions: conditions andGroup: groupName
	^ self
		conditionalPropertiesAt: #'packageNames'
		conditions: conditions
		andGroup: groupName
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalProjectsAtConditions: conditions andGroup: groupName
	^ self
		conditionalPropertiesAt: #'projectNames'
		conditions: conditions
		andGroup: groupName
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalProperties
	^ conditionalProperties ifNil: [ conditionalProperties := Dictionary new ]
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPropertiesAt: key conditions: conditions andGroup: groupName

	| thePropertiesMap |
	thePropertiesMap := (self conditionalProperties at: conditions asArray sort ifAbsent: [ ^ Set new])
		at: groupName ifAbsent: [ ^ Set new ].
	^ (thePropertiesMap at: key asSymbol ifAbsent: [ Set new ]) asSet
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPropertiesAtConditions: conditions andGroup: groupName addComponentNames: names
	^ self
		_conditionalPropertiesAt: #'componentNames'
		conditions: conditions
		andGroup: groupName
		addNames: names
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPropertiesAtConditions: conditions andGroup: groupName addPackageNames: names
	^ self
		_conditionalPropertiesAt: #'packageNames'
		conditions: conditions
		andGroup: groupName
		addNames: names
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPropertiesAtConditions: conditions andGroup: groupName addProjectNames: names
	^ self
		_conditionalPropertiesAt: #'projectNames'
		conditions: conditions
		andGroup: groupName
		addNames: names
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPropertiesAtConditions: conditions andGroup: groupName removeComponentNames: names
	^ self
		_conditionalPropertiesAt: #'componentNames'
		conditions: conditions
		andGroup: groupName
		removeNames: names
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPropertiesAtConditions: conditions andGroup: groupName removePackageNames: names
	^ self
		_conditionalPropertiesAt: #'packageNames'
		conditions: conditions
		andGroup: groupName
		removeNames: names
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
conditionalPropertiesAtConditions: conditions andGroup: groupName removeProjectNames: names
	^ self
		_conditionalPropertiesAt: #'projectNames'
		conditions: conditions
		andGroup: groupName
		removeNames: names
%

category: 'private'
method: RwAbstractProjectLoadComponentV2
conditionalPropertyMatchers

	conditionalPropertyMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPropertyMatchers := Dictionary new.
	self conditionalProperties keysAndValuesDo: [:platformPatterns :groupMap |
		conditionalPropertyMatchers
			at: (platformPatterns collect: [:pattern | self _platformPatternMatcherFor: pattern ])
			put: groupMap ].
	^ conditionalPropertyMatchers
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
definedGroupNames

	^ definedGroupNames 
		ifNil: [ definedGroupNames :=  Dictionary new]
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
definedGroupNames: aColl

	definedGroupNames := aColl
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
defineGroupNamed: groupName 

	"add a group that does not include any other groups"

	self defineGroupNamed: groupName toIncludeGroups: #()
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
defineGroupNamed: groupName toIncludeGroups: includedGroups

	"add a group that does not include any other groups"

	self definedGroupNames at: groupName put: includedGroups asArray sort
%

category: 'ston'
method: RwAbstractProjectLoadComponentV2
excludedInstVars
	^ #(#'conditionalPropertyMatchers' #'conditionalPackageMapSpecMatchers')
%

category: 'exporting'
method: RwAbstractProjectLoadComponentV2
export

	|  exportUrl exportPath |
	exportPath := self _repositoryRootPath asFileReference / self _configsPath.
	exportPath ensureCreateDirectory.
	exportUrl := exportPath fileSystem isMemoryFileSystem
		ifTrue: [ 'memory:' , exportPath pathString ]
		ifFalse: [ 'file:' , exportPath pathString, '/' ].
	^ self exportToUrl: exportUrl
%

category: 'exporting'
method: RwAbstractProjectLoadComponentV2
exportToUrl: directoryUrl

	^ self copy
		initializeForExport
		_exportToUrl: directoryUrl
%

category: 'ston'
method: RwAbstractProjectLoadComponentV2
fromSton: stonReader
	"componentNames and projectNames no longer supported ... componentNames and projectnames should be inserted into conditionalProperties under 'common'"

	| instanceVariableNames componentNames projectNames |
	instanceVariableNames := self class allInstVarNames.
	stonReader
		parseMapDo: [ :instVarName :value | 
			instVarName = #'componentNames'
				ifTrue: [ 
					componentNames ifNil: [ componentNames := Set new ].
					componentNames addAll: value ]
				ifFalse: [ 
					instVarName = #'projectNames'
						ifTrue: [ 
							projectNames ifNil: [ projectNames := Set new ].
							projectNames addAll: value ]
						ifFalse: [ 
							instVarName = #'conditionalPackages'
								ifTrue: [ conditionalProperties := value ]
								ifFalse: [ self instVarAt: (instanceVariableNames indexOf: instVarName asSymbol) put: value ] ] ] ].
	componentNames
		ifNotNil: [ 
			self
				conditionalPropertiesAtConditions: {'common'}
				andGroup: 'core'
				addComponentNames: componentNames ].
	projectNames
		ifNotNil: [ 
			self
				conditionalPropertiesAtConditions: {'common'}
				andGroup: 'core'
				addProjectNames: projectNames ]
%

category: 'initialization'
method: RwAbstractProjectLoadComponentV2
initialize
	comment := ''
%

category: 'initialization'
method: RwAbstractProjectLoadComponentV2
initializeForExport
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	projectName := nil.
	conditionalPropertyMatchers := conditionalPackageMapSpecMatchers := nil.
	self conditionalProperties
		ifNotNil: [ :cp | 
			| orderedConditionalProperties |
			orderedConditionalProperties := self class orderedDictionaryClass new.
			(cp keys asSortedCollection: [ :a :b | (a at: 1) <= (b at: 1) ])
				do: [ :ar | 
					| dict orderedPropertyNames |
					dict := cp at: ar.
					orderedPropertyNames := self class orderedDictionaryClass new.
					dict keys asArray sort
						do: [ :group | orderedPropertyNames at: group put: (dict at: group) ].
					orderedConditionalProperties at: ar put: orderedPropertyNames ].
			conditionalProperties := orderedConditionalProperties ].
	conditionalPackageMapSpecs
		ifNotNil: [ 
			| orderedConditionalPackageMapSpecs |
			orderedConditionalPackageMapSpecs := self class orderedDictionaryClass new.
			(conditionalPackageMapSpecs keys asSortedCollection: [ :a :b | a <= b ])
				do: [ :platformName | 
					| orderedUserMap userMap |
					orderedUserMap := self class orderedDictionaryClass new.
					userMap := conditionalPackageMapSpecs at: platformName.
					(userMap keys asSortedCollection: [ :a :b | a <= b ])
						do: [ :userName | 
							| attributeMap orderedAttributeMap |
							attributeMap := userMap at: userName.
							orderedAttributeMap := self class orderedDictionaryClass new.
							(attributeMap keys asSortedCollection: [ :a :b | a <= b ])
								do: [ :attributeName | 
									| packageMap orderedPackageMap |
									packageMap := attributeMap at: attributeName.
									orderedPackageMap := self class orderedDictionaryClass new.
									(packageMap keys asSortedCollection: [ :a :b | a <= b ])
										do: [ :packageName | 
											(packageMap at: packageName) isEmpty
												ifFalse: [ orderedPackageMap at: packageName put: (packageMap at: packageName) ] ].
									orderedPackageMap isEmpty
										ifFalse: [ orderedAttributeMap at: attributeName put: orderedPackageMap ] ].
							orderedAttributeMap isEmpty
								ifFalse: [ orderedUserMap at: userName put: orderedAttributeMap ] ].
					orderedUserMap isEmpty
						ifFalse: [ orderedConditionalPackageMapSpecs at: platformName put: orderedUserMap ] ].
			conditionalPackageMapSpecs := orderedConditionalPackageMapSpecs ]
%

category: 'initialization'
method: RwAbstractProjectLoadComponentV2
initializeForImport

	"if spec has been imported, clear out any of the fields that represent state that should 
	not be shared"

	projectName := nil
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
name

   ^name
%

category: 'initialization'
method: RwAbstractProjectLoadComponentV2
name: anObject

   name := anObject
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
packageNames
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames |
	allDefinedPackageNames := Set new.
	self conditionalProperties
		keysAndValuesDo: [ :conditions :groupMap | 
			groupMap
				keysAndValuesDo: [ :groupName :propertiesMap | allDefinedPackageNames addAll: (propertiesMap at: #'packageNames') ] ].
	^ allDefinedPackageNames
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
packageNamesForPlatformConfigurationAttributes: platformConfigurationAttributes groupNames: groupNames
	"Answer the collection of package names defined in the receiver."

	| allDefinedPackageNames matchers |
	allDefinedPackageNames := Set new.
	matchers := self conditionalPropertyMatchers.
	self conditionalProperties
		keysAndValuesDo: [ :conditions :ignored | 
			platformConfigurationAttributes
				do: [ :anObject | 
					matchers
						keysAndValuesDo: [ :ar :groupMap | 
							ar
								do: [ :matcher | 
									(matcher match: anObject)
										ifTrue: [ 
											groupMap
												keysAndValuesDo: [ :groupName :packageMap | 
													(groupNames includes: groupName)
														ifTrue: [ allDefinedPackageNames addAll: (packageMap at: #'packageNames') ] ] ] ] ] ] ].
	^ allDefinedPackageNames
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
projectName

   ^projectName
%

category: 'initialization'
method: RwAbstractProjectLoadComponentV2
projectName: anObject

   projectName := anObject
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
removeComponentNamed: aComponentName
	"this can create empty componentName lists  ... the export logic _should_ cleanup empty list, which is sufficient"

	self conditionalProperties
		keysAndValuesDo: [ :conditionsArray :conditionMap | 
			conditionMap
				keysAndValuesDo: [ :groupName :propertiesMap | (propertiesMap at: #'componentNames' ifAbsent: [#()]) remove: aComponentName ifAbsent: [  ] ] ]
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
removePackageNamed: aPackageName
	"this can create empty packageName lists or empty packageMapSpecMaps ... the export logic _should_ cleanup empty list, which is sufficient"

	self conditionalProperties
		keysAndValuesDo: [ :conditionsArray :conditionMap | 
			conditionMap
				keysAndValuesDo: [ :groupName :propertiesMap | (propertiesMap at: #'packageNames' ifAbsent: [#()]) remove: aPackageName ifAbsent: [  ] ] ].
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformPattern :packageMapSpecsMap | 
			packageMapSpecsMap
				keysAndValuesDo: [ :userId :packageMapSpecs | 
					(packageMapSpecs at: #'packageNameToPlatformPropertiesMap')
						removeKey: aPackageName
						ifAbsent: [  ] ] ]
%

category: 'accessing'
method: RwAbstractProjectLoadComponentV2
removeProjectNamed: aProjectName
	"this can create empty projectName lists  ... the export logic _should_ cleanup empty list, which is sufficient"

	self conditionalProperties
		keysAndValuesDo: [ :conditionsArray :conditionMap | 
			conditionMap
				keysAndValuesDo: [ :groupName :propertiesMap | (propertiesMap at: #'projectNames' ifAbsent: [#()]) remove: aProjectName ifAbsent: [  ] ] ].
%

category: 'ston'
method: RwAbstractProjectLoadComponentV2
stonOn: stonWriter   
	| instanceVariableNames |
	instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
	stonWriter writeObject: self
		streamMap: 
			[:dictionary |
			instanceVariableNames do: 
					[:each |
					(self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
						ifNotNil: [:value | dictionary at: each asSymbol put: value]
						ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
%

category: 'validation'
method: RwAbstractProjectLoadComponentV2
validate
	"ensure that the data structures within the receiver contain valid information:
		1. only packages defined in the receiver may be referenced in the reciever
		2. platform implementation is responsible for validating platform structures"

	| allDefinedPackageNames knownGroupNames |
	self name ifNil: [ self error: 'name is nil' ].
	allDefinedPackageNames := Set new.
	knownGroupNames := self definedGroupNames keys.
	self definedGroupNames
		keysAndValuesDo: [ :groupName :dependentGroups | 
			dependentGroups
				do: [ :dependentGroupName | 
					(knownGroupNames includes: dependentGroupName)
						ifFalse: [ 
							Error
								signal:
									'The group ' , dependentGroupName printString , ' is not a defined group' ] ] ].
	self conditionalProperties
		keysAndValuesDo: [ :conditions :groupMap | 
			groupMap
				keysAndValuesDo: [ :groupName :propertiesMap | 
					(self definedGroupNames keys includes: groupName)
						ifFalse: [ 
							Error
								signal:
									'Conditional packages includes group name ' , groupName printString
										, ' that is not a defined group' ].
					allDefinedPackageNames
						addAll: (propertiesMap at: #'packageNames' ifAbsent: [ #() ]) ] ].
	self conditionalPackageMapSpecs
		keysAndValuesDo: [ :platformName :userIdMap | 
			(RwSpecification _supportedPlatformNames includes: platformName)
				ifFalse: [ 
					Error
						signal:
							'Unknown platform name ' , platformName printString
								, ' in conditional package map specs' ].
			platformName = 'gemstone'
				ifTrue: [ self _validateGemStonePlatform: allDefinedPackageNames userIdMap: userIdMap ] ].
	^ true
%

category: 'private'
method: RwAbstractProjectLoadComponentV2
_conditionalPropertiesAt: key conditions: conditions andGroup: groupName addNames: names
	| theNames theConditionalPropertiesMap |
	theConditionalPropertiesMap := (self conditionalProperties
		at: conditions asArray sort
		ifAbsentPut: [ Dictionary new ]) at: groupName ifAbsentPut: [ Dictionary new ].
	theNames := (theConditionalPropertiesMap
		at: key asSymbol
		ifAbsentPut: [ Set new ]) asSet.
	theNames addAll: names.
	theNames := theNames asArray sort.
	theConditionalPropertiesMap at: key asSymbol put: theNames.
	^ theNames
%

category: 'private'
method: RwAbstractProjectLoadComponentV2
_conditionalPropertiesAt: key conditions: conditions andGroup: groupName removeNames: names
	| theNames theConditionalPropertiesMap |
	theConditionalPropertiesMap := (self conditionalProperties
		at: conditions asArray sort
		ifAbsentPut: [ Dictionary new ]) at: groupName ifAbsentPut: [ Dictionary new ].
	theNames := (theConditionalPropertiesMap
		at: key asSymbol
		ifAbsentPut: [ Set new ]) asSet.
	theNames removeAll: names.
	theNames := theNames asArray sort.
	theConditionalPropertiesMap at: key asSymbol put: theNames.
	^ theNames
%

category: 'private'
method: RwAbstractProjectLoadComponentV2
_platformPatternMatcherFor: pattern

	" Returns an instance of RwAbstractConfigurationPlatformAttributeMatcher:
		RwStringConfigurationPlatformAttributeMatcher,
		RwGemStoneVersionConfigurationPlatformAttributeMatcher,
		or RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher
	"

	| versionPattern gsVersion1 gsVersion2 |
	(pattern beginsWith: 'gs')
		ifFalse: [ 
			"simple equality match"
			^ RwStringConfigurationPlatformAttributeMatcher new
					pattern: pattern;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	"GemStone version pattern"
	versionPattern := (pattern copyFrom: 3 to: pattern size) substrings: '.'.
	(versionPattern last beginsWith: '[')
		ifTrue: [ 
			| vpSize rangePattern dashIndex |
			"range pattern"
			vpSize := versionPattern size.
			gsVersion1 := RwGemStoneVersionNumber new: vpSize .
			1 to: vpSize - 1
				do: [:index | gsVersion1 at: index put: (versionPattern at: index) asInteger ].
			gsVersion1 at: vpSize put: 0.
			rangePattern := (versionPattern at: vpSize) trimBoth.
			(((rangePattern at: 1) = $[) and: [ (rangePattern at: rangePattern size) = $] ])
				ifFalse: [ self error: 'Poorly formed GemStone version range pattern ', rangePattern printString, ' in ', pattern printString ].
			rangePattern := rangePattern copyFrom: 2 to: rangePattern size -1.
			dashIndex := rangePattern indexOf: $-.
			dashIndex <= 1
				ifTrue: [ self error: 'Invalid version range pattern missing range begin' , rangePattern printString, ' in ', pattern printString ].
			gsVersion1 at: vpSize put: (rangePattern copyFrom: 1 to: dashIndex -1) asInteger.
			dashIndex = rangePattern size
				ifTrue: [
					"open range"
					gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size -1.
					gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
					^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
							pattern: gsVersion1;
							pattern2: gsVersion2;
							patternMatchBlock: [:a :b :c | (a <= b) & (b < c ) ];
							yourself ].
			"closed range"
			gsVersion2 := gsVersion1 copy.
			gsVersion2 at: vpSize put: (rangePattern copyFrom: dashIndex + 1 to: rangePattern size) asInteger.
			^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
					pattern: gsVersion1;
					pattern2: gsVersion2;
					patternMatchBlock: [:a :b :c | (a <= b) & (b <= c ) ];
					yourself ].
	versionPattern last = 'x' 
		ifFalse: [
			"specific version number match, use ="
			^ RwGemStoneVersionConfigurationPlatformAttributeMatcher new
					pattern: (pattern copyFrom: 3 to: pattern size) asRwGemStoneVersionNumber;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	" 'gs', <gemstone-version-number> , '.x'"
	"match all values in x field"
	gsVersion1 := ((pattern copyFrom: 3 to: pattern size - 2), '.0') asRwGemStoneVersionNumber.
	gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size - 1.
	gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
	^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
			pattern: gsVersion1;
			pattern2: gsVersion2;
			patternMatchBlock: [:a :b :c | (a <= b) & (b < c ) ];
			yourself
%

category: 'validation'
method: RwAbstractProjectLoadComponentV2
_validateGemStonePlatform: allDefinedPackageNames userIdMap: userIdMap
	"ensure that the data structures within the receiver contain valid information:
		1. only packages defined in the receiver may be referenced in the reciever
		2. platform implementation is responsible for validating platform structures"

	userIdMap
		keysAndValuesDo: [ :userId :platformPropertiesMap | 
			platformPropertiesMap
				keysAndValuesDo: [ :key :packagePropertiesMap | 
					key == #'packageNameToPlatformPropertiesMap'
						ifFalse: [ Error signal: 'Unknown platformPropertiesMap key ' , key printString ].
					packagePropertiesMap
						keysAndValuesDo: [ :packageName :packageProperties | 
							(allDefinedPackageNames includes: packageName)
								ifFalse: [ 
									Error
										signal:
											'Undefined package name ' , packageName printString
												, ' used in plaform properties map' ].
							packageProperties
								keysDo: [ :packagePropertyName | 
									(self class _gemstoneSupportedPackagePropertyNames
										includes: packagePropertyName)
										ifFalse: [ Error signal: 'Unknown package property name ' , packagePropertyName printString ] ] ] ] ]
%

! Class implementation for 'RwNestedProjectLoadComponentV2'

!		Instance methods for 'RwNestedProjectLoadComponentV2'

category: 'visiting'
method: RwNestedProjectLoadComponentV2
acceptNestedVisitor: aVisitor

	^aVisitor visitProjectLoadComponent: self
%

category: 'visiting'
method: RwNestedProjectLoadComponentV2
acceptVisitor: aVisitor

	^self error: 'nested configuration cannot be used as a top-level configuration. The receiver is nested inside of project load configurations'
%

category: 'testing'
method: RwNestedProjectLoadComponentV2
isIndependentlyLoadable
	"nested configuration is not independently loadable ... they can only be loaded when referenced from another config"

	^ false
%

! Class implementation for 'RwProjectLoadComponentV2'

!		Instance methods for 'RwProjectLoadComponentV2'

category: 'visiting'
method: RwProjectLoadComponentV2
acceptVisitor: aVisitor

	^aVisitor visitProjectLoadComponent: self
%

! Class implementation for 'RwSpecification'

!		Class methods for 'RwSpecification'

category: 'instance creation'
classmethod: RwSpecification
fromUrl: specNameOrUrl

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ ^ self fromFile: url pathForFile ].
	url scheme asString = 'https'
		ifTrue: [ self error: 'not yet supported'.
"
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ self _readStonFrom: response decodeFromUTF8
" ].
	self error: 'Unknown scheme: ' , url scheme printString
%

category: 'instance creation'
classmethod: RwSpecification
new

	^self basicNew initialize
%

category: 'accessing'
classmethod: RwSpecification
_supportedPlatformNames
	^ #('gemstone' 'pharo' 'squeak' 'vast')
%

!		Instance methods for 'RwSpecification'

category: 'conversion'
method: RwSpecification
asSpecification

	^ self
%

category: 'private'
method: RwSpecification
currentVersion
	"
		0.1.0 - initial version for specs
		0.2.0 - defaultConfigurationNames and defaultGroupNames i.v. added to RwProjectSpecification
		0.3.0 - remoteUrl i.v. added to RwGitRepositorySpecification
	"
	^ '0.3.0'
%

category: 'initialization'
method: RwSpecification
initialize
  
%

category: 'initialization'
method: RwSpecification
initializeForExport
  "if spec has been exported, clear out any of the fields that represent local disk state"
%

category: 'initialization'
method: RwSpecification
initializeForImport

  "if spec has been imported, clear out any of the fields that represent local disk state"
  
%

category: 'copying'
method: RwSpecification
postCopy
  "don't forget to copy any compound state objects"

  
%

category: 'accessing'
method: RwSpecification
projectUrl: ignored
  
%

category: 'accessing'
method: RwSpecification
version

	^ nil
%

! Class implementation for 'RwLoadSpecificationV2'

!		Class methods for 'RwLoadSpecificationV2'

category: 'gemstone-support'
classmethod: RwLoadSpecificationV2
_gemstoneAllUsersName
	^ 'allusers'
%

!		Instance methods for 'RwLoadSpecificationV2'

category: 'comparing'
method: RwLoadSpecificationV2
= anObject
	^ self specName = anObject specName
		and: [ 
			self projectName = anObject projectName
				and: [ 
					self projectAlias = anObject projectAlias
						and: [ 
							self projectsHome = anObject projectsHome
								and: [ 
									self componentNames asArray sort = anObject componentNames asArray sort
										and: [ 
											self groupNames asArray sort = anObject groupNames asArray sort
												and: [ 
													self projectSpecFile = anObject projectSpecFile
														and: [ 
															self repositoryResolutionPolicy = anObject repositoryResolutionPolicy
																and: [ 
																	self gitUrl = anObject gitUrl
																		and: [ 
																			self diskUrl = anObject diskUrl
																				and: [ 
																					self mercurialUrl = anObject mercurialUrl
																						and: [ 
																							self svnUrl = anObject svnUrl
																								and: [ 
																									self revision = anObject revision
																										and: [ 
																											self comment = anObject comment
																												and: [ 
																													self _platformProperties = anObject _platformProperties
																														or: [ self platformProperties = anObject platformProperties ] ] ] ] ] ] ] ] ] ] ] ] ] ] ]
%

category: 'visiting'
method: RwLoadSpecificationV2
acceptVisitor: aVisitor

	^ aVisitor visitLoadSpecification: self
%

category: 'accessing'
method: RwLoadSpecificationV2
comment
	^ comment ifNil: [ ^ '' ]
%

category: 'accessing'
method: RwLoadSpecificationV2
comment: aString
	comment := aString
%

category: 'accessing'
method: RwLoadSpecificationV2
componentNames
	^ componentNames
%

category: 'accessing'
method: RwLoadSpecificationV2
componentNames: anArray
	componentNames := anArray asArray
%

category: 'accessing'
method: RwLoadSpecificationV2
customConditionalAttributes

	^ customConditionalAttributes ifNil: [ #() ]
%

category: 'accessing'
method: RwLoadSpecificationV2
customConditionalAttributes: anArray

	customConditionalAttributes := anArray
%

category: 'accessing'
method: RwLoadSpecificationV2
diskUrl
	^ diskUrl
%

category: 'accessing'
method: RwLoadSpecificationV2
diskUrl: anUrlString
	diskUrl := anUrlString
%

category: 'exporting'
method: RwLoadSpecificationV2
exportTo: directoryReference
	(directoryReference / self specName, 'ston')
		writeStreamDo: [ :fileStream | STON put: self copy initializeForExport onStreamPretty: fileStream ]
%

category: 'ston'
method: RwLoadSpecificationV2
fromSton: stonReader
	"return an instance of RwResolvedLoadSpecificationV2 that wraps the receiver"

	^ (super fromSton: stonReader)
		_validate;
		yourself
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneDefaultMethodEnvForUser: userId
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultMethodEnv ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultMethodEnv ] ].
	^ userProperties
		at: #'defaultMethodEnv'
		ifAbsent: [ self _gemstoneDefaultMethodEnv ]
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneDefaultSymbolDictNameForUser: userId
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultSymbolDictName ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultSymbolDictName ] ].
	^ userProperties
		at: #'defaultSymbolDictName'
		ifAbsent: [ self _gemstoneDefaultSymbolDictName ]
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultUseSessionMethodsForExtensions ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultUseSessionMethodsForExtensions ] ].
	^ userProperties
		at: #'defaultUseSessionMethodsForExtensions'
		ifAbsent: [ self _gemstoneDefaultUseSessionMethodsForExtensions ]
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneSetDefaultMethodEnvForUser: userId to: env

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultMethodEnv put: env
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneSetDefaultMethodEnvTo: env
	self gemstoneSetDefaultMethodEnvForUser: self _gemstoneAllUsersName to: env
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultSymbolDictName put: symbolDictName
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneSetDefaultSymbolDictNameTo: symbolDictName
	self gemstoneSetDefaultSymbolDictNameForUser: self _gemstoneAllUsersName to: symbolDictName
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultUseSessionMethodsForExtensions put: aBool
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool
	self gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: self _gemstoneAllUsersName to: aBool
%

category: 'accessing'
method: RwLoadSpecificationV2
gitUrl
	^ gitUrl
%

category: 'accessing'
method: RwLoadSpecificationV2
gitUrl: anUrlString
	gitUrl := anUrlString
%

category: 'accessing'
method: RwLoadSpecificationV2
groupNames
	^ groupNames
%

category: 'accessing'
method: RwLoadSpecificationV2
groupNames: anArray
	groupNames := anArray asArray
%

category: 'comparing'
method: RwLoadSpecificationV2
hash
	| hashValue |
	hashValue := self specName hash.
	hashValue := hashValue bitXor: self projectName hash.
	hashValue := hashValue bitXor: self projectAlias hash.
	hashValue := hashValue bitXor: self projectSpecFile hash.
	hashValue := hashValue bitXor: self gitUrl hash.
	hashValue := hashValue bitXor: self diskUrl hash.
	hashValue := hashValue bitXor: self mercurialUrl hash.
	hashValue := hashValue bitXor: self svnUrl hash.
	hashValue := hashValue bitXor: self revision hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self componentNames hash.
	hashValue := hashValue bitXor: self projectsHome hash.
	hashValue := hashValue bitXor: self groupNames hash.
	hashValue := hashValue bitXor: self repositoryResolutionPolicy hash.
	hashValue := hashValue bitXor: self _platformProperties hash.
	^ hashValue
%

category: 'initialization'
method: RwLoadSpecificationV2
initialize
	super initialize.
	componentNames := {}.
	groupNames := {'core'}.
	projectSpecFile := 'rowan/project.ston'.
	comment := ''
%

category: 'initialization'
method: RwLoadSpecificationV2
initializeForExport
	"if spec is to be exported, clear out any of the fields that represent local disk state"

	super initializeForExport.
	repositoryResolutionPolicy := projectsHome :=  nil
%

category: 'testing'
method: RwLoadSpecificationV2
isStrict
	^ self repositoryResolutionPolicy == #strict
%

category: 'accessing'
method: RwLoadSpecificationV2
mercurialUrl
	^ mercurialUrl
%

category: 'accessing'
method: RwLoadSpecificationV2
mercurialUrl: anUrlString
	mercurialUrl := anUrlString
%

category: 'accessing'
method: RwLoadSpecificationV2
platformProperties
	^ platformProperties ifNil: [ platformProperties := Dictionary new ]
%

category: 'copying'
method: RwLoadSpecificationV2
postCopy
	"don't forget to copy any compound state objects"

	super postCopy.
	componentNames := componentNames copy.
	groupNames := groupNames copy.
	platformProperties
		ifNotNil: [ 
			| platformPropertiesCopy |
			platformPropertiesCopy := platformProperties copy.
			platformProperties
				keysAndValuesDo: [ :platformName :userIdMap | 
					| userIdMapCopy |
					userIdMapCopy := userIdMap copy.
					platformPropertiesCopy at: platformName put: userIdMapCopy.
					platformName = 'gemstone'
						ifTrue: [ 
							userIdMap
								keysAndValuesDo: [ :userId :platformPropertiesMap | 
									| platformPropertiesMapCopy |
									platformPropertiesMapCopy := platformPropertiesMap copy.
									userIdMapCopy at: userId put: platformPropertiesMapCopy.
									platformPropertiesMap
										keysAndValuesDo: [ :propertyKey :propertyValue | platformPropertiesMapCopy at: propertyKey put: propertyValue copy ] ] ]
						ifFalse: [ self error: 'postCopy of ' , platformName printString , ' not yet implemented' ] ].
			platformProperties := platformPropertiesCopy ]
%

category: 'accessing'
method: RwLoadSpecificationV2
projectAlias
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	^ projectAlias ifNil: [ self projectName ]
%

category: 'accessing'
method: RwLoadSpecificationV2
projectAlias: aString
	"project alias is used as the name of the root directory for the project ... necessary if the project is 
		embedded in another project's git repository or you want use a non-default directory on disk"

	projectAlias := aString
%

category: 'accessing'
method: RwLoadSpecificationV2
projectName
	^ projectName
%

category: 'accessing'
method: RwLoadSpecificationV2
projectName: aString
	projectName := aString
%

category: 'accessing'
method: RwLoadSpecificationV2
projectsHome
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	^ projectsHome ifNil: [FileLocator rowanProjectsHome ]
%

category: 'accessing'
method: RwLoadSpecificationV2
projectsHome: aStringOrFileReference
	"projects home specifies the disk location where projects cloned/created by the receiver will be located."

	projectsHome := aStringOrFileReference asFileReference
%

category: 'accessing'
method: RwLoadSpecificationV2
projectSpecFile
	^ projectSpecFile
%

category: 'accessing'
method: RwLoadSpecificationV2
projectSpecFile: aString
	projectSpecFile := aString
%

category: 'accessing'
method: RwLoadSpecificationV2
projectUrl
	^ self gitUrl
		ifNotNil: [ :urlString | urlString ]
		ifNil: [ 
			self svnUrl
				ifNotNil: [ :urlString | urlString ]
				ifNil: [ self mercurialUrl ifNotNil: [ :urlString | urlString ] ifNil: [ self diskUrl ] ] ]
%

category: 'accessing'
method: RwLoadSpecificationV2
repositoryResolutionPolicy
	^repositoryResolutionPolicy
%

category: 'accessing'
method: RwLoadSpecificationV2
repositoryResolutionPolicy: aSymbolOrNil
	"
	#strict - repository will be forced to match the specificed revision
	"

	repositoryResolutionPolicy := aSymbolOrNil
%

category: 'accessing'
method: RwLoadSpecificationV2
repositoryRoot
	^ self projectsHome / self projectAlias
%

category: 'actions'
method: RwLoadSpecificationV2
resolveStrict
	"resolve using #strict repositoryResolutionpolicy"

	| oldPolicy |
	self isStrict
		ifTrue: [ ^ self resolve ].
	oldPolicy := self repositoryResolutionPolicy.
	[ 
	"force #strict policy to ensure that the revision is checked out out in the repository"
	self repositoryResolutionPolicy: #'strict'.
	^ self resolve ]
		ensure: [ self repositoryResolutionPolicy: oldPolicy ]
%

category: 'actions'
method: RwLoadSpecificationV2
resolveWithParentProject: aResolvedProject
	"give embedded projects a chance to resolve cleanly"

	self projectsHome: aResolvedProject projectsHome.
	^ self resolve
%

category: 'accessing'
method: RwLoadSpecificationV2
revision
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision
%

category: 'accessing'
method: RwLoadSpecificationV2
revision: aRevisionString
	" git :: committish; svn :: revision; mercurial :: changeset; disk :: empty string"

	^ revision := aRevisionString
%

category: 'accessing'
method: RwLoadSpecificationV2
specName

	^ specName ifNil: [ self projectAlias ]
%

category: 'accessing'
method: RwLoadSpecificationV2
specName: aString
	specName := aString
%

category: 'accessing'
method: RwLoadSpecificationV2
svnUrl
	^ svnUrl
%

category: 'accessing'
method: RwLoadSpecificationV2
svnUrlUrl: anUrlString
	svnUrl := anUrlString
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
_gemstoneAllUsersName

	^ self class _gemstoneAllUsersName
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
_gemstoneDefaultMethodEnv
	^ 0
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
_gemstoneDefaultSymbolDictName

	^ 'UserGlobals'
%

category: 'gemstone-support'
method: RwLoadSpecificationV2
_gemstoneDefaultUseSessionMethodsForExtensions

	^ false
%

category: 'accessing'
method: RwLoadSpecificationV2
_platformProperties
	"direct access to IV ... used by ="

	^ platformProperties
%

category: 'private'
method: RwLoadSpecificationV2
_validate
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	| repoUrls |
	#(#'componentNames' #'groupNames' #'specName' #'projectSpecFile')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	repoUrls := {}.
	#(#'gitUrl' #'diskUrl' #'mercurialUrl' #'svnUrl')
		do: [ :messageName | (self perform: messageName) ifNotNil: [ repoUrls add: messageName asString ] ].
	repoUrls size > 1
		ifTrue: [ Error signal: 'Only one of (gitUrl diskUrl mercurialUrl svnUrl) must be be set' ].
	(repoUrls size = 0 or: [ repoUrls = #('diskUrl') ])
		ifTrue: [ 
			self revision
				ifNotNil: [ :rev | 
					Error
						signal:
							'Invalid revision ' , rev printString
								, '. Should be nil for disk-based repository' ] ]
		ifFalse: [ 
			self gitUrl asRwUrl scheme ~= 'file'
				ifTrue: [ 
					self revision
						ifNil: [ 
							Error
								signal:
									'The instance variable ''revision'' must be set for the'
										, (repoUrls at: 1) asString printString ] ] ].
	self platformProperties
		keysAndValuesDo: [ :platformName :userIdMap | 
			(self class _supportedPlatformNames includes: platformName)
				ifFalse: [ 
					Error
						signal:
							'Unknown platform name ' , platformName printString , ' in platform properties' ].
			platformName = 'gemstone'
				ifTrue: [ self _validateGemStonePlatformUserIdMap: userIdMap ] ].
	^ true
%

category: 'private'
method: RwLoadSpecificationV2
_validateGemStonePlatformUserIdMap: userIdMap
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	| expectedPropertyMap |
	expectedPropertyMap := Dictionary new
		add: #'defaultSymbolDictName' -> CharacterCollection;
		add: #'defaultMethodEnv' -> SmallInteger;
		add: #'defaultUseSessionMethodsForExtensions' -> Boolean;
		yourself.
	userIdMap
		keysAndValuesDo: [ :userId :platformPropertiesMap | 
			platformPropertiesMap
				keysAndValuesDo: [ :propertyKey :propertyValue | 
					(expectedPropertyMap includesKey: propertyKey)
						ifTrue: [ 
							| expectedClass |
							expectedClass := expectedPropertyMap at: propertyKey.
							(propertyValue isKindOf: expectedClass)
								ifFalse: [ 
									Error
										signal:
											'Value of property (' , propertyKey printString , '->'
												, propertyValue printString , ') is expected to be class '
												, expectedClass name asString printString , ' not class '
												, propertyValue class name asString printString ] ]
						ifFalse: [ Error signal: 'Unknown platform property key ' , propertyKey printString ] ] ]
%

! Class implementation for 'RwProjectSpecificationV2'

!		Instance methods for 'RwProjectSpecificationV2'

category: 'comparing'
method: RwProjectSpecificationV2
= anObject
	| lazyEqual |
	^ self specName = anObject specName
		and: [ 
			self projectName = anObject projectName
				and: [ 
					self componentsPath = anObject componentsPath
						and: [ 
							self packageFormat = anObject packageFormat
								and: [ 
									self packageConvention = anObject packageConvention
										and: [ 
											self packagesPath = anObject packagesPath
												and: [ 
													self projectsPath = anObject projectsPath
														and: [ 
															self specsPath = anObject specsPath
																and: [ 
																	lazyEqual := self _repoType = anObject _repoType
																		or: [ self repoType = anObject repoType ].
																	lazyEqual
																		and: [ 
																			self comment = anObject comment
																				and: [ self loadedCommitId = anObject loadedCommitId ] ] ] ] ] ] ] ] ] ]
%

category: 'accessing'
method: RwProjectSpecificationV2
comment
	^ comment ifNil: [ ^ '' ]
%

category: 'accessing'
method: RwProjectSpecificationV2
comment: aString
	comment := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
componentsPath

	^ componentsPath
%

category: 'accessing'
method: RwProjectSpecificationV2
componentsPath: aString
	componentsPath := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
componentsRoot
	^ self repositoryRoot / self componentsPath
%

category: 'exporting'
method: RwProjectSpecificationV2
exportTo: directoryReference

	(directoryReference / self projectSpecPath / self specName, 'ston')
		writeStreamDo: [ :fileStream | STON put: self copy initializeForExport onStreamPretty: fileStream ]
%

category: 'comparing'
method: RwProjectSpecificationV2
hash
	| hashValue |
	hashValue := self specName hash.
	hashValue := hashValue bitXor: self projectName hash.
	hashValue := hashValue bitXor: self componentsPath hash.
	hashValue := hashValue bitXor: self packagesPath hash.
	hashValue := hashValue bitXor: self projectsPath hash.
	hashValue := hashValue bitXor: self specsPath hash.
	hashValue := hashValue bitXor: self _repoType hash.
	hashValue := hashValue bitXor: self comment hash.
	hashValue := hashValue bitXor: self packageFormat hash.
	hashValue := hashValue bitXor: self packageConvention hash.
	hashValue := hashValue bitXor: self loadedCommitId hash.
	^ hashValue
%

category: 'initialization'
method: RwProjectSpecificationV2
initialize
	super initialize.
	componentsPath := 'rowan/components'.
	packagesPath := 'rowan/src'.
	projectsPath := 'rowan/projects'.
	specsPath := 'rowan/specs'.
	projectSpecPath := 'rowan'.
	specName := 'project'.
	comment := ''
%

category: 'initialization'
method: RwProjectSpecificationV2
initializeForExport
	"if spec is to be exported, clear out any of the fields that represent local disk state"

	super initializeForExport.
	projectName := loadedCommitId := repoType := nil
%

category: 'accessing'
method: RwProjectSpecificationV2
loadedCommitId
	^ loadedCommitId
%

category: 'accessing'
method: RwProjectSpecificationV2
loadedCommitId: aCommitId
	loadedCommitId := aCommitId
%

category: 'accessing'
method: RwProjectSpecificationV2
packageConvention
	^ packageConvention ifNil: [ 'RowanHybrid' ]
%

category: 'accessing'
method: RwProjectSpecificationV2
packageConvention: aString
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	packageConvention := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
packageFormat
	^ self packageFormatIfAbsent: [ 'tonel' ]
%

category: 'accessing'
method: RwProjectSpecificationV2
packageFormat: aString
	(#('tonel' 'filetree') includes: aString)
		ifFalse: [ 
			self
				error:
					'Unknown package format ' , aString printString
						, '. Should be one of: tonel, or filetree' ].
	packageFormat := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
packageFormatIfAbsent: absentBlock
	^ packageFormat ifNil: absentBlock
%

category: 'accessing'
method: RwProjectSpecificationV2
packagesPath

	^ packagesPath
%

category: 'accessing'
method: RwProjectSpecificationV2
packagesPath: aString
	packagesPath := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
packagesRoot
	^ self repositoryRoot / self packagesPath
%

category: 'accessing'
method: RwProjectSpecificationV2
projectName

	^ projectName
%

category: 'accessing'
method: RwProjectSpecificationV2
projectName: aString
	projectName := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
projectRoots
	^ {(self componentsRoot).
	(self packagesRoot).
	(self specsRoot).
	(self projectsRoot)}
%

category: 'accessing'
method: RwProjectSpecificationV2
projectsPath

	^ projectsPath
%

category: 'accessing'
method: RwProjectSpecificationV2
projectsPath: aString
	projectsPath := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
projectSpecPath

	^ projectSpecPath
%

category: 'accessing'
method: RwProjectSpecificationV2
projectSpecPath: aString
	projectSpecPath := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
projectsRoot
	^ self repositoryRoot / self projectsPath
%

category: 'accessing'
method: RwProjectSpecificationV2
repoType
	^ repoType ifNil: [ repoType := #disk ]
%

category: 'accessing'
method: RwProjectSpecificationV2
repoType: aSymbol
	(#(#'disk' #'git') includes: aSymbol asSymbol)
		ifFalse: [ self error: 'Unknown repo type ' , aSymbol asSymbol printString ].
	^ repoType := aSymbol asSymbol
%

category: 'accessing'
method: RwProjectSpecificationV2
specName

	^ specName
%

category: 'accessing'
method: RwProjectSpecificationV2
specName: aString
	specName := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
specsPath

	^ specsPath
%

category: 'accessing'
method: RwProjectSpecificationV2
specsPath: aString
	specsPath := aString
%

category: 'accessing'
method: RwProjectSpecificationV2
specsRoot
	^ self repositoryRoot / self specsPath
%

category: 'accessing'
method: RwProjectSpecificationV2
_repoType
	"direct access to IV ... used by ="

	^ repoType
%

category: 'actions'
method: RwProjectSpecificationV2
_resolve
	"answer true if attaching to an existing repository"

	^ self repositoryDefinition resolve
		ifTrue: [ 
			self _checkAndUpdateRepositoryRevision.
			self _checkProjectDirectoryStructure ]
		ifFalse: [ false ]
%

category: 'private'
method: RwProjectSpecificationV2
_validate
	"ensure that the data structures within the receiver contain valid information:
		1. platform implementation is responsible for validating platform structures"

	#(#'componentsPath' #'packagesPath' #'projectsPath' #'specsPath' #'projectName' #'specName' #'projectSpecPath')
		do: [ :messageName | 
			(self perform: messageName)
				ifNil: [ 
					Error
						signal:
							'The instance variable ' , messageName asString printString , ' cannot be nil' ] ].
	^ true
%

! Class implementation for 'RwUrl'

!		Class methods for 'RwUrl'

category: 'instance creation'
classmethod: RwUrl
fromString: aString
  "Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

  "RwUrl fromString: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'"

  "RwUrl fromString: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"

  "RwUrl fromString: 'telnet:chaos.resnet.gatech.edu'"

  "RwUrl fromString: 'file:/etc/passwd'"

  | remainder index scheme fragment newUrl |
  remainder := aString trimSeparators.
  index := remainder indexOf: $#.
  index > 0
    ifTrue: [ 
      "extract the fragment, if any"
      fragment := remainder copyFrom: index + 1 to: remainder size.
      remainder := remainder copyFrom: 1 to: index - 1 ].
  scheme := self schemeNameForString: remainder.
  newUrl := (self urlClassForScheme: scheme) new
    privateInitializeFromText: remainder.
  newUrl privateFragment: fragment.
  ^ newUrl
%

category: 'constants'
classmethod: RwUrl
schemeName

	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^ #'none'
%

category: 'parsing'
classmethod: RwUrl
schemeNameForString: aString
	"Get the scheme name from a string, or return nil if it's not specified. 
	Used in internal parsing routines - an outsider may as well use asUrl. 
	Return scheme in lowercases."
	
	"Url schemeNameForString: 'http://www.yahoo.com'"
	"Url schemeNameForString: '/etc/passwed'"
	"Url schemeNameForString: '/etc/testing:1.2.3'"

	| index schemeName |
	index := aString indexOf: $: ifAbsent: [^ nil].
	schemeName := aString copyFrom: 1 to: index - 1.
	(schemeName allSatisfy: [:each | each isLetter]) ifFalse: [^ nil].
	^ schemeName asLowercase
%

category: 'parsing'
classmethod: RwUrl
urlClassForScheme: scheme
	"explicitly list the classes to be matched: https://github.com/dalehenrich/Rowan/issues/211"

	^ {RwFileUrl.
	RwCypressUrl.
	RwTonelUrl.
	RwFiletreeUrl.
	RwGithubUrl.
	RwGenericUrl.
	RwHierarchicalUrl.
	RwSmalltalkRepositoryUrl.
	RwGitFileTreeUrl.
	RwHttpUrl.
	RwHttpsUrl}
		detect: [ :urlClass | urlClass schemeName = scheme ]
		ifNone: [ RwGenericUrl ]
%

!		Instance methods for 'RwUrl'

category: 'converting'
method: RwUrl
asRwRepository
  "return a platform appropriate repository for the repository identified in the receiver"

  ^ self subclassResponsibility: #'asRwRepository'
%

category: 'converting'
method: RwUrl
asRwUrl
  ^ self
%

category: 'converting'
method: RwUrl
asString

	^self printString
%

category: 'converting'
method: RwUrl
asUrl
	^self
%

category: 'accessing'
method: RwUrl
authority
	^''
%

category: 'fragment'
method: RwUrl
fragment
	^fragment
%

category: 'accessing'
method: RwUrl
pathFor: coll
  | ans sz |
  ans := WriteStreamPortable on: String new.
  sz := coll size.
  1 to: sz do: [ :index | 
    | pathElem |
    pathElem := coll at: index.
    ans nextPutAll: pathElem.
    index < sz
      ifTrue: [ ans nextPut: $/ ] ].
  ^ ans contents
%

category: 'printing'
method: RwUrl
printOn: aStream

	^self subclassResponsibility: #printOn:
%

category: 'fragment'
method: RwUrl
privateFragment: aString
	fragment := aString
%

category: 'parsing'
method: RwUrl
privateInitializeFromText: aString

	^self subclassResponsibility: #privateInitializeFromText:
%

category: 'parsing'
method: RwUrl
privateInitializeFromText: aString relativeTo: aUrl
	"initialize from the given string, as a relative URL.  aString will have had the scheme name removed, if it was present to begin with.  If it was, then the scheme name was the same as the receiver's scheme name"

	"by default, just do regular initialization"
	^self privateInitializeFromText: aString
%

category: 'classification'
method: RwUrl
scheme
	"return a string with the scheme of this URL.  For instance, HTTP"

	^self subclassResponsibility: #scheme
%

! Class implementation for 'RwFileUrl'

!		Class methods for 'RwFileUrl'

category: 'instance creation'
classmethod: RwFileUrl
fromString: aString
	"Method that can be called explicitly to create a FileUrl."

	^self new privateInitializeFromText: aString
%

category: 'constants'
classmethod: RwFileUrl
schemeName
	^'file'
%

!		Instance methods for 'RwFileUrl'

category: 'converting'
method: RwFileUrl
asRwRepository
  "return a platform appropriate repository for the repository located in the directory referenced by the receiver"

  ^ self createRwRepositoryForPath: self pathForDirectory
%

category: 'accessing'
method: RwFileUrl
fileName
	"Return the last part of the path,
	most often a filename but can also be a directory."

	^self path last
%

category: 'testing'
method: RwFileUrl
firstPartIsDriveLetter
	"Return true if the first part of the path is a letter
	followed by a $: like 'C:' "
	
	| firstPart |
	path isEmpty ifTrue: [^false].
	firstPart := path first.
	^firstPart size = 2 and: [
		firstPart first isLetter
			and: [firstPart last = $:]]
%

category: 'accessing'
method: RwFileUrl
host
	"Return the host name, either 'localhost', '', or a fully qualified domain name."
	
	^host ifNil: ['']
%

category: 'accessing'
method: RwFileUrl
host: hostName
	"Set the host name, either 'localhost', '', or a fully qualified domain name."
	
	host := hostName
%

category: 'private-initialization'
method: RwFileUrl
host: aHostString pathParts: aCollection isAbsolute: aBoolean

	host := aHostString.
	path := aCollection.
	isAbsolute := aBoolean
%

category: 'private-initialization'
method: RwFileUrl
initializeFromPathString: aPathString
	"<aPathString> is a file path as a String.
	We construct a path collection using various heuristics."

	| pathString hasDriveLetter |
	pathString := aPathString.
	pathString isEmpty ifTrue: [pathString := '/'].
	"Copy without empty string preceeding first / or between duplicated /s."
	path := ((pathString subStrings: '/') copyWithout: '') collect: [:token | token unescapePercents].

	"A path like 'C:' refers in practice to 'c:/'"
	((pathString endsWith: '/') or:
		[(hasDriveLetter := self firstPartIsDriveLetter) and: [path size = 1]])
			ifTrue: [path add: ''].

	"Decide if we are absolute by checking for leading $/ or
	beginning with drive letter. Smarts for other OSes?"
	self isAbsolute: ((pathString beginsWith: '/')
						or: [hasDriveLetter ifNil: [self firstPartIsDriveLetter]])
%

category: 'accessing'
method: RwFileUrl
isAbsolute
	"Should the path be considered absolute to
	the filesystem instead of relative to the default directory?"
 
	^isAbsolute
%

category: 'accessing'
method: RwFileUrl
isAbsolute: aBoolean
	"Set if the path should be considered absolute to
	the filesystem instead of relative to the default directory."

	isAbsolute := aBoolean
%

category: 'accessing'
method: RwFileUrl
path
	"Return an ordered collection of the path elements."
	
	^path
%

category: 'accessing'
method: RwFileUrl
path: aCollection
	"Set the collection of path elements."

	path := aCollection
%

category: 'paths'
method: RwFileUrl
pathDirString
	"Path to directory as url, using slash as delimiter.
	Filename is left out."

	^String streamContents: [ :s |
		isAbsolute ifTrue: [ s nextPut: $/ ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPut: $/]]
%

category: 'paths'
method: RwFileUrl
pathForDirectory
	"Path using local file system's pathname delimiter.
	DOS paths with drive letters should not
	be prepended with a delimiter even though
	they are absolute. Filename is left out."

	| delimiter |
	delimiter :=  '/'.
	^String streamContents: [ :s |
		(self isAbsolute and: [self firstPartIsDriveLetter not])
			ifTrue: [ s nextPutAll: delimiter ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPutAll: delimiter]]
%

category: 'paths'
method: RwFileUrl
pathForFile
  ^ self pathString
%

category: 'private-initialization'
method: RwFileUrl
pathParts: aCollection isAbsolute: aBoolean

	^self host: nil pathParts: aCollection isAbsolute: aBoolean
%

category: 'paths'
method: RwFileUrl
pathString
	"Path as it appears in a URL with $/ as delimiter."
	
	
	^String streamContents: [ :s | | first |
		self isAbsolute ifTrue:[ s nextPut: $/ ].
		first := true.
		self path do: [ :p |
			first ifFalse: [ s nextPut: $/ ].
			first := false.
			s nextPutAll: p  ] ]
%

category: 'copying'
method: RwFileUrl
postCopy
	"Be sure not to share the path with the copy."

	super postCopy.
	path := path copy
%

category: 'printing'
method: RwFileUrl
printOn: aStream
	"Return the FileUrl according to RFC3986
		'file:'['//'<host>]<path>#<fragment>
	Note that <host> being '' is equivalent to 'localhost' and is not printed."

	aStream nextPutAll: self schemeName;
		nextPut: $:.

	"File URLs with hosts (which are fairly useless) cannot be relative."
	host isEmpty ifFalse: [isAbsolute ifFalse: [aStream nextPutAll: '<ErroneousURL>'. ^nil].
						aStream nextPutAll: '//';
						nextPutAll: host].

	aStream
		nextPutAll: self pathString.

	fragment ifNotNil:
		[aStream
			nextPut: $#;
			nextPutAll: fragment ].
%

category: 'private-initialization'
method: RwFileUrl
privateInitializeFromText: aString
	"Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

	| schemeName pathString bare i |
	bare := aString trimSeparators.
	schemeName := RwUrl schemeNameForString: bare.
	(schemeName == nil  or: [schemeName ~= self schemeName])
		ifTrue: [
			host := ''.
			pathString := bare]
		ifFalse: [
			"First remove schemeName and colon"
			bare := bare copyFrom: (schemeName size + 2) to: bare size.
			"A proper file URL then has two slashes before host,
			A malformed URL is interpreted as using syntax file:<path>."
			(bare beginsWith: '//')
				ifTrue: [i := bare indexOf: $/ startingAt: 3.
						i=0 ifTrue: [
								host := bare copyFrom: 3 to: bare size.
								pathString := '']
							ifFalse: [
								host := bare copyFrom: 3 to: i-1.
								pathString := bare copyFrom: host size + 3 to: bare size]]
				ifFalse: [host := ''.
						pathString := bare]].
	self initializeFromPathString: pathString
%

category: 'private-initialization'
method: RwFileUrl
privateInitializeFromText: pathString relativeTo: aUrl
	"<pathString> should be a filesystem path.
	This url is adjusted to be aUrl + the path."

	| newPath |
	self host: aUrl host.
	self initializeFromPathString: pathString.
	self isAbsolute: aUrl isAbsolute.

	newPath := aUrl path copy.
	newPath removeLast.	"empty string that says its a directory"
	path do: [ :token |
		((token ~= '..') and: [token ~= '.']) ifTrue: [ 
			newPath addLast: token unescapePercents ].
		token = '..' ifTrue: [ 
			newPath isEmpty ifFalse: [ 
				newPath last = '..' ifFalse: [ newPath removeLast ] ] ].
		"token = '.' do nothing" ].
	path := newPath

	
%

category: 'classification'
method: RwFileUrl
scheme
	^self class schemeName
%

category: 'classification'
method: RwFileUrl
schemeName
	^self class schemeName
%

category: 'accessing'
method: RwFileUrl
segments

	^ path
%

! Class implementation for 'RwCypressUrl'

!		Class methods for 'RwCypressUrl'

category: 'constants'
classmethod: RwCypressUrl
schemeName
  ^ 'cypress'
%

! Class implementation for 'RwTonelUrl'

!		Class methods for 'RwTonelUrl'

category: 'constants'
classmethod: RwTonelUrl
schemeName
	^'tonel'
%

!		Instance methods for 'RwTonelUrl'

category: 'converting'
method: RwTonelUrl
asRwRepository
  "return a platform appropriate repository for the repository located in the directory referenced by the receiver"

  ^ self createRwTonelRepositoryForPath: self pathForDirectory
%

! Class implementation for 'RwFiletreeUrl'

!		Class methods for 'RwFiletreeUrl'

category: 'constants'
classmethod: RwFiletreeUrl
schemeName
	^'filetree'
%

!		Instance methods for 'RwFiletreeUrl'

category: 'converting'
method: RwFiletreeUrl
asRwRepository
  "return a platform appropriate repository for the repository located in the directory referenced by the receiver"

  ^ self createRwFiletreeRepositoryForPath: self pathForDirectory
%

category: 'printing'
method: RwFiletreeUrl
printOn: aStream
  aStream
    nextPutAll: self schemeName;
    nextPut: $:.
  aStream nextPutAll: '//'.
  aStream nextPutAll: self pathString
%

category: 'private-initialization'
method: RwFiletreeUrl
privateInitializeFromText: aString
  "Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

  | schemeName pathString bare |
  host := ''.
  bare := aString trimSeparators.
  schemeName := RwUrl schemeNameForString: bare.
  (schemeName == nil or: [ schemeName ~= self schemeName ])
    ifTrue: [ pathString := bare ]
    ifFalse: [ 
      "First remove schemeName and colon"
      bare := bare copyFrom: schemeName size + 2 to: bare size.
      (bare beginsWith: '//')
        ifTrue: [ pathString := bare copyFrom: 3 to: bare size ]
        ifFalse: [ pathString := bare ] ].
  self initializeFromPathString: pathString
%

! Class implementation for 'RwGithubUrl'

!		Class methods for 'RwGithubUrl'

category: 'constants'
classmethod: RwGithubUrl
schemeName
  ^ 'github'
%

!		Instance methods for 'RwGithubUrl'

category: 'converting'
method: RwGithubUrl
asRwRepository
  "return a platform appropriate repository for the repository identified in the receiver"

  ^ self halt: 'not yet implemented'
%

category: 'accessing'
method: RwGithubUrl
dirPath
  ^ self pathFor: dir
%

category: 'accessing'
method: RwGithubUrl
isAbsolute
	"Should the path be considered absolute to
	the filesystem instead of relative to the default directory?"
 
	^false
%

category: 'printing'
method: RwGithubUrl
printOn: aStream
  aStream
    nextPutAll: self schemeName;
    nextPut: $:.
  aStream nextPutAll: '//'.
  aStream nextPutAll: self pathString
%

category: 'private-initialization'
method: RwGithubUrl
privateInitializeFromText: aString
  | parsedPath |
  super privateInitializeFromText: aString.
  parsedPath := {host} , path.
  (parsedPath
    detect: [ :e | e includes: $: ]
    ifNone: [ 
      project := parsedPath copyFrom: 1 to: parsedPath size - 1.
      dir := parsedPath copyFrom: 2 to: parsedPath size ])
    ifNotNil: [ :pivot | 
      project := parsedPath copyFrom: 1 to: (parsedPath indexOf: pivot) - 1.
      project addLast: (pivot copyUpTo: $:).
      committish := pivot copyFrom: (pivot indexOf: $:) + 1 to: pivot size.
      dir := parsedPath
        copyFrom: (parsedPath indexOf: pivot) + 1
        to: parsedPath size ].
  path := parsedPath.
  host := ''
%

category: 'accessing'
method: RwGithubUrl
projectPath
  ^ self pathFor: project
%

! Class implementation for 'RwGenericUrl'

!		Class methods for 'RwGenericUrl'

category: 'parsing'
classmethod: RwGenericUrl
fromString: aString
	| schemeName locator |
	schemeName := RwUrl schemeNameForString: aString.
	schemeName ifNil: [ ^self schemeName: 'xnoscheme' locator: aString ].
	locator := aString copyFrom: (schemeName size + 2) to: aString size.
	^self schemeName: schemeName locator: locator
%

category: 'instance creation'
classmethod: RwGenericUrl
schemeName: schemeName  locator: locator
	^self new schemeName: schemeName  locator: locator
%

!		Instance methods for 'RwGenericUrl'

category: 'access'
method: RwGenericUrl
locator
	^locator
%

category: 'paths'
method: RwGenericUrl
pathForDirectory

	^self locator
%

category: 'printing'
method: RwGenericUrl
printOn: aStream
	
	self schemeName ifNotNil: [
		aStream nextPutAll: self schemeName; nextPut: $:].
	
	aStream nextPutAll: self locator.

	self fragment ifNotNil: [
		aStream nextPut: $#; nextPutAll: self fragment].
%

category: 'parsing'
method: RwGenericUrl
privateInitializeFromText: aString
	schemeName := RwUrl schemeNameForString: aString.
	locator :=  schemeName 
		ifNil: [ aString ]
		ifNotNil: [ aString copyFrom: (schemeName size+2) to: aString size ].
%

category: 'parsing'
method: RwGenericUrl
privateInitializeFromText: aString relativeTo: aUrl
	schemeName := aUrl schemeName.
	locator := aString.
%

category: 'classification'
method: RwGenericUrl
scheme
	^ self schemeName.
%

category: 'access'
method: RwGenericUrl
schemeName
	^schemeName
%

category: 'private'
method: RwGenericUrl
schemeName: schemeName0  locator: locator0
	schemeName := schemeName0.
	locator := locator0.
%

! Class implementation for 'RwHierarchicalUrl'

!		Class methods for 'RwHierarchicalUrl'

category: 'instance creation'
classmethod: RwHierarchicalUrl
schemeName: schemeName  authority: authority  path: path  query: query
	^self new schemeName: schemeName  authority: authority  path: path  query: query
%

!		Instance methods for 'RwHierarchicalUrl'

category: 'access'
method: RwHierarchicalUrl
authority
	^authority
%

category: 'printing'
method: RwHierarchicalUrl
fullPath
  | ans |
  ans := WriteStreamPortable on: String new.
  path
    do: [ :pathElem | 
      ans nextPut: $/.
      ans nextPutAll: pathElem ].
  self query == nil
    ifFalse: [ 
      ans nextPut: $?.
      ans nextPutAll: self query ].
  self fragment == nil
    ifFalse: [ 
      ans nextPut: $#.
      ans nextPutAll: self fragment ].
  ^ ans contents
%

category: 'access'
method: RwHierarchicalUrl
isAbsolute
	
	path size > 0 ifFalse: [^ false].
	(path at: 1) size > 0 ifFalse: [^ false].
	^ ((path at: 1) at: 1) ~~ $.
%

category: 'access'
method: RwHierarchicalUrl
password
	"http://user:pword@foo.com' asUrl password"
	^password
%

category: 'access'
method: RwHierarchicalUrl
path
	"return a collection of the decoded path elements, as strings"
	^path
%

category: 'access'
method: RwHierarchicalUrl
path: aCollection
	"Set the collection of path elements."

	path := aCollection
%

category: 'access'
method: RwHierarchicalUrl
port
	^port
%

category: 'copying'
method: RwHierarchicalUrl
postCopy
	"Be sure not to share the path with the copy"

	super postCopy.
	path := path copy
%

category: 'printing'
method: RwHierarchicalUrl
printOn: aStream
  aStream nextPutAll: self schemeName.
  aStream nextPutAll: '://'.
  self username
    ifNotNil: [ 
      aStream nextPutAll: self username.
      self password
        ifNotNil: [ 
          aStream nextPutAll: ':'.
          aStream nextPutAll: self password ].
      aStream nextPutAll: '@' ].
  aStream nextPutAll: self authority.
  port
    ifNotNil: [ 
      aStream
        nextPut: $:;
        nextPutAll: port printString ].
  path
    do: [ :pathElem | 
      aStream nextPut: $/.
      aStream nextPutAll: pathElem ].
  self query == nil
    ifFalse: [ 
      aStream nextPut: $?.
      aStream nextPutAll: self query ].
  self fragment == nil
    ifFalse: [ 
      aStream nextPut: $#.
      aStream nextPutAll: self fragment ]
%

category: 'parsing'
method: RwHierarchicalUrl
privateInitializeFromText: aString

	| remainder ind specifiedSchemeName |
	remainder := aString.
	schemeName
		ifNil: [ 
			specifiedSchemeName := RwUrl schemeNameForString: remainder.
			specifiedSchemeName
				ifNotNil: [ 
					schemeName := specifiedSchemeName.
					remainder := remainder copyFrom: schemeName size + 2 to: remainder size ].
			schemeName
				ifNil: [ 
					"assume HTTP"
					schemeName := 'http' ] ].	"remove leading // if it's there"
	(remainder beginsWith: '//')
		ifTrue: [ remainder := remainder copyFrom: 3 to: remainder size ].	"get the query"
	ind := remainder indexOf: $?.
	ind > 0
		ifTrue: [ 
			query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1 ].	"get the authority"
	ind := remainder indexOf: $/.
	ind > 0
		ifTrue: [ 
			ind = 1
				ifTrue: [ authority := '' ]
				ifFalse: [ 
					authority := remainder copyFrom: 1 to: ind - 1.
					remainder := remainder copyFrom: ind + 1 to: remainder size ] ]
		ifFalse: [ 
			authority := remainder.
			remainder := '' ].	"extract the username+password"
	(authority includes: $@)
		ifTrue: [ 
			username := authority copyUpTo: $@.
			authority := authority
				copyFrom: (authority indexOf: $@) + 1
				to: authority size.
			(username includes: $:)
				ifTrue: [ 
					password := (username copyFrom: (username indexOf: $:) + 1 to: username size)
						unescapePercents asByteArray decodeFromUTF8 asString.
					username := (username copyUpTo: $:) unescapePercents asByteArray
						decodeFromUTF8 asString ]
				ifFalse: [ 
					password := nil.
					username := username unescapePercents asByteArray decodeFromUTF8 asString ] ].	"Extract the port"
	(authority includes: $:)
		ifTrue: [ 
			| lastColonIndex portString |
			lastColonIndex := authority findLast: [ :c | c = $: ].
			portString := authority copyFrom: lastColonIndex + 1 to: authority size.
			(portString size > 0) 
				ifTrue: [
					(portString allSatisfy: [ :each | each isDigit ])
						ifTrue: [ 
							port := Integer fromString: portString.
							port > 65535
								ifTrue: [ self error: 'Invalid port number' ] ]
						ifFalse: [ self error: 'Invalid port number' ] ].
			authority := authority copyFrom: 1 to: lastColonIndex - 1 ].	

	"get the path"
	path := self privateParsePath: remainder relativeTo: #()
%

category: 'parsing'
method: RwHierarchicalUrl
privateParsePath: remainder relativeTo: basePath 
	| nextTok s parsedPath |
	s := remainder readStream.
	parsedPath := OrderedCollection new.
	parsedPath addAll: basePath.
	parsedPath isEmpty ifFalse: [ parsedPath removeLast ].
	
	[ s peek = $/ ifTrue: [ s next ].
	nextTok := WriteStreamPortable on: String new.
	[ s atEnd or: [ s peek = $/ ] ] whileFalse: [ nextTok nextPut: s next ].
	nextTok := nextTok contents unescapePercents.
	nextTok = '..' 
		ifTrue: [ parsedPath size > 0 ifTrue: [ parsedPath removeLast ] ]
		ifFalse: [ nextTok ~= '.' ifTrue: [ parsedPath add: nextTok ] ].
	s atEnd ] whileFalse.
	parsedPath isEmpty ifTrue: [ parsedPath add: '' ].
	^ parsedPath
%

category: 'access'
method: RwHierarchicalUrl
query
	"return the query, the part after any ?.  Any %XY's have already been decoded.  If there wasno query part, nil is returned (it is possible to also have an empty query"
	^query 
%

category: 'classification'
method: RwHierarchicalUrl
scheme
	^ self schemeName.
%

category: 'access'
method: RwHierarchicalUrl
schemeName
	^schemeName
%

category: 'private'
method: RwHierarchicalUrl
schemeName: schemeName0  authority: authority0  path: path0  query: query0
	"initialize a new instance"
	schemeName := schemeName0.
	authority := authority0.
	path := path0.
	query := query0.
%

category: 'access'
method: RwHierarchicalUrl
segments

	^ path
%

category: 'access'
method: RwHierarchicalUrl
username
	"http://user:pword@foo.com' asUrl username"
	^username
%

! Class implementation for 'RwHttpUrl'

!		Class methods for 'RwHttpUrl'

category: 'constants'
classmethod: RwHttpUrl
schemeName
	^'http'
%

! Class implementation for 'RwHttpsUrl'

!		Class methods for 'RwHttpsUrl'

category: 'constants'
classmethod: RwHttpsUrl
schemeName
	^'https'
%

! Class implementation for 'RwSmalltalkRepositoryUrl'

!		Class methods for 'RwSmalltalkRepositoryUrl'

category: 'constants'
classmethod: RwSmalltalkRepositoryUrl
schemeName
  ^ 'smalltalk'
%

!		Instance methods for 'RwSmalltalkRepositoryUrl'

category: 'converting'
method: RwSmalltalkRepositoryUrl
asRwRepository
  "return a platform appropriate repository for the repository identified in the receiver"

  ^ self halt: 'not yet implemented'
%

category: 'accessing'
method: RwSmalltalkRepositoryUrl
committish

   ^committish
%

category: 'accessing'
method: RwSmalltalkRepositoryUrl
dir

   ^dir
%

category: 'accessing'
method: RwSmalltalkRepositoryUrl
dirPath
  ^ self pathFor: dir
%

category: 'parsing'
method: RwSmalltalkRepositoryUrl
privateParsePath: remainder relativeTo: basePath
  | parsedPath |
  parsedPath := super privateParsePath: remainder relativeTo: basePath.
  (parsedPath
    detect: [ :e | e includes: $: ]
    ifNone: [ 
      project := parsedPath copyFrom: 1 to: parsedPath size - 1.
      dir := parsedPath copyFrom: 2 to: parsedPath size ])
    ifNotNil: [ :pivot | 
      project := parsedPath copyFrom: 1 to: (parsedPath indexOf: pivot) - 1.
      project addLast: (pivot copyUpTo: $:).
      committish := pivot copyFrom: (pivot indexOf: $:) + 1 to: pivot size.
      dir := parsedPath
        copyFrom: (parsedPath indexOf: pivot) + 1
        to: parsedPath size ].
  ^ parsedPath
%

category: 'accessing'
method: RwSmalltalkRepositoryUrl
project

   ^project
%

category: 'accessing'
method: RwSmalltalkRepositoryUrl
projectPath
  ^ self pathFor: project
%

! Class implementation for 'RwGitFileTreeUrl'

!		Class methods for 'RwGitFileTreeUrl'

category: 'constants'
classmethod: RwGitFileTreeUrl
schemeName
  ^ 'gitfiletree'
%

! Class implementation for 'STON'

!		Class methods for 'STON'

category: 'convenience'
classmethod: STON
fromStream: readStream
	^ (self reader on: readStream) next
%

category: 'convenience'
classmethod: STON
fromString: string
  ^ self fromStream: string readStream
%

category: 'accessing'
classmethod: STON
jsonWriter
	^ STONWriter new
		  jsonMode: true;
		  yourself
%

category: 'accessing'
classmethod: STON
listClass
	^ Array
%

category: 'accessing'
classmethod: STON
mapClass
	^ Dictionary
%

category: 'convenience'
classmethod: STON
put: object asJsonOnStream: stream
	(self jsonWriter on: stream) nextPut: object
%

category: 'convenience'
classmethod: STON
put: object asJsonOnStreamPretty: stream
	(self jsonWriter on: stream)
		prettyPrint: true; 
		nextPut: object
%

category: 'convenience'
classmethod: STON
put: object onStream: stream
	(self writer on: stream) nextPut: object
%

category: 'convenience'
classmethod: STON
put: object onStreamPretty: stream
	(self writer on: stream)
		prettyPrint: true; 
		nextPut: object
%

category: 'accessing'
classmethod: STON
reader
	^ STONReader new
%

category: 'convenience'
classmethod: STON
toJsonString: object
  ^ String streamContents: [ :stream | self put: object asJsonOnStream: stream ]
%

category: 'convenience'
classmethod: STON
toJsonStringPretty: object
  ^ String
    streamContents: [ :stream | self put: object asJsonOnStreamPretty: stream ]
%

category: 'convenience'
classmethod: STON
toString: object
  ^ String streamContents: [ :stream | self put: object onStream: stream ]
%

category: 'convenience'
classmethod: STON
toStringPretty: object
  ^ String streamContents: [ :stream | self put: object onStreamPretty: stream ]
%

category: 'accessing'
classmethod: STON
writer
	^ STONWriter new
%

! Class implementation for 'STONReader'

!		Class methods for 'STONReader'

category: 'instance creation'
classmethod: STONReader
on: readStream
	^ self new
		on: readStream;
		yourself
%

!		Instance methods for 'STONReader'

category: 'initialize-release'
method: STONReader
allowComplexMapKeys: boolean
	allowComplexMapKeys := boolean
%

category: 'testing'
method: STONReader
atEnd
	^ readStream atEnd
%

category: 'initialize-release'
method: STONReader
classes

	^ classes
%

category: 'initialize-release'
method: STONReader
close
	readStream ifNotNil: [
		readStream close.
		readStream := nil ]
%

category: 'private'
method: STONReader
consumeWhitespace
	"Strip whitespaces from the input stream."

	[ readStream atEnd not and: [ readStream peek isSeparator ] ]
		whileTrue: [ readStream next ]
%

category: 'error handling'
method: STONReader
error: aString
	| streamPosition |
	"Remain compatible with streams that don't understand #position"
	streamPosition := [ readStream position ]
		on: MessageNotUnderstood do: [ nil ].
	^ STONReaderError signal: aString streamPosition: streamPosition
%

category: 'private'
method: STONReader
expectChar: character
	"Expect character and consume input and optional whitespace at the end,
	 throw an error otherwise."

	(self matchChar: character)
		ifFalse: [ self error: character asString, ' expected' ]
%

category: 'initialize-release'
method: STONReader
initialize
  objects := IdentityDictionary new.
  classes := IdentityDictionary new.
  allowComplexMapKeys := false.
  stack := OrderedCollection new.
  unresolvedReferences := 0
%

category: 'private'
method: STONReader
isClassChar: char
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' includes: char
%

category: 'private'
method: STONReader
isClassStartChar: char
	^ 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' includes: char
%

category: 'private'
method: STONReader
isSimpleSymbolChar: char
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char
%

category: 'private'
method: STONReader
match: string do: block
	"Try to read and consume string and execute block if successful.
	Else do nothing (but do not back up)"

	(string allSatisfy: [ :each | readStream peekFor: each ])
		ifTrue: [ 
			self consumeWhitespace.
			block value ]
%

category: 'private'
method: STONReader
matchChar: character
	"Tries to match character, consume input and 
	answer true if successful and consumes whitespace at the end."

	^ (readStream peekFor: character)
		ifTrue: [ 
			self consumeWhitespace.
			true ]
		ifFalse: [ false ]
%

category: 'private'
method: STONReader
newReference
	| index reference |
	index := objects size + 1.
	reference := STONReference index: index.
	objects at: index put: reference.
	^ reference
%

category: 'public'
method: STONReader
next
	| object |
	self consumeWhitespace.
	object := self parseValue.
	unresolvedReferences > 0
		ifTrue: [ self processSubObjectsOf: object ].
	^ object
%

category: 'initialize-release'
method: STONReader
on: aReadStream
	readStream := aReadStream
%

category: 'parsing-internal'
method: STONReader
parseCharacter
  | char |
  (char := readStream next) = $\
    ifFalse: [ ^ char ].
  (#($' $" $/ $\) includes: (char := readStream next))
    ifTrue: [ ^ char ].
  char = $b
    ifTrue: [ ^ Character backspace ].
  char = $f
    ifTrue: [ ^ Character newPage ].
  char = $n
    ifTrue: [ ^ Character lf ].
  char = $r
    ifTrue: [ ^ Character cr ].
  char = $t
    ifTrue: [ ^ Character tab ].
  char = $u
    ifTrue: [ ^ self parseCharacterHex ].
  self error: 'invalid escape character \' , (String with: char)
%

category: 'parsing-internal'
method: STONReader
parseCharacterHex
  | value |
  value := self parseCharacterHexDigit.
  3 timesRepeat: [ value := (value bitShift: 4) + self parseCharacterHexDigit ].
  ^ Character codePoint: value
%

category: 'parsing-internal'
method: STONReader
parseCharacterHexDigit
	| digit |
	readStream atEnd ifFalse: [ 
		digit := readStream next asInteger.
		(digit between: "$0" 48 and: "$9" 57)
			ifTrue: [ ^ digit - 48 ].
		(digit between: "$A" 65 and: "$F" 70)
			ifTrue: [ ^ digit - 55 ].
		(digit between: "$a" 97 and: "$f" 102)
			ifTrue: [ ^ digit - 87 ] ].
	self error: 'hex-digit expected'
%

category: 'parsing-internal'
method: STONReader
parseClass
	| className |
	className := self stringStreamContents: [ :stream |
		[ readStream atEnd not and: [ self isClassChar: readStream peek ] ] whileTrue: [ 
			stream nextPut: readStream next ] ].
	self consumeWhitespace.
	^ self lookupClass: className asSymbol
%

category: 'parsing-internal'
method: STONReader
parseConstantDo: block
	"Parse and consume either true|false|nil|null and execute block 
	or else do nothing (but do not back up).
	Hand written implementation to avoid the use of #position:"
	
	(readStream peek = $t)
		ifTrue: [
			^ self match: 'true' do: [ block value: true ] ].
	(readStream peek = $f)
		ifTrue: [
			^ self match: 'false' do: [ block value: false ] ].
	(readStream peek = $n)
		ifTrue: [
			readStream next.
			(readStream peek = $i)
				ifTrue: [
					self match: 'il' do: [ block value: nil ] ].
			(readStream peek = $u)
				ifTrue: [
					self match: 'ull' do: [ block value: nil ] ] ]
%

category: 'parsing'
method: STONReader
parseList
	| reference array |
	reference := self newReference.
	array := STON listClass streamContents: [ :stream |
		self parseListDo: [ :each | stream nextPut: each ] ].
	self setReference: reference to: array.
	^ array
%

category: 'parsing'
method: STONReader
parseListDo: block
	| index |
	self expectChar: $[.
	(self matchChar: $]) 
		ifTrue: [ ^ self ].
	index := 1.
	[ readStream atEnd ] whileFalse: [
		block cull: self parseValue cull: index.
		(self matchChar: $]) 
			ifTrue: [ ^ self ].
		index := index + 1.
		self expectChar: $, ].
	self error: 'end of list expected'
%

category: 'parsing'
method: STONReader
parseListSingleton
	| value |
	value := nil.
	self parseListDo: [ :each :index |
		index = 1 ifTrue: [ value := each ] ].
	^ value
%

category: 'parsing'
method: STONReader
parseMap
	| map |
	map := STON mapClass new.
	self storeReference: map.
	self parseMapDo: [ :key :value |
		map at: key put: value ].
	^ map
%

category: 'parsing'
method: STONReader
parseMapDo: block
  self expectChar: ${.
  (self matchChar: $})
    ifTrue: [ ^ self ].
  [ readStream atEnd ] whileFalse: [ | name value |
      name := self parseValue.
      (allowComplexMapKeys
        or: [ name isString or: [ name isNumber ] ])
        ifFalse: [ self error: 'unexpected property name type' ].
      self expectChar: $:.
      value := self parseValue.
      block value: name value: value.
      (self matchChar: $})
        ifTrue: [ ^ self ].
      self expectChar: $, ].
  self error: 'end of map expected'
%

category: 'parsing-internal'
method: STONReader
parseNumber
	| negated number |
	negated := readStream peekFor: $-.
	number := self parseNumberInteger.
	(readStream peekFor: $.)
		ifTrue: [ number := number + self parseNumberFraction ].
	((readStream peekFor: $e) or: [ readStream peekFor: $E ])
		ifTrue: [ number := number * self parseNumberExponent ].
	negated
		ifTrue: [ number := number negated ].
	self consumeWhitespace.
	^ number
%

category: 'parsing-internal'
method: STONReader
parseNumberExponent
	| number negated |
	number := 0.
	(negated := readStream peekFor: $-)
		ifFalse: [ readStream peekFor: $+ ].
	[ readStream atEnd not and: [ readStream peek isDigit ] ]
		whileTrue: [ number := 10 * number + readStream next digitValue ].
	negated
		ifTrue: [ number := number negated ].
	^ 10 raisedTo: number
%

category: 'parsing-internal'
method: STONReader
parseNumberFraction
	| number power |
	number := 0.
	power := 1.0.
	[ readStream atEnd not and: [ readStream peek isDigit ] ] whileTrue: [
		number := 10 * number + readStream next digitValue.
		power := power * 10.0 ].
	^ number / power
%

category: 'parsing-internal'
method: STONReader
parseNumberInteger
	| number |
	number := 0.
	[ readStream atEnd not and: [ readStream peek isDigit ] ] whileTrue: [ 
		number := 10 * number + readStream next digitValue ].
	^ number
%

category: 'parsing'
method: STONReader
parseObject
	| targetClass reference object |
	targetClass := self parseClass.
	reference := self newReference.
	object := targetClass fromSton: self.
	self setReference: reference to: object.
	^ object
%

category: 'parsing-internal'
method: STONReader
parseReference
	| index |
	self expectChar: $@.
	index := self parseNumberInteger.
	self consumeWhitespace.
	unresolvedReferences := unresolvedReferences + 1.
	^ STONReference index: index
%

category: 'parsing-internal'
method: STONReader
parseString
	^ self parseStringInternal
%

category: 'parsing-internal'
method: STONReader
parseStringInternal
  | result delimiter |
  delimiter := readStream next.
  (delimiter = $' or: [ delimiter = $" ])
    ifFalse: [ self error: ''' or " expected' ].
  result := self
    stringStreamContents: [ :stream | 
      [ readStream atEnd or: [ readStream peek = delimiter ] ]
        whileFalse: [ stream nextPut: self parseCharacter ] ].
  self expectChar: delimiter.
  ^ result
%

category: 'parsing-internal'
method: STONReader
parseSymbol
	| string |
	self expectChar: $#.
	readStream peek = $'
		ifTrue: [ ^ self parseStringInternal asSymbol ].
	string := self stringStreamContents: [ :stream |
		[ readStream atEnd not and: [ self isSimpleSymbolChar: readStream peek ] ] whileTrue: [
			stream nextPut: readStream next ] ].
	string isEmpty
		ifFalse: [ 
			self consumeWhitespace.
			^ string asSymbol ].
	self error: 'unexpected input'
%

category: 'parsing'
method: STONReader
parseValue
	| char |
	readStream atEnd ifFalse: [ 
		(self isClassStartChar: (char := readStream peek)) 
			ifTrue: [ ^ self parseObject ].
		char = ${
			ifTrue: [ ^ self parseMap ].
		char = $[
			ifTrue: [ ^ self parseList ].
		(char = $' or: [ char = $" ])
			ifTrue: [ ^ self parseString ].
		char = $#
			ifTrue: [ ^ self parseSymbol ].
		char = $@
			ifTrue: [ ^ self parseReference ].
		(char = $- or: [ char isDigit ])
			ifTrue: [ ^ self parseNumber ].
		self parseConstantDo: [ :value | ^ value ] ].
	self error: 'invalid input'
%

category: 'private'
method: STONReader
processSubObjectsOf: object
  stack addFirst: object.
  [ stack isEmpty ]
    whileFalse: [ stack removeFirst stonProcessSubObjects: [ :each | each isStonReference
            ifTrue: [ self resolveReference: each ]
            ifFalse: [ each stonContainSubObjects
                ifTrue: [ stack addFirst: each ]
                ifFalse: [ each ] ] ] ]
%

category: 'initialize-release'
method: STONReader
reset
	unresolvedReferences := 0.
	objects removeAll
%

category: 'private'
method: STONReader
resolveReference: reference
	^ self resolveReferenceIndex: reference index
%

category: 'private'
method: STONReader
resolveReferenceIndex: index
	^ objects at: index
%

category: 'private'
method: STONReader
setReference: reference to: object
	objects at: reference index put: object
%

category: 'private'
method: STONReader
storeReference: object
	| index |
	index := objects size + 1.
	objects at: index put: object.
	^ index
%

category: 'private'
method: STONReader
stringStreamContents: block
  stringStream ifNil: [ stringStream := WriteStream on: String new ].
  stringStream reset.
  block value: stringStream.
  ^ stringStream contents
%

! Class implementation for 'STONReference'

!		Class methods for 'STONReference'

category: 'instance creation'
classmethod: STONReference
index: integer
	^ self new
		index: integer;
		yourself
%

!		Instance methods for 'STONReference'

category: 'comparing'
method: STONReference
= anObject
	^ self class == anObject class and: [ self index = anObject index ]
%

category: 'comparing'
method: STONReference
hash
	^ index hash
%

category: 'accessing'
method: STONReference
index
	^ index
%

category: 'accessing'
method: STONReference
index: integer
	index := integer
%

category: 'testing'
method: STONReference
isStonReference
	^ true
%

category: 'printing'
method: STONReference
printOn: stream
	super printOn: stream.
	stream nextPut: $(; print: index; nextPut: $)
%

! Class implementation for 'STONStreamWriter'

!		Class methods for 'STONStreamWriter'

category: 'instance creation'
classmethod: STONStreamWriter
on: stonWriter
	^ self new
		on: stonWriter;
		yourself
%

!		Instance methods for 'STONStreamWriter'

category: 'initialize-release'
method: STONStreamWriter
initialize
  first := true
%

category: 'initialize-release'
method: STONStreamWriter
on: stonWriter
	writer := stonWriter
%

! Class implementation for 'STONListWriter'

!		Instance methods for 'STONListWriter'

category: 'accessing'
method: STONListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer listElementSeparator ].
	writer nextPut: anObject
%

! Class implementation for 'STONShortListWriter'

!		Instance methods for 'STONShortListWriter'

category: 'accessing'
method: STONShortListWriter
add: anObject
	first ifTrue: [ first := false ] ifFalse: [ writer shortListElementSeparator ].
	writer nextPut: anObject
%

! Class implementation for 'STONMapWriter'

!		Instance methods for 'STONMapWriter'

category: 'accessing'
method: STONMapWriter
at: key put: value
	first ifTrue: [ first := false ] ifFalse: [ writer mapElementSeparator ].
	writer encodeKey: key value: value
%

! Class implementation for 'STONTestDomainObject'

!		Class methods for 'STONTestDomainObject'

category: 'instance creation'
classmethod: STONTestDomainObject
dummy
	| random atRandom atRandomIndex |
	random :=  HostRandom new.
	atRandom := [:anInt | (random next * anInt) truncated + 1 ].
	atRandomIndex := [:coll | coll at: (atRandom value: coll size) ].
	^ self new
		integer: (atRandom value: 999999);
		float: (atRandom value: 999) / Float pi;
		boolean: (atRandomIndex value: #(true false));
		bytes: (ByteArray streamContents: [ :out | 32 timesRepeat: [ out nextPut: (atRandom value: 255) ] ]);
		description: (String streamContents: [ :out | (atRandom value: 16) timesRepeat: [ out nextPutAll: 'Blah' ] ]);
		color: (atRandomIndex value: #(#red #green #blue));
		tags: (Array 
			with: (atRandomIndex value: #(#one #two #three))
 			with: (atRandomIndex value: #(#alpha #beta #gamma)) 
			with: (atRandomIndex value: #(#low #medium #high)));
		yourself
%

category: 'ston-core'
classmethod: STONTestDomainObject
stonName
	^ #TestDomainObject
%

!		Instance methods for 'STONTestDomainObject'

category: 'comparing'
method: STONTestDomainObject
= anObject
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ color = anObject color
		and: [ 
			modified = anObject modified
				and: [ 
					created = anObject created
						and: [ 
							description = anObject description
								and: [ 
									boolean = anObject boolean
										and: [ 
											(float closeTo: anObject float) 		"Use #closeTo: instead of #= to increase portability"
												and: [ 
													bytes = anObject bytes 
														and: [ 
															integer = anObject integer 
																and: [ tags = anObject tags ] ] ] ] ] ] ] ]
%

category: 'accessing'
method: STONTestDomainObject
boolean
	^ boolean
%

category: 'accessing'
method: STONTestDomainObject
boolean: anObject
	boolean := anObject
%

category: 'accessing'
method: STONTestDomainObject
bytes
	^ bytes
%

category: 'accessing'
method: STONTestDomainObject
bytes: anObject
	bytes := anObject
%

category: 'accessing'
method: STONTestDomainObject
color
	^ color
%

category: 'accessing'
method: STONTestDomainObject
color: anObject
	color := anObject
%

category: 'accessing'
method: STONTestDomainObject
created
	^ created
%

category: 'accessing'
method: STONTestDomainObject
created: anObject
	created := anObject
%

category: 'accessing'
method: STONTestDomainObject
description
	^ description
%

category: 'accessing'
method: STONTestDomainObject
description: anObject
	description := anObject
%

category: 'accessing'
method: STONTestDomainObject
float
	^ float
%

category: 'accessing'
method: STONTestDomainObject
float: anObject
	float := anObject
%

category: 'comparing'
method: STONTestDomainObject
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ color hash
		bitXor:
			(modified hash
				bitXor:
					(created hash
						bitXor:
							(description hash
								bitXor: (boolean hash bitXor: (float hash bitXor: (bytes hash bitXor: (integer hash bitXor: tags hash)))))))
%

category: 'initialize-release'
method: STONTestDomainObject
initialize
  "GemStone DateAndTime uses a float for seconds so serialize/materialize of floats is problematic ... this technique causes a ScaledDecimal to be used which does suffer from Float problems"

  created := modified := DateAndTime fromString: DateAndTime now asString
%

category: 'accessing'
method: STONTestDomainObject
integer
	^ integer
%

category: 'accessing'
method: STONTestDomainObject
integer: anObject
	integer := anObject
%

category: 'accessing'
method: STONTestDomainObject
modified
	^ modified
%

category: 'accessing'
method: STONTestDomainObject
modified: anObject
	modified := anObject
%

category: 'accessing'
method: STONTestDomainObject
tags
	^ tags
%

category: 'accessing'
method: STONTestDomainObject
tags: anObject
	tags := anObject
%

! Class implementation for 'STONTestUser'

!		Class methods for 'STONTestUser'

category: 'instance creation'
classmethod: STONTestUser
dummy
	"self dummy"
	
	| username password random atRandom |
	random :=  HostRandom new.
	atRandom := [:anInt | (random next * anInt) truncated + 1 ].
	username := String streamContents: [ :stream |
		stream << 'user'; print: (atRandom value: 999); << '@company'; print: (atRandom value: 99); << '.com' ].
	password := String streamContents: [ :stream |
		stream << 'secret'; print:  (atRandom value: 999) ].
	^ self new
		username: username;
		password: password;
		yourself
%

category: 'instance creation'
classmethod: STONTestUser
new

	^ self basicNew
		initialize;
		yourself
%

category: 'ston-core'
classmethod: STONTestUser
stonName
	^ #TestUser
%

!		Instance methods for 'STONTestUser'

category: 'comparing'
method: STONTestUser
= anObject
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ username = anObject username and: [ password = anObject password and: [ enabled = anObject enabled ] ]
%

category: 'accessing'
method: STONTestUser
enabled
	^ enabled
%

category: 'accessing'
method: STONTestUser
enabled: anObject
	enabled := anObject
%

category: 'comparing'
method: STONTestUser
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ username hash bitXor: (password hash bitXor: enabled hash)
%

category: 'initialize-release'
method: STONTestUser
initialize 
	enabled := true
%

category: 'accessing'
method: STONTestUser
password
	^ password
%

category: 'accessing'
method: STONTestUser
password: anObject
	password := anObject
%

category: 'accessing'
method: STONTestUser
username
	^ username
%

category: 'accessing'
method: STONTestUser
username: anObject
	username := anObject
%

! Class implementation for 'STONTestUser2'

!		Class methods for 'STONTestUser2'

category: 'ston-core'
classmethod: STONTestUser2
stonName
	^ #TestUser2
%

!		Instance methods for 'STONTestUser2'

category: 'ston-core'
method: STONTestUser2
fromSton: stonReader
	stonReader parseMapDo: [ :key :value |
		key = #username ifTrue: [ username := value ].
		key = #password ifTrue: [ password := value ].
		key = #enabled ifTrue: [ enabled := value ] ]
	
%

category: 'ston-core'
method: STONTestUser2
stonOn: stonWriter	
	stonWriter writeObject: self streamMap: [ :dictionary |
		dictionary
			at: #username put: username;
			at: #password put: password;
			at: #enabled put: enabled ]
%

! Class implementation for 'STONTestUser3'

!		Class methods for 'STONTestUser3'

category: 'ston-core'
classmethod: STONTestUser3
stonName
	^ #TestUser3
%

!		Instance methods for 'STONTestUser3'

category: 'ston-core'
method: STONTestUser3
stonShouldWriteNilInstVars
	^ true
%

! Class implementation for 'STONWriter'

!		Class methods for 'STONWriter'

category: 'class initialization'
classmethod: STONWriter
initialize
	self initializeSTONCharacters.
	self initializeSTONSimpleSymbolCharacters
%

category: 'class initialization'
classmethod: STONWriter
initializeSTONCharacters
	| escapes |
	STONCharacters := Array new: 127.
	32 to: 126 do: [ :each | 
		STONCharacters at: each + 1 put: #pass ].
	escapes := #( 8 '\b' 9 '\t' 10 '\n' 12 '\f' 13 '\r' 34 '\"' 39 '\''' 92 '\\' ).
	1 to: escapes size - 1 by: 2 do: [ :index | 
		STONCharacters 
			at: (escapes at: index) + 1
			put: (escapes at: index + 1) ]
%

category: 'class initialization'
classmethod: STONWriter
initializeSTONSimpleSymbolCharacters
  "STONSimpleSymbolCharacters asArray collectWithIndex: [ :each :index |
		each isZero ifTrue: [ (index - 1) asCharacter ] ]."

  STONSimpleSymbolCharacters := (ByteArray new: 256)
    atAllPut: 1;
    yourself.
  1 to: 256 do: [ :each | | char |
    char := (each - 1) asCharacter.
    (self isSimpleSymbolChar: char)
      ifTrue: [ STONSimpleSymbolCharacters at: each put: 0 ] ]
%

category: 'private'
classmethod: STONWriter
isSimpleSymbolChar: char
	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./' includes: char
%

category: 'instance creation'
classmethod: STONWriter
on: writeStream
	^ self new
		on: writeStream;
		yourself
%

!		Instance methods for 'STONWriter'

category: 'initialize-release'
method: STONWriter
close
	writeStream ifNotNil: [
		writeStream close.
		writeStream := nil ]
%

category: 'private'
method: STONWriter
encodeKey: key value: value
	self nextPut: key.
	self prettyPrintSpace.
	writeStream nextPut: $:.
	self prettyPrintSpace.
	self nextPut: value
%

category: 'writing'
method: STONWriter
encodeList: elements
	writeStream nextPut: $[.
	elements isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				self newlineIndent.
				elements 
					do: [ :each | self nextPut: each ]
					separatedBy: [ self listElementSeparator ] ].
			self newlineIndent ].
	writeStream nextPut: $]
%

category: 'writing'
method: STONWriter
encodeMap: pairs
	| first |
	first := true.
	writeStream nextPut: ${.
	pairs isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				self newlineIndent.
				pairs keysAndValuesDo: [ :key :value |
					first 
						ifTrue: [ first := false ] 
						ifFalse: [ self mapElementSeparator ].
					self encodeKey: key value: value ] ].
			self newlineIndent ].
	writeStream nextPut: $}
%

category: 'private'
method: STONWriter
encodeString: string
  | encodedString |
  encodedString := string.
  writeStream
    nextPut:
      (jsonMode
        ifTrue: [ $" ]
        ifFalse: [ $' ]).
  encodedString do: [ :each | self encodeCharacter: each ].
  writeStream
    nextPut:
      (jsonMode
        ifTrue: [ $" ]
        ifFalse: [ $' ])
%

category: 'private'
method: STONWriter
indentedDo: block
	level := level + 1.
	block value.
	level := level - 1
%

category: 'initialize-release'
method: STONWriter
initialize
  prettyPrint := false.
  newLine := String with: Character lf.
  level := 0.
  referencePolicy := #'normal'.
  jsonMode := false.
  objects := IdentityDictionary new
%

category: 'initialize-release'
method: STONWriter
jsonMode: boolean
	jsonMode := boolean
%

category: 'private'
method: STONWriter
listElementSeparator
	writeStream nextPut: $,.
	self newlineIndent
%

category: 'private'
method: STONWriter
mapElementSeparator
	writeStream nextPut: $,.
	self newlineIndent
%

category: 'initialize-release'
method: STONWriter
newLine: string
	newLine := string
%

category: 'private'
method: STONWriter
newlineIndent
	prettyPrint ifTrue: [ 
		writeStream nextPutAll: newLine.
		level timesRepeat: [ writeStream tab ] ]
%

category: 'public'
method: STONWriter
nextPut: anObject
	anObject stonOn: self
%

category: 'initialize-release'
method: STONWriter
on: aWriteStream
	writeStream := aWriteStream
%

category: 'initialize-release'
method: STONWriter
prettyPrint: boolean
	prettyPrint := boolean
%

category: 'private'
method: STONWriter
prettyPrintSpace
	prettyPrint ifTrue: [ writeStream space ]
%

category: 'initialize-release'
method: STONWriter
referencePolicy: policy
  (#(#'normal' #'ignore' #'error') includes: policy)
    ifFalse: [ self error: 'Unknown reference policy: ' , policy printString ].
  referencePolicy := policy
%

category: 'initialize-release'
method: STONWriter
reset
	objects removeAll
%

category: 'private'
method: STONWriter
shortListElementSeparator
	writeStream nextPut: $,.
	self prettyPrintSpace
%

category: 'private'
method: STONWriter
with: object do: block
	| index |
	referencePolicy = #ignore 
		ifTrue: [ ^ block value ].
	(index := objects at: object ifAbsent: [ nil ]) notNil
		ifTrue: [
			referencePolicy = #error
				ifTrue: [ ^ STONWriterError signal: 'Shared reference detected' ].
			self writeReference: index ]
		ifFalse: [
			index := objects size + 1.
			objects at: object put: index.
			block value ]
%

category: 'writing'
method: STONWriter
writeBoolean: boolean
	writeStream print: boolean
%

category: 'writing'
method: STONWriter
writeInteger: integer
	writeStream print: integer
%

category: 'writing'
method: STONWriter
writeList: collection
	self with: collection do: [ 
		self encodeList: collection ]
%

category: 'writing'
method: STONWriter
writeMap: hashedCollection
	self with: hashedCollection do: [ 
		self encodeMap: hashedCollection ]
%

category: 'writing'
method: STONWriter
writeNull
	jsonMode
		ifTrue: [ writeStream nextPutAll: 'null' ]
		ifFalse: [ writeStream print: nil ]
%

category: 'writing'
method: STONWriter
writeObject: anObject
  | instanceVariableNames |
  (instanceVariableNames := anObject class allInstVarNames) isEmpty
    ifTrue: [ self writeObject: anObject do: [ self encodeMap: #() ] ]
    ifFalse: [ self writeObject: anObject streamMap: [ :dictionary | instanceVariableNames
            do: [ :each | (anObject instVarAt: (instanceVariableNames indexOf: each asSymbol))
                ifNotNil: [ :value | dictionary at: each asSymbol put: value ]
                ifNil: [ anObject stonShouldWriteNilInstVars
                    ifTrue: [ dictionary at: each asSymbol put: nil ] ] ] ] ]
%

category: 'writing'
method: STONWriter
writeObject: anObject do: block
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ STONWriterError signal: 'Wrong object class for JSON mode' ].
	self with: anObject do: [
		writeStream nextPutAll: anObject class stonName.
		self prettyPrintSpace.
		block value ]
%

category: 'writing'
method: STONWriter
writeObject: object listSingleton: element
	self writeObject: object do: [
		writeStream nextPut: $[.
		self 
			prettyPrintSpace;
			nextPut: element;
			prettyPrintSpace.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeObject: object streamList: block
	self writeObject: object do: [ | listWriter |
		listWriter := STONListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self newlineIndent.
			block value: listWriter ].
		self newlineIndent.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeObject: object streamMap: block
	self writeObject: object do: [ | mapWriter |
		mapWriter := STONMapWriter on: self.
		writeStream nextPut: ${.
		self indentedDo: [
			self newlineIndent.
			block value: mapWriter ].
		self newlineIndent.
		writeStream nextPut: $} ]
%

category: 'writing'
method: STONWriter
writeObject: object streamShortList: block
	self writeObject: object do: [ | listWriter |
		listWriter := STONShortListWriter on: self.
		writeStream nextPut: $[.
		self indentedDo: [
			self prettyPrintSpace.
			block value: listWriter ].
		self prettyPrintSpace.
		writeStream nextPut: $] ]
%

category: 'writing'
method: STONWriter
writeReference: index
	writeStream
		nextPut: $@;
		print: index
%

category: 'writing'
method: STONWriter
writeString: string
	self encodeString: string
%

category: 'writing'
method: STONWriter
writeSymbol: symbol
	jsonMode
		ifTrue: [
			self writeString: symbol ]
		ifFalse: [
			writeStream nextPut: $#.
			(self isSimpleSymbol: symbol)
				ifTrue: [
					writeStream nextPutAll: symbol ]
				ifFalse: [
					self encodeString: symbol ] ]
%

! Class implementation for 'ZnBufferedReadStream'

!		Class methods for 'ZnBufferedReadStream'

category: 'instance creation'
classmethod: ZnBufferedReadStream
new

	^ self basicNew
		initialize;
		yourself
%

category: 'instance creation'
classmethod: ZnBufferedReadStream
on: readStream
	^ self new
		on: readStream;
		yourself
%

category: 'convenience'
classmethod: ZnBufferedReadStream
on: readStream do: block
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)
%

!		Instance methods for 'ZnBufferedReadStream'

category: 'testing'
method: ZnBufferedReadStream
atEnd
	^ position > limit and: [ stream atEnd ]
	
%

category: 'initialize-release'
method: ZnBufferedReadStream
close
	stream close
%

category: 'testing'
method: ZnBufferedReadStream
closed
	^ stream closed
%

category: 'accessing'
method: ZnBufferedReadStream
collectionSpecies
	^ stream isBinary
		ifTrue: [ ByteArray ]
		ifFalse: [ String ]
%

category: 'accessing'
method: ZnBufferedReadStream
contents
	
	^ self upToEnd
%

category: 'accessing'
method: ZnBufferedReadStream
defaultBufferSize
	^ 2 raisedToInteger: 16
%

category: 'private'
method: ZnBufferedReadStream
discardBuffer
	limit := 0.
	position := 1
%

category: 'initialization'
method: ZnBufferedReadStream
initialize

	position := 1.
	limit := 0
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
int16
	^ self nextIntegerOfSize: 2 signed: true bigEndian: true 
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
int32
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
int8
	^ self nextIntegerOfSize: 1 signed: true bigEndian: true 
%

category: 'accessing'
method: ZnBufferedReadStream
isBinary
	^ stream isBinary
%

category: 'testing'
method: ZnBufferedReadStream
isStream
	^ true
%

category: 'accessing'
method: ZnBufferedReadStream
next
	"Return the next element and move over it"
	
	position > limit
		ifTrue: [ self nextBuffer ].
	^ position <= limit
		ifTrue: [ 
			| char |
			char := buffer at: position.
			position := position + 1.
			char ]
		ifFalse: [ nil ]
%

category: 'accessing'
method: ZnBufferedReadStream
next: requestedCount 
	"Read requestedCount elements and return them as a collection.
	If less are available, a smaller collection will be returned."

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
%

category: 'accessing'
method: ZnBufferedReadStream
next: requestedCount into: collection
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self 
		next: requestedCount 
		into: collection 
		startingAt: 1   
%

category: 'accessing'
method: ZnBufferedReadStream
next: requestedCount into: collection startingAt: offset
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| read |
	read := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ read = requestedCount 
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + read - 1 ]     
%

category: 'private'
method: ZnBufferedReadStream
nextBuffer
	stream atEnd ifTrue: [ ^ self ].
	limit := stream readInto: buffer startingAt: 1 count: buffer size.
	position := 1
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextInt32
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextIntegerOfSize: numberOfBytes signed: signed bigEndian: bigEndian
	"Assuming the receiver is a stream of bytes, read the next integer of size numberOfBytes.
	If bigEndian is true, use network byte order, most significant byte first, 
	else use little endian order, least significant byte first.
	If signed is true, interpret as a two-complement signed value, 
	else interpret as a plain unsigned value."
	
	| value |
	value := 0.
	bigEndian
		ifTrue: [ 
			(numberOfBytes - 1) * 8 to: 0 by: -8 do: [ :shift |
				value := value + (self next bitShift: shift) ] ]
		ifFalse: [ 
			0 to: (numberOfBytes - 1) * 8 by: 8 do: [ :shift |
				value := value + (self next bitShift: shift) ] ].
	^ (signed and: [ (value bitAt: numberOfBytes * 8) = 1 ])
		ifTrue: [ value - (1 << (numberOfBytes * 8)) ]
		ifFalse: [ value ]
%

category: 'accessing'
method: ZnBufferedReadStream
nextInto: collection
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection
%

category: 'accessing'
method: ZnBufferedReadStream
nextLine
"Answer next line (may be empty) without line end delimiters, or nil if at end.
Leave the stream positioned after the line delimiter(s).
Handle a zoo of line delimiters CR, LF, or CR-LF pair"

| cr lf chrcls result ch |
self atEnd ifTrue: [^nil].
cr := (chrcls:= Character) cr.
lf := chrcls  lf.
result := self collectionSpecies new.
[ ch := self next .
  (ch == cr or:[ ch == lf ]) ifTrue:[ 
    ch == cr ifTrue:[ self peekFor: lf ].
    ^ result 
  ].
  result add: ch .
  self atEnd 
] whileFalse .
^ result
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextLittleEndianNumber: numberOfBytes
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: false
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextNumber: numberOfBytes
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: true
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
nextWord
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true
%

category: 'initialize-release'
method: ZnBufferedReadStream
on: readStream
	stream := readStream.
	self sizeBuffer: self defaultBufferSize
%

category: 'accessing'
method: ZnBufferedReadStream
peek
	"Return the next element but do not move over it"
	
	position > limit
		ifTrue: [ self nextBuffer ].
	^ position <= limit
		ifTrue: [ buffer at: position ]
		ifFalse: [ nil ]
%

category: 'accessing'
method: ZnBufferedReadStream
peek: count
  self peek .
  ^ buffer copyFrom: position to: (position + count min: limit)
%

category: 'accessing'
method: ZnBufferedReadStream
peekFor: object
	"Answer false and do not move over the next element if it is not equal to object, or if the receiver is at the end. 
	Answer true and move over the next element when it is equal to object."

	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
%

category: 'accessing'
method: ZnBufferedReadStream
position
	
	"If the buffer advanced, we need to check the original stream position, minus what we have read.
	The -1 is because the buffer is base 1"
	^ stream position - limit + position - 1
%

category: 'accessing'
method: ZnBufferedReadStream
position: anInteger 
	
	| bufferEnd bufferStart |
	bufferEnd := stream position.
	bufferStart := bufferEnd - limit.
	(anInteger between: bufferStart and: bufferEnd)
		ifTrue: [ position := anInteger - bufferStart + 1 ]
		ifFalse: [ 
			"We reset the buffer and update the position in the underlying stream"
			limit := 0.
			position := 1.
			stream position: anInteger ]
%

category: 'private'
method: ZnBufferedReadStream
readFromBufferInto: collection startingAt: offset count: requestedCount
	"Read up to requestedCount elements into collection starting at offset,
	from my buffer, answering the number of elements read.
	There could be fewer elements buffered."

	| read |
	read := 0.
	position <= limit
		ifTrue: [ read := limit - position + 1 min: requestedCount.
			collection
				replaceFrom: offset
				to: offset + read - 1
				with: buffer
				startingAt: position.
			position := position + read ].
	^ read
%

category: 'accessing'
method: ZnBufferedReadStream
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	answering the number of elements read, there could be fewer elements available."

	| countRead countYetToRead |
	"First, read from elements already in my buffer."
	countRead := self readFromBufferInto: collection startingAt: offset count: requestedCount.
	countYetToRead := requestedCount - countRead.
	countYetToRead > 0
		ifTrue: [ "See if there are more elements to be read from the underlying stream"
			| newOffset |
			newOffset := offset + countRead.
			(self shouldBufferReadOfCount: countYetToRead)
				ifTrue: [ self nextBuffer.
					position > limit ifTrue: [ ^ countRead ].
					limit > 0
						ifTrue:
							[ countRead := countRead + (self readInto: collection startingAt: newOffset count: countYetToRead) ] ]
				ifFalse:
					[ countRead := countRead + (stream readInto: collection startingAt: newOffset count: countYetToRead) ] ].
	^ countRead
%

category: 'accessing'
method: ZnBufferedReadStream
setToEnd
	
	stream setToEnd
%

category: 'private'
method: ZnBufferedReadStream
shouldBufferReadOfCount: elementCount
	"For larger read requests, buffering fails to give an advantage."

	^ elementCount < (buffer size / 2)
%

category: 'accessing'
method: ZnBufferedReadStream
size

	^ stream size
%

category: 'initialize-release'
method: ZnBufferedReadStream
sizeBuffer: size
	buffer := self collectionSpecies new: size
%

category: 'accessing'
method: ZnBufferedReadStream
skip: count
	"Skip over count elements.
	This could be further optimzed."
	
	count timesRepeat: [ self next ]
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
uint16
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true 
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
uint32
	^ self nextIntegerOfSize: 4 signed: false bigEndian: true 
%

category: 'accessing-bytes'
method: ZnBufferedReadStream
uint8
	^ self nextIntegerOfSize: 1 signed: false bigEndian: true
%

category: 'accessing'
method: ZnBufferedReadStream
upTo: value 
	"Read upto but not including value and return them as a collection.
	If value is not found, return the entire contents of the stream.
	This could be further optimzed."
	
	^ self collectionSpecies 
		streamContents: [ :writeStream | | element |
			[ self atEnd or: [ (element := self next) = value ] ] whileFalse: [ 
				writeStream nextPut: element ] ]
%

category: 'accessing'
method: ZnBufferedReadStream
upToAll: aCollection 
"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

| startPos endMatch result x |
startPos := self position.
x := self upTo: aCollection first.
self atEnd ifTrue: [ ^ x ].
2 to: aCollection size do: [:i |
	self peek = (aCollection at: i)
		ifTrue: [ self next ]
		ifFalse: [ self position: startPos.
		^ self upToEnd ] ].
endMatch := self position.
self position: startPos.
result := self next: endMatch - startPos - aCollection size.
self position: endMatch.
^ result
%

category: 'accessing'
method: ZnBufferedReadStream
upToEnd
	"Read elements until the stream is atEnd and return them as a collection."

	^ self collectionSpecies
		streamContents: [ :out | 
			[ self atEnd ] whileFalse: [ 
				position > limit
					ifTrue: [ self nextBuffer ].	
				out next: limit - position + 1 putAll: buffer startingAt: position.
				position := limit + 1 ] ]
%

category: 'accessing'
method: ZnBufferedReadStream
wrappedStream
	^ stream
%

category: 'accessing'
method: ZnBufferedReadStream
wrappedStreamName
	^ stream wrappedStreamName
%

! Class implementation for 'ZnBufferedReadWriteStream'

!		Class methods for 'ZnBufferedReadWriteStream'

category: 'instance creation'
classmethod: ZnBufferedReadWriteStream
on: writeStream
	^ self basicNew
		on: writeStream;
		yourself
%

category: 'convenience'
classmethod: ZnBufferedReadWriteStream
on: readStream do: block
	"Execute block with as argument a ZnBufferedReadStream on readStream.
	Return the value of block."

	^ block value: (self on: readStream)
%

!		Instance methods for 'ZnBufferedReadWriteStream'

category: 'testing'
method: ZnBufferedReadWriteStream
atEnd
	
	^ self readingActionDo: [ readStream atEnd ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
close
	
	writeStream flush.
	writeStream close.
%

category: 'testing'
method: ZnBufferedReadWriteStream
closed
	^ readStream closed
%

category: 'accessing'
method: ZnBufferedReadWriteStream
flush
	
	self writingActionDo: [ writeStream flush ]
%

category: 'testing'
method: ZnBufferedReadWriteStream
isBinary
	
	^ readStream isBinary
%

category: 'testing'
method: ZnBufferedReadWriteStream
isReadOnly
	
	^ false
%

category: 'testing'
method: ZnBufferedReadWriteStream
isStream

	^ true
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next
	
	^ self readingActionDo: [ 
		readStream next ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next: anInteger 
	
	^ self readingActionDo: [ 
		readStream next: anInteger ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next: count putAll: collection

	self writingActionDo: [ 
		writeStream next: count putAll: collection ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
next: count putAll: collection startingAt: offset
	
	self writingActionDo: [
		writeStream next: count putAll: collection startingAt: offset ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
nextPut: aCharacter 
	
	self writingActionDo: [ writeStream nextPut: aCharacter ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
nextPutAll: aString 
	
	^ self writingActionDo: [ writeStream nextPutAll: aString ]
%

category: 'instance creation'
method: ZnBufferedReadWriteStream
on: aStream

	lastRead := true.
	readStream := ZnBufferedReadStream on: aStream.
	writeStream := ZnBufferedWriteStream on: aStream.
%

category: 'accessing'
method: ZnBufferedReadWriteStream
peek
	
	^ self readingActionDo: [ 
		readStream peek ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
position
	
	^ lastRead
		ifTrue: [ readStream position ]
		ifFalse: [ writeStream position ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
position: anInteger 
	
	self writingActionDo: [ 
		writeStream position: anInteger ]
	
%

category: 'private'
method: ZnBufferedReadWriteStream
readingActionDo: aBlock

	"Reading from the read stream.
	We should 
	 - flush the write stream
	 - discard the read buffer (which may contain incorrect data).
	 - and then perform the read."
	
	lastRead ifFalse: [ 
		writeStream flush.
		readStream discardBuffer ].
	^ aBlock ensure: [ lastRead := true ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
readInto: collection startingAt: offset count: requestedCount
	
	^ self readingActionDo: [ 
		readStream readInto: collection startingAt: offset count: requestedCount ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
setToEnd
	
	^ self writingActionDo: [ 
		writeStream setToEnd ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
size
	^ readStream size
%

category: 'initialize-release'
method: ZnBufferedReadWriteStream
sizeBuffer: anInteger 
	
	readStream sizeBuffer: anInteger.
	writeStream sizeBuffer: anInteger.
%

category: 'accessing'
method: ZnBufferedReadWriteStream
skip: anInteger 
	
	self readingActionDo: [ 
		readStream skip: anInteger ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
truncate
	
	self writingActionDo: [ writeStream truncate ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
truncate: anInteger 
	
	self writingActionDo: [ writeStream truncate: anInteger ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
upTo: aCharacter 
	
	^ self readingActionDo: [ readStream upTo: aCharacter ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
upToEnd
	
	^ self readingActionDo: [ readStream upToEnd ]
%

category: 'accessing'
method: ZnBufferedReadWriteStream
wrappedStream

	^ readStream wrappedStream
%

category: 'accessing'
method: ZnBufferedReadWriteStream
wrappedStreamName

	^ readStream wrappedStreamName
%

category: 'accessing'
method: ZnBufferedReadWriteStream
writingActionDo: aBlock
	
	"Writing to the write stream.
	We should 
	 - write the write stream
	 - discard the read buffer (which may contain incorrect data)"
	lastRead ifTrue: [ 
		writeStream discardBuffer ].
	readStream discardBuffer.
	^ aBlock ensure: [ lastRead := false ]
%

! Class implementation for 'ZnBufferedWriteStream'

!		Class methods for 'ZnBufferedWriteStream'

category: 'instance creation'
classmethod: ZnBufferedWriteStream
on: writeStream
	^ self basicNew
		on: writeStream;
		yourself
%

category: 'convenience'
classmethod: ZnBufferedWriteStream
on: writeStream do: block
	"Execute block with as argument a ZnBufferedWriteStream on writeStream,
	making sure #flush is called at the end. Return the value of block."
	
	| bufferedWriteStream result |
	bufferedWriteStream := self on: writeStream.
	result := block value: bufferedWriteStream.
	bufferedWriteStream flush.
	^ result
%

!		Instance methods for 'ZnBufferedWriteStream'

category: 'private'
method: ZnBufferedWriteStream
buffer
	buffer isNil 
		ifTrue: [ self sizeBuffer: self defaultBufferSize ].
	^ buffer
%

category: 'accessing'
method: ZnBufferedWriteStream
bufferFreeSize
	^ self bufferSize - position
%

category: 'accessing'
method: ZnBufferedWriteStream
bufferSize
	^ buffer isNil 
		ifTrue: [ self defaultBufferSize ]
		ifFalse: [ buffer size ]
%

category: 'initialize-release'
method: ZnBufferedWriteStream
close
	self flush.
	stream close
%

category: 'accessing'
method: ZnBufferedWriteStream
cr
	self nextPut: Character cr
%

category: 'accessing'
method: ZnBufferedWriteStream
crlf
	self cr; lf
%

category: 'accessing'
method: ZnBufferedWriteStream
defaultBufferSize
	^ 2 raisedToInteger: 16
%

category: 'private'
method: ZnBufferedWriteStream
discardBuffer

	position := 0
%

category: 'accessing'
method: ZnBufferedWriteStream
flush
	self flushBuffer.
	stream flush
%

category: 'private'
method: ZnBufferedWriteStream
flushBuffer
	position = 0 ifTrue: [ ^ self ].
	position = self bufferSize
		ifTrue: [ stream nextPutAll: buffer ]
		ifFalse: [ stream nextPutAll: (buffer copyFrom: 1 to: position) ].
	position := 0
%

category: 'private'
method: ZnBufferedWriteStream
flushBufferIfFull
	position = self bufferSize 
		ifTrue: [ self flushBuffer ]
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
int16: integer
	^ self nextIntegerOfSize: 2 signed: true bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
int32: integer
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
int8: integer
	^ self nextIntegerOfSize: 1 signed: true bigEndian: true put: integer
%

category: 'testing'
method: ZnBufferedWriteStream
isBinary

	^ stream isBinary
%

category: 'testing'
method: ZnBufferedWriteStream
isStream

	^ true
%

category: 'accessing'
method: ZnBufferedWriteStream
lf
	self nextPut: Character lf
%

category: 'accessing'
method: ZnBufferedWriteStream
next: count putAll: collection
	"Write count elements from collection"
	
	self 
		next: count 
		putAll: collection 
		startingAt: 1
%

category: 'accessing'
method: ZnBufferedWriteStream
next: count putAll: collection startingAt: offset
	"Write count elements from collection starting at offset."
	
	self flushBufferIfFull.
	count <= self bufferFreeSize
		ifTrue: [
			self buffer replaceFrom: position + 1 to: position + count with: collection startingAt: offset.
			position := position + count ]
		ifFalse: [
			self flushBuffer.
			count > (self bufferSize / 2)
				ifTrue: [ stream next: count putAll: collection startingAt: offset ]
				ifFalse: [ self next: count putAll: collection startingAt: offset ] ]
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextInt32Put: integer
	^ self nextIntegerOfSize: 4 signed: true bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextIntegerOfSize: numberOfBytes signed: signed bigEndian: bigEndian put: value
	"Assuming the receiver is a stream of bytes, write value as the next integer of size numberOfBytes.
	If bigEndian is true, use network byte order, most significant byte first, 
	else use little endian order, least significant byte first.
	If signed is true, encode as a two-complement signed value, 
	else encode as a plain unsigned value."
	
	| unsignedValue |
	unsignedValue := (signed and: [ value negative ])
		ifTrue: [ (1 << (numberOfBytes * 8)) + value ] 
		ifFalse: [ value ].
	(unsignedValue between: 0 and: (2 raisedTo: (numberOfBytes * 8)) - 1)
		ifFalse: [ self error: 'Domain Error ', unsignedValue printString, ' outside of expected range' ].
	bigEndian
		ifTrue: [ 
			numberOfBytes to: 1 by: -1 do: [ :index |
				self nextPut: (unsignedValue digitAt: index) ] ]
		ifFalse: [ 
			1 to: numberOfBytes do: [ :index |
				self nextPut: (unsignedValue digitAt: index) ] ].
	^ value
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextLittleEndianNumber: numberOfBytes put: integer
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: false put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextNumber: numberOfBytes put: integer
	^ self nextIntegerOfSize: numberOfBytes signed: false bigEndian: true put: integer
%

category: 'accessing'
method: ZnBufferedWriteStream
nextPut: object
	self flushBufferIfFull.
	position := position + 1.
	self buffer at: position put: object
%

category: 'accessing'
method: ZnBufferedWriteStream
nextPutAll: collection
	"Write a collection"
	
	self 
		next: collection size 
		putAll: collection 
		startingAt: 1
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
nextWordPut: integer
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true put: integer
%

category: 'initialize-release'
method: ZnBufferedWriteStream
on: writeStream
	stream := writeStream.
	position := 0
%

category: 'accessing'
method: ZnBufferedWriteStream
position

	^ stream position + position
%

category: 'accessing'
method: ZnBufferedWriteStream
position: anInteger 
	self flush.
	stream position: anInteger
%

category: 'accessing'
method: ZnBufferedWriteStream
print: object
	object printOn: self
%

category: 'printing'
method: ZnBufferedWriteStream
printOn: aStream
	aStream 
		nextPutAll: 'a '; 
		nextPutAll: self class name
%

category: 'accessing'
method: ZnBufferedWriteStream
setToEnd
	
	self flush.
	stream setToEnd
%

category: 'accessing'
method: ZnBufferedWriteStream
sizeBuffer: size
	buffer := (stream isBinary ifTrue: [ ByteArray ] ifFalse: [ String ]) new: size
%

category: 'accessing'
method: ZnBufferedWriteStream
space
	self nextPut: Character space
%

category: 'accessing'
method: ZnBufferedWriteStream
tab
	self nextPut: Character tab
%

category: 'accessing'
method: ZnBufferedWriteStream
truncate
	
	stream truncate
%

category: 'accessing'
method: ZnBufferedWriteStream
truncate: anInteger 
	
	self flush.
	stream truncate: anInteger
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
uint16: integer
	^ self nextIntegerOfSize: 2 signed: false bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
uint32: integer
	^ self nextIntegerOfSize: 4 signed: false bigEndian: true put: integer
%

category: 'accessing-bytes'
method: ZnBufferedWriteStream
uint8: integer
	^ self nextIntegerOfSize: 1 signed: false bigEndian: true put: integer
%

! Class implementation for 'ZnCharacterEncoder'

!		Class methods for 'ZnCharacterEncoder'

category: 'accessing'
classmethod: ZnCharacterEncoder
canonicalEncodingIdentifier: string
	^ (string select: [ :each | each isAlphaNumeric ]) asLowercase
%

category: 'accessing'
classmethod: ZnCharacterEncoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	self subclassResponsibility: #handlesEncoding:
%

category: 'accessing'
classmethod: ZnCharacterEncoder
knownEncodingIdentifiers
	"Return a collection of all known encoding identifiers in the system"
	
	self = ZnCharacterEncoder ifFalse: [ ^ #() ].
	^ Array streamContents: [ :all |
		self allSubclasses do: [ :subClass |
			all nextPutAll: subClass knownEncodingIdentifiers ] ]
%

category: 'instance creation'
classmethod: ZnCharacterEncoder
newForEncoding: string
	"Return a new character encoder object for an encoding described by string.
	Search for a subclass that handles it and delegate (subclassResponsibility)."
	
	| concreteSubclass |
	concreteSubclass := self allSubclasses 
		detect: [ :each | each handlesEncoding: string ] 
		ifNone: [ ^ self error: 'The ', string printString, ' is not currently supported.' ].
	^ concreteSubclass newForEncoding: string
%

!		Instance methods for 'ZnCharacterEncoder'

category: 'converting'
method: ZnCharacterEncoder
asZnCharacterEncoder
	^ self
%

category: 'converting'
method: ZnCharacterEncoder
backOnStream: stream
	"Move back one character on stream, assuming stream understands #back"
	
	self subclassResponsibility: #backOnStream:
%

category: 'convenience'
method: ZnCharacterEncoder
decodeAsCodePoints: bytes
	"Decode bytes and return the resulting code points"
	
	self subclassResponsibility: #decodeAsCodePoints:
%

category: 'convenience'
method: ZnCharacterEncoder
encodeCodePoints: codePoints
	"Encode codePoints and return the resulting byte array"
	
	^ self subclassResponsibility: #encodeCodePoints:
%

category: 'converting'
method: ZnCharacterEncoder
encodedByteCountFor: character
	"Return how many bytes are needed to encode character"
	
	"We should use #codePoint but #asInteger is faster"
	
	^ self encodedByteCountForCodePoint: character asInteger
%

category: 'convenience'
method: ZnCharacterEncoder
encodedByteCountForCodePoints: codePoints
	"Return the exact number of bytes it would take to encode codePoints as a byte array"

	^self subclassResponsibility: #encodedByteCountForCodePoints:
%

category: 'convenience'
method: ZnCharacterEncoder
encodedByteCountForString: string
	"Return the exact number of bytes it would take to encode string as a byte array"

	^self subclassResponsibility: #encodedByteCountForCodePoints:
%

category: 'convenience'
method: ZnCharacterEncoder
next: count putAll: string startingAt: offset toStream: stream
	"Write count characters from string starting at offset to stream."
	
	offset to: offset + count - 1 do: [ :index | 
		self nextPut: (string at: index) toStream: stream ]
%

category: 'converting'
method: ZnCharacterEncoder
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"
	
	self subclassResponsibility: #nextCodePointFromStream:
%

category: 'converting'
method: ZnCharacterEncoder
nextFromStream: stream
	"Read and return the next character from stream"
	
	"We should use #codePoint: but #value: is faster"
	
	^ Character value: (self nextCodePointFromStream: stream)
%

category: 'converting'
method: ZnCharacterEncoder
nextPut: character toStream: stream
	"Write the encoding for character to stream"
	
	"We should use #codePoint but #asInteger is faster"
	
	self nextPutCodePoint: character asInteger toStream: stream
%

! Class implementation for 'ZnUTF8Encoder'

!		Class methods for 'ZnUTF8Encoder'

category: 'accessing'
classmethod: ZnUTF8Encoder
default
	"Return a cached instance of the most commonly used encoder,
	which is faster than going via #newForEncoding: that does a subclass search"
	
	^ Default ifNil: [ Default := self new ]
%

category: 'accessing'
classmethod: ZnUTF8Encoder
handlesEncoding: string
	"Return true when my instances handle the encoding described by string"
	
	^ (self canonicalEncodingIdentifier: string) = 'utf8'
%

category: 'accessing'
classmethod: ZnUTF8Encoder
knownEncodingIdentifiers
	^ #( utf8 )
%

category: 'instance creation'
classmethod: ZnUTF8Encoder
newForEncoding: string
	"No further parametrization needed"
	
	^ self new
%

!		Instance methods for 'ZnUTF8Encoder'

category: 'converting'
method: ZnUTF8Encoder
backOnStream: stream
	"Move back one character on stream"

	self error: 'not currently supported'
%

category: 'convenience'
method: ZnUTF8Encoder
decodeAsCodePoints: bytes
	"Decode bytes and return the resulting code points"

	| ar |
	ar := {}.
	bytes decodeFromUTF8 do: [:char | ar add: char codePoint ].
	^ ar
%

category: 'convenience'
method: ZnUTF8Encoder
decodeBytes: bytes
	"Decode bytes and return the resulting string"

	^ bytes decodeFromUTF8
%

category: 'convenience'
method: ZnUTF8Encoder
encodeCodePoints: codePoints
	"Encode codePoints and return the resulting byte array"
	
	^ codePoints asByteArray asUnicodeString encodeAsUTF8
%

category: 'converting'
method: ZnUTF8Encoder
encodedByteCountFor: character
	"Return how many bytes are needed to encode character"
		
	^ character asString encodeAsUTF8 size
%

category: 'convenience'
method: ZnUTF8Encoder
encodedByteCountForCodePoints: codePoints
	"Return the exact number of bytes it would take to encode codePoints as a byte array"

	^ (self encodeCodePoints: codePoints) size
%

category: 'convenience'
method: ZnUTF8Encoder
encodedByteCountForString: string
	"Return the exact number of bytes it would take to encode string as a byte array"

	^ (self encodeString: string) size
%

category: 'convenience'
method: ZnUTF8Encoder
encodeString: string
	"Encode string and return the resulting Utf8 instance"
	
	^ string encodeAsUTF8 asByteArray
%

category: 'accessing'
method: ZnUTF8Encoder
identifier
	^ #utf8
%

category: 'convenience'
method: ZnUTF8Encoder
next: count putAll: string startingAt: offset toStream: stream
	"Write count characters from string starting at offset to stream."
	
	stream nextPutAll: (string copyFrom: offset to: offset + count - 1) encodeAsUTF8 asByteArray
%

category: 'converting'
method: ZnUTF8Encoder
nextCodePointFromStream: stream
	"Read and return the next integer code point from stream"

	self error: 'not currently supported'
%

category: 'converting'
method: ZnUTF8Encoder
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for Integer code point to stream"

	^ stream nextPutAll: (Character codePoint: codePoint) asString encodeAsUTF8
%

category: 'convenience'
method: ZnUTF8Encoder
readInto: aCollection startingAt: offset count: requestedCount fromStream: stream
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd"

	| decodedCollection pos |
	pos := stream position.
	decodedCollection := self decodeBytes: stream contents.
	aCollection 
		replaceFrom: offset 
		to: offset + (requestedCount min: decodedCollection size) - 1
		with: decodedCollection
		startingAt: pos + 1.
	stream setToEnd.
	^ 	requestedCount min: decodedCollection size
%

! Class implementation for 'ZnCharacterReadWriteStream'

!		Class methods for 'ZnCharacterReadWriteStream'

category: 'instance creation'
classmethod: ZnCharacterReadWriteStream
on: wrappedStream encoding: encoding

	^ self new
		on: wrappedStream encoding: encoding;
		yourself
%

!		Instance methods for 'ZnCharacterReadWriteStream'

category: 'accessing'
method: ZnCharacterReadWriteStream
atEnd
	
	^ readStream atEnd
%

category: 'accessing'
method: ZnCharacterReadWriteStream
close
	
	writeStream close
%

category: 'testing'
method: ZnCharacterReadWriteStream
closed
	^ writeStream closed
%

category: 'accessing'
method: ZnCharacterReadWriteStream
collectionSpecies
	^ String
%

category: 'accessing'
method: ZnCharacterReadWriteStream
cr
	
	writeStream cr
%

category: 'accessing'
method: ZnCharacterReadWriteStream
flush

	writeStream flush
%

category: 'testing'
method: ZnCharacterReadWriteStream
isReadOnly
	
	^ false
%

category: 'accessing'
method: ZnCharacterReadWriteStream
next
	
	^ readStream next
%

category: 'accessing'
method: ZnCharacterReadWriteStream
next: anInteger 
	
	^ readStream next: anInteger
%

category: 'accessing'
method: ZnCharacterReadWriteStream
nextPut: aCharacter

	^ writeStream nextPut: aCharacter
%

category: 'accessing'
method: ZnCharacterReadWriteStream
nextPutAll: aString

	^ writeStream nextPutAll: aString
%

category: 'instance creation'
method: ZnCharacterReadWriteStream
on: aStream encoding: encoding
	| encoder |
	encoder := encoding asZnCharacterEncoder.
	readStream := ZnCharacterReadStream on: aStream encoding: encoder.
	writeStream := ZnCharacterWriteStream on: aStream encoding: encoder
%

category: 'accessing'
method: ZnCharacterReadWriteStream
peek
	
	^ readStream peek
%

category: 'accessing'
method: ZnCharacterReadWriteStream
position

	^ readStream position
%

category: 'accessing'
method: ZnCharacterReadWriteStream
position: anInteger 
	
	readStream position: anInteger
%

category: 'accessing'
method: ZnCharacterReadWriteStream
readOnlyCopy

	^ readStream
%

category: 'accessing'
method: ZnCharacterReadWriteStream
setToEnd

	writeStream setToEnd
%

category: 'accessing'
method: ZnCharacterReadWriteStream
size

	^ readStream size
%

category: 'accessing'
method: ZnCharacterReadWriteStream
skip: anInteger 
	
	readStream skip: anInteger
%

category: 'accessing'
method: ZnCharacterReadWriteStream
space
	
	writeStream space
%

category: 'accessing'
method: ZnCharacterReadWriteStream
upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."
	
	^ self collectionSpecies streamContents: [ :out |
		| partialMatch pattern matched |
		partialMatch := (self collectionSpecies new: aCollection size) writeStream.
		pattern := aCollection readStream.
		matched := false.
		([ matched or: [ self atEnd or: [ pattern atEnd ] ] ]) whileFalse: [
			| ch |
			(ch := self next) = pattern next
				ifTrue: [
					pattern atEnd
						ifTrue: [ matched := true ]
						ifFalse: [ partialMatch nextPut: ch ] ]
				ifFalse: [
					pattern reset.
					out nextPutAll: partialMatch contents.
					partialMatch reset.
					out nextPut: ch ] ].
		matched ifFalse: [ out nextPutAll: partialMatch contents ] ]
%

category: 'accessing'
method: ZnCharacterReadWriteStream
upToEnd
	
	^ readStream upToEnd
%

! Class implementation for 'ZnEncodedStream'

!		Class methods for 'ZnEncodedStream'

category: 'accessing'
classmethod: ZnEncodedStream
defaultEncoder
	^ ZnUTF8Encoder default
%

category: 'instance creation'
classmethod: ZnEncodedStream
on: wrappedStream
	^ self new
		on: wrappedStream;
		yourself
%

category: 'instance creation'
classmethod: ZnEncodedStream
on: wrappedStream encoding: encoding
	^ self new
		on: wrappedStream;
		encoding: encoding;
		yourself
%

!		Instance methods for 'ZnEncodedStream'

category: 'initialize-release'
method: ZnEncodedStream
close
	stream close
%

category: 'testing'
method: ZnEncodedStream
closed
	^ stream closed
%

category: 'accessing'
method: ZnEncodedStream
encoder
	^ encoder ifNil: [ encoder := self class defaultEncoder ]
%

category: 'initialize-release'
method: ZnEncodedStream
encoder: characterEncoder
	encoder := characterEncoder
%

category: 'initialize-release'
method: ZnEncodedStream
encoding: encoding
	encoder := encoding asZnCharacterEncoder
%

category: 'testing'
method: ZnEncodedStream
isStream
  ^ true
%

category: 'initialize-release'
method: ZnEncodedStream
on: wrappedStream
	stream := wrappedStream
%

category: 'accessing'
method: ZnEncodedStream
position
	^ stream position
%

category: 'accessing'
method: ZnEncodedStream
position: aPosition
	stream position: aPosition
%

category: 'accessing'
method: ZnEncodedStream
setToEnd
	stream setToEnd
%

category: 'accessing'
method: ZnEncodedStream
size
	^ stream size
%

category: 'accessing'
method: ZnEncodedStream
truncate
	stream truncate
%

category: 'accessing'
method: ZnEncodedStream
truncate: anInteger 
	stream truncate: anInteger
%

category: 'accessing'
method: ZnEncodedStream
wrappedStream
	^ stream
%

category: 'accessing'
method: ZnEncodedStream
wrappedStreamName
	^ stream wrappedStreamName
%

! Class implementation for 'ZnEncodedReadStream'

!		Instance methods for 'ZnEncodedReadStream'

category: 'testing'
method: ZnEncodedReadStream
atEnd
	^ peeked isNil and: [ stream atEnd ]
%

category: 'accessing'
method: ZnEncodedReadStream
contents
	
	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			collectionStream nextPutAll: (self encoder decodeBytes: stream  contents) ]
%

category: 'testing'
method: ZnEncodedReadStream
isBinary
	^ false
%

category: 'testing'
method: ZnEncodedReadStream
isReadOnly

	^ true
%

category: 'accessing'
method: ZnEncodedReadStream
next
	^ peeked
		ifNil: [ 
			stream atEnd ifFalse: [ self nextElement ] ]
		ifNotNil: [ | character |
			character := peeked.
			peeked := nil.
			character ]
%

category: 'accessing'
method: ZnEncodedReadStream
next: requestedCount 
	"Read requestedCount elements into new collection and return it,
	 it could be that less elements were available"

	^ self 
		next: requestedCount 
		into: (self collectionSpecies new: requestedCount)
%

category: 'accessing'
method: ZnEncodedReadStream
next: requestedCount into: collection
	"Read requestedCount elements into collection,
	returning a copy if less elements are available"
	
	^ self
		next: requestedCount
		into: collection
		startingAt: 1
%

category: 'accessing'
method: ZnEncodedReadStream
next: requestedCount into: collection startingAt: offset
	"Read requestedCount elements into collection starting at offset,
	returning a copy if less elements are available"
	
	| readCount |
	readCount := self 
		readInto: collection 
		startingAt: offset 
		count: requestedCount.
	^ requestedCount = readCount
		ifTrue: [ collection ]
		ifFalse: [ collection copyFrom: 1 to: offset + readCount - 1 ]
%

category: 'private'
method: ZnEncodedReadStream
nextElement
	self subclassResponsibility 
%

category: 'accessing'
method: ZnEncodedReadStream
nextInto: collection
	"Read the next elements of the receiver into collection,
	returning a copy if less elements are available"
	
	^ self
		next: collection size
		into: collection
%

category: 'accessing'
method: ZnEncodedReadStream
peek
	^ peeked
		ifNil: [
			stream atEnd ifFalse: [ peeked := self nextElement ] ]
%

category: 'accessing'
method: ZnEncodedReadStream
peekFor: object
	^ self peek = object
		ifTrue: [ 
			self next.
			true ]
		ifFalse: [ false ]
%

category: 'accessing'
method: ZnEncodedReadStream
position

	^ super position - (peeked ifNil: [ 0 ] ifNotNil: [ 1 ])
%

category: 'accessing'
method: ZnEncodedReadStream
position: anInteger
	super position: anInteger.
	peeked := nil
%

category: 'accessing'
method: ZnEncodedReadStream
readInto: collection startingAt: offset count: requestedCount
	"Read requestedCount elements into collection starting at offset,
	returning the number of elements read, there could be less elements available.
	This is an inefficient abstract implementation, reading one by one."
	
	0 to: requestedCount - 1 do: [ :count | | object |
		(object := self next) ifNil: [ ^ count ].  
		collection at: offset + count put: object ].
	^ requestedCount
%

category: 'accessing'
method: ZnEncodedReadStream
skip: count
	count timesRepeat: [ self next ]
%

category: 'accessing'
method: ZnEncodedReadStream
upTo: anObject 	
	^ self collectionSpecies 
		streamContents: [ :out | | element |
			[ self atEnd or: [ (element := self next) = anObject ] ] whileFalse: [ 
				out nextPut: element ] ]
%

category: 'accessing'
method: ZnEncodedReadStream
upToEnd
	^ self collectionSpecies
		streamContents: [ :collectionStream | 
			[ self atEnd ] whileFalse: [ collectionStream nextPut: self next ] ]
%

! Class implementation for 'ZnCharacterReadStream'

!		Instance methods for 'ZnCharacterReadStream'

category: 'accessing'
method: ZnCharacterReadStream
collectionSpecies
	^ String
%

category: 'accessing'
method: ZnCharacterReadStream
match: subCollection 
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."
	| pattern startMatch |
	pattern := subCollection readStream.
	startMatch := nil.
	[ pattern atEnd ] whileFalse: 
		[ self atEnd ifTrue: [ ^ false ].
		self next = pattern next 
			ifTrue: [ pattern position = 1 ifTrue: [ startMatch := self position ] ]
			ifFalse: 
				[ pattern position: 0.
				startMatch ifNotNil: 
					[ self position: startMatch.
					startMatch := nil ] ] ].
	^ true
%

category: 'private'
method: ZnCharacterReadStream
nextElement
	^ self encoder nextFromStream: stream
%

category: 'accessing'
method: ZnCharacterReadStream
nextLine
	"Read a CR, LF or CRLF terminated line, returning the contents of the line without the EOL. Return nil when the receiver is #atEnd."
	
	self atEnd ifTrue: [ ^ nil ].
	^ self collectionSpecies streamContents: [ :out |
		out nextPutAll: (self encoder decodeBytes: stream nextLine) ]
%

category: 'accessing'
method: ZnCharacterReadStream
readInto: collection startingAt: offset count: requestedCount 
	"Read count elements and place them in collection starting at offset.
	Return the number of elements actually read."
	
	^ peeked 
		ifNil: [ | readCount |
			readCount := self encoder 
					readInto: collection 
					startingAt: offset 
					count: requestedCount 
					fromStream: stream.
			readCount ]
		ifNotNil: [ 
			collection at: offset put: peeked.
			peeked := nil.
			(self 
				readInto: collection 
				startingAt: offset + 1
				count: requestedCount - 1) + 1 ]
%

category: 'accessing'
method: ZnCharacterReadStream
upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."
	
	^ self collectionSpecies streamContents: [ :out |
		| partialMatch pattern matched |
		partialMatch := (self collectionSpecies new: aCollection size) writeStreamPortable.
		pattern := aCollection readStreamPortable.
		matched := false.
		([ matched or: [ self atEnd or: [ pattern atEnd ] ] ]) whileFalse: [
			| ch |
			(ch := self next) = pattern next
				ifTrue: [
					pattern atEnd
						ifTrue: [ matched := true ]
						ifFalse: [ partialMatch nextPut: ch ] ]
				ifFalse: [
					pattern reset.
					out nextPutAll: partialMatch contents.
					partialMatch reset.
					out nextPut: ch ] ].
		matched ifFalse: [ out nextPutAll: partialMatch contents ] ]
%

! Class implementation for 'ZnEncodedWriteStream'

!		Instance methods for 'ZnEncodedWriteStream'

category: 'accessing'
method: ZnEncodedWriteStream
<< collection
	^ self nextPutAll: collection
%

category: 'accessing'
method: ZnEncodedWriteStream
flush
	stream flush
%

category: 'testing'
method: ZnEncodedWriteStream
isBinary
	^ false
%

category: 'accessing'
method: ZnEncodedWriteStream
next: count putAll: collection	
	self 
		next: count 
		putAll: collection 
		startingAt: 1
%

category: 'accessing'
method: ZnEncodedWriteStream
next: count putAll: collection startingAt: offset
	"Write count items from collection starting at offset.
	This is an inefficient abstract implementation writing one by one."
	
	0 to: count - 1 do: [ :each | 
		self nextPut: (collection at: offset + each) ]
%

category: 'accessing'
method: ZnEncodedWriteStream
nextPut: anObject
	self subclassResponsibility
%

category: 'accessing'
method: ZnEncodedWriteStream
nextPutAll: collection

	stream nextPutAll: (self encoder encodeString: collection)
%

! Class implementation for 'ZnCharacterWriteStream'

!		Instance methods for 'ZnCharacterWriteStream'

category: 'accessing'
method: ZnCharacterWriteStream
cr
	self nextPut: Character cr
%

category: 'accessing'
method: ZnCharacterWriteStream
crlf
	self cr; lf
%

category: 'accessing'
method: ZnCharacterWriteStream
lf
	self nextPut: Character lf
%

category: 'accessing'
method: ZnCharacterWriteStream
next: count putAll: collection startingAt: offset
	"Write count characters from collection starting at offset."
	
	self encoder 
		next: count 
		putAll: collection 
		startingAt: offset 
		toStream: stream 
%

category: 'accessing'
method: ZnCharacterWriteStream
nextPut: object
	self encoder 
		nextPut: object 
		toStream: stream.
	^ object
%

category: 'accessing'
method: ZnCharacterWriteStream
print: object
	object printOn: self
%

category: 'accessing'
method: ZnCharacterWriteStream
space
	self nextPut: Character space
%

category: 'accessing'
method: ZnCharacterWriteStream
tab
	self nextPut: Character tab
%

! Class implementation for 'AbstractBinaryFileStream'

!		Class methods for 'AbstractBinaryFileStream'

category: 'instance creation'
classmethod: AbstractBinaryFileStream
handle: aCollection file: aFile forWrite: aTrue
	
	^ self basicNew
		handle: aCollection file: aFile forWrite: aTrue;
		yourself
%

!		Instance methods for 'AbstractBinaryFileStream'

category: 'testing'
method: AbstractBinaryFileStream
atEnd

	^ File atEnd: handle
%

category: 'accessing'
method: AbstractBinaryFileStream
contents
	"Answer the contents of the receiver while leaving the position unchanged.
	Fail if the receiver doesn't support positioning.
	#upToEnd provides an alternative that doesn't rely on stream positioning."

	| savedPosition contents |

	savedPosition := self position.
	self position: 0.
	contents := self upToEnd.
	self position: savedPosition.
	^contents
%

category: 'character writing'
method: AbstractBinaryFileStream
cr
	
	self nextPut: Character cr asInteger
%

category: 'character writing'
method: AbstractBinaryFileStream
crlf
	
	self nextPutAll: String crlf
%

category: 'accessing'
method: AbstractBinaryFileStream
file
	^ file
%

category: 'accessing'
method: AbstractBinaryFileStream
file: aFile 
	
	file := aFile
%

category: 'flushing'
method: AbstractBinaryFileStream
flush
	"When writing, this flushes the write buffer the stream uses to reduce
	the number of write() system calls it makes. This should generally be
	used before #sync, but on Windows they do the same thing."
	
	File flush: handle
%

category: 'initialization'
method: AbstractBinaryFileStream
handle: aCollection file: aFile forWrite: aBoolean
	
	handle := aCollection.
	file := aFile.
	forWrite := aBoolean
%

category: 'testing'
method: AbstractBinaryFileStream
isBinary
	^ true
%

category: 'character writing'
method: AbstractBinaryFileStream
lf

	self nextPut: Character lf asInteger
%

category: 'accessing'
method: AbstractBinaryFileStream
name

	^ file name
%

category: 'accessing'
method: AbstractBinaryFileStream
next
	"Answer the next byte from this file, or nil if at the end of the file."

	^ (self next: 1) ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]
%

category: 'accessing'
method: AbstractBinaryFileStream
next: n
	"Return a string with the next n characters of the filestream in it."

	^ self next: n into: (ByteArray new: n)
%

category: 'accessing'
method: AbstractBinaryFileStream
next: n into: aBuffer
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read |
	readBuffer := aBuffer.
	read := File read: handle into: readBuffer startingAt: 1 count: n.
	^read = n 
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
%

category: 'reading'
method: AbstractBinaryFileStream
next: n into: aString startingAt: startIndex
	"Read n bytes into the given string.
	Return aString or a partial copy if less than
	n elements have been read."
	|read|
	read := (self readInto: aString startingAt: startIndex count: n).
	^read = n 
		ifTrue: [ aString ]
		ifFalse: [ aString copyFrom: 1 to: startIndex + read - 1 ]	
%

category: 'writing'
method: AbstractBinaryFileStream
next: amount putAll: aByteArray

	forWrite
		ifFalse: [ ^ self error: 'Cannot write a read-only file' ].
	[ File
		write: handle
		from: aByteArray
		startingAt: 1
		count: amount ]
		on: Error
		do: [:ex | (FileWriteError fileName: self name)
				signal:
					(self close
						ifTrue: [ 'File ' , self name , ' is closed' ]
						ifFalse: [ 'File ' , self name , ' write failed' ]) ].
	^ aByteArray
%

category: 'writing'
method: AbstractBinaryFileStream
next: anInteger putAll: aCollection startingAt: startIndex
"Store the next anInteger elements from the given collection."
(startIndex = 1 and:[anInteger = aCollection size])
	ifTrue:[^self nextPutAll: aCollection].
^self nextPutAll: (aCollection copyFrom: startIndex to: startIndex+anInteger-1)
%

category: 'accessing'
method: AbstractBinaryFileStream
nextInto: aBuffer
	"Return a string with the next n characters of the filestream in it."

	^ self next: aBuffer size into: aBuffer
%

category: 'endianess'
method: AbstractBinaryFileStream
nextLittleEndianNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := self next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
%

category: 'endianess'
method: AbstractBinaryFileStream
nextLittleEndianNumber: n put: value
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [:i | bytes at: i put: (value digitAt: i)].
	self nextPutAll: bytes
%

category: 'writing'
method: AbstractBinaryFileStream
nextPut: anInteger

	^ self nextPutAll: (ByteArray with: anInteger asInteger)
%

category: 'writing'
method: AbstractBinaryFileStream
nextPutAll: aByteArray
	self next: aByteArray basicSize putAll: aByteArray
%

category: 'accessing'
method: AbstractBinaryFileStream
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	self subclassResponsibility
%

category: 'positioning'
method: AbstractBinaryFileStream
position

	^ File getPosition: handle
%

category: 'positioning'
method: AbstractBinaryFileStream
position: aPosition
	
	File setPosition: handle to: aPosition
%

category: 'printing'
method: AbstractBinaryFileStream
printOn: aStream
	"Put a printed version of the receiver onto aStream."

	aStream
		nextPutAll: self class name;
		nextPutAll: ': ';
		print: file name
%

category: 'reading'
method: AbstractBinaryFileStream
readInto: readBuffer startingAt: startIndex count: count

	^ File read: handle into: readBuffer startingAt: startIndex count: count
%

category: 'positioning'
method: AbstractBinaryFileStream
reset
	self position: 0
%

category: 'positioning'
method: AbstractBinaryFileStream
setToEnd
	
	self position: self size
%

category: 'accessing'
method: AbstractBinaryFileStream
size

	^ File sizeOf: handle
%

category: 'positioning'
method: AbstractBinaryFileStream
skip: n
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.
	By default we read n characters and we avoid reading the output"
	self next: n
%

category: 'flushing'
method: AbstractBinaryFileStream
sync	
	"When writing, this syncs any written/flushed data still in the kernel
	file system buffers to disk. This should generally be used after #flush,
	but on Windows they do the same thing."

	File sync: handle
%

category: 'accessing'
method: AbstractBinaryFileStream
upTo: delim

	^ self upToAnyOf: (ByteArray with: delim)
%

category: 'accessing'
method: AbstractBinaryFileStream
upToAnyOf: delimiters

	^ ByteArray new: 1000 streamContents: [ :stream | | ch |
		[ (ch := self next) isNil or: [ delimiters includes: ch] ] 
			whileFalse: [ stream nextPut: ch ] ]
%

category: 'accessing'
method: AbstractBinaryFileStream
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	^ByteArray streamContents: [ :newStream |
		| next |
		[ (next := self next) isNil ] whileFalse: [
			newStream nextPut: next ] ]
%

category: 'accessing'
method: AbstractBinaryFileStream
wrappedStreamName

	^self name
%

! Class implementation for 'BinaryFileStream'

!		Instance methods for 'BinaryFileStream'

category: 'open/close'
method: BinaryFileStream
close
	self closed
		ifTrue: [ ^ self ].
	handle close.
	handle := nil
%

category: 'testing'
method: BinaryFileStream
closed
	^ handle isOpen not
%

category: 'finalization'
method: BinaryFileStream
finalize

	^ self close
%

category: 'accessing'
method: BinaryFileStream
peek
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	self atEnd ifTrue: [^ nil].
	next := self next.
	self position: self position - 1.
	^ next
%

category: 'positioning'
method: BinaryFileStream
skip: n
	"Set the character position to n characters from the current position."
	self position: self position + n
%

category: 'positioning'
method: BinaryFileStream
truncate

	self truncate: 0
%

category: 'positioning'
method: BinaryFileStream
truncate: pos
	"Truncate to this position"

	^ File truncate: handle to: pos
%

! Class implementation for 'AbstractUUIDTest'

!		Class methods for 'AbstractUUIDTest'

category: 'Testing'
classmethod: AbstractUUIDTest
isAbstract
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'AbstractUUIDTest'
%

!		Instance methods for 'AbstractUUIDTest'

category: 'running'
method: AbstractUUIDTest
generatorClass
  self subclassResponsibility
%

category: 'running'
method: AbstractUUIDTest
setUp
  super setUp.
  currentUuidGeneratorClass := UUID _generatorClass.
  UUID generatorClass: self generatorClass
%

category: 'running'
method: AbstractUUIDTest
tearDown
  super tearDown.
  UUID generatorClass: currentUuidGeneratorClass
%

category: 'tests'
method: AbstractUUIDTest
testComparison
  "self debug: #testComparison"

  | a b |
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
  self assert: a < b.
  self deny: a > b.
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df642'.
  self assert: a < b.
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df640'.
  self assert: a > b
%

category: 'tests'
method: AbstractUUIDTest
testComparisonA
  | a b |
  a := UUID fromString: '0608b9dc-02e4-4dd0-9f8a-ea45160df641'.
  b := UUID fromString: 'e85ae7ba-3ca3-4bae-9f62-cc2ce51c525e'.
  self assert: a = a copy.
  self assert: b = b copy.
  self assert: a < b.
  self assert: b > a.
  self deny: a > b = (b > a)
%

category: 'tests'
method: AbstractUUIDTest
testCreation
  | uuid |
  uuid := UUID new.
  self should: [ uuid size = 16 ].
  self shouldnt: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationEquality
  | uuid1 uuid2 |
  uuid1 := UUID new.
  uuid2 := UUID new.
  self should: [ uuid1 = uuid1 ].
  self should: [ uuid2 = uuid2 ].
  self shouldnt: [ uuid1 = uuid2 ].
  self shouldnt: [ uuid1 hash = uuid2 hash ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationFromString
  | uuid string |
  string := UUID nilUUID asString.
  uuid := UUID fromString: string.
  self should: [ uuid size = 16 ].
  self should: [ uuid = UUID nilUUID ].
  self should: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ].
  self should: [ uuid asArray asSet size = 1 ].
  self should: [ (uuid asArray asSet asArray at: 1) = 0 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationFromStringNotNil
  | uuid string |
  string := UUID new asString.
  uuid := UUID fromString: string.
  self should: [ uuid size = 16 ].
  self should: [ uuid asString size = 36 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationNil
  | uuid |
  uuid := UUID nilUUID.
  self should: [ uuid size = 16 ].
  self should: [ uuid isNilUUID ].
  self should: [ uuid asString size = 36 ].
  self should: [ uuid asArray asSet size = 1 ].
  self should: [ (uuid asArray asSet asArray at: 1) = 0 ]
%

category: 'tests'
method: AbstractUUIDTest
testCreationNodeBased
  (UUID new asString copyLast: 12) = (UUID new asString copyLast: 12)
    ifFalse: [ ^ self ].
  1000
    timesRepeat: [ 
      | uuid |
      uuid := UUID new.
      self should: [ ((uuid at: 7) bitAnd: 16rF0) = 16r10 ].
      self should: [ ((uuid at: 9) bitAnd: 16rC0) = 16r80 ] ]
%

category: 'tests'
method: AbstractUUIDTest
testDuplicationsKinda
  | check size |
  size := 5000.
  check := Set new: size.
  size
    timesRepeat: [ 
      | uuid |
      uuid := UUID new.
      self shouldnt: [ check includes: uuid ].
      check add: uuid ]
%

category: 'tests'
method: AbstractUUIDTest
testOrder
  100
    timesRepeat: [ 
      | uuid1 uuid2 |
      uuid1 := UUID new.
      uuid2 := UUID new.
      (uuid1 asString copyLast: 12) = (uuid2 asString copyLast: 12)
        ifTrue: [ 
          self should: [ uuid1 < uuid2 ].
          self should: [ uuid2 > uuid1 ].
          self shouldnt: [ uuid1 = uuid2 ] ] ]
%

category: 'tests'
method: AbstractUUIDTest
testUniqueness
  "Test uniqueness for a given number of generated values"

  | maxTestValue |
  maxTestValue := 1000.
  self
    assert:
      ((1 to: maxTestValue) collect: [ :i | UUID new asString ]) asSet size
        = maxTestValue
%

! Class implementation for 'FastUUIDTest'

!		Instance methods for 'FastUUIDTest'

category: 'running'
method: FastUUIDTest
generatorClass
  ^ FastUUIDGenerator
%

! Class implementation for 'DirectoryEntryTest'

!		Instance methods for 'DirectoryEntryTest'

category: 'accessing'
method: DirectoryEntryTest
entry
	System sessionIsOnStoneHost ifFalse: [ self error: 'DirectoryEntryTest needs to be run on the stone''s host' ].
	^ FileLocator extent1 resolve entry
%

category: 'tests'
method: DirectoryEntryTest
testCreationTimeIsADateAndTimeInstance
	"While creation is the message sent to a directory entry, creation returns a DateAndTime object"

	| creation |
	creation := self entry creation.
	self assert: creation class equals: DateAndTime
	
%

category: 'tests'
method: DirectoryEntryTest
testIsDirectory
	| ref entry |
	ref := FileLocator imageDirectory resolve.
	entry := ref entry.
	self assert: entry isDirectory.
	self assert: FileLocator imageDirectory resolve equals: FileLocator dbfScratchDir resolve
%

category: 'tests'
method: DirectoryEntryTest
testIsFile
	self assert: self entry isFile.
	self deny: self entry isDirectory
%

category: 'tests'
method: DirectoryEntryTest
testIsNotDirectory
	self deny: self entry isDirectory
%

category: 'tests'
method: DirectoryEntryTest
testIsNotFile
	| ref |
	ref := FileLocator imageDirectory resolve.
	self deny: ref entry isFile
%

category: 'tests'
method: DirectoryEntryTest
testModificationTimeIsADateAndTimeInstance
	"While modification is the message sent to a directory entry, modification returns a DateAndTime object"

	| modification |
	modification := self entry modification.
	self assert: modification class equals: DateAndTime
	
%

category: 'tests'
method: DirectoryEntryTest
testReference
	| ref entry |
	ref := FileLocator image resolve.
	entry := ref entry.
	self assert: entry reference equals: ref
%

category: 'tests'
method: DirectoryEntryTest
testSize
	self assert: self entry size _isInteger
%

! Class implementation for 'FileLocatorTest'

!		Instance methods for 'FileLocatorTest'

category: 'compatibility tests'
method: FileLocatorTest
testAsAbsolute
	locator := FileLocator image.
	self assert: locator asAbsolute = locator
%

category: 'compatibility tests'
method: FileLocatorTest
testBasename
	locator := FileLocator image / 'griffle'.
	self assert: locator basename = 'griffle'
%

category: 'compatibility tests'
method: FileLocatorTest
testCommaAddsExtension
	locator := FileLocator image / 'griffle'.
	self assert: (locator , 'plonk') basename = 'griffle.plonk'
%

category: 'compatibility tests'
method: FileLocatorTest
testCommaAddsExtensionAgain
	locator := FileLocator image / 'griffle.plonk'.
	self assert: (locator , 'nurp') basename = 'griffle.plonk.nurp'
%

category: 'compatibility tests'
method: FileLocatorTest
testContainsLocator
	locator := FileLocator image.
	self assert: (locator contains: locator / 'griffle').
%

category: 'compatibility tests'
method: FileLocatorTest
testContainsPath
	
	locator := FileLocator image.
	self assert: (locator contains: (locator resolve / 'griffle') path).
%

category: 'compatibility tests'
method: FileLocatorTest
testContainsReference
	locator := FileLocator image.
	self assert: (locator contains: (locator resolve / 'griffle')).
%

category: 'tests'
method: FileLocatorTest
testCPath
	| ref |
	ref := FileLocator C / 'WINDOWS'.
	self assert: (ref fileSystem isKindOf: FileSystem).
	self assert: ref path = (Path / 'C:' / 'WINDOWS')
%

category: 'compatibility tests'
method: FileLocatorTest
testEqual
	| a b |
	a := FileLocator image.
	b := FileLocator image.
	self deny: a == b.
	self assert: a = b.
%

category: 'compatibility tests'
method: FileLocatorTest
testExtension
	locator := FileLocator image, 'bak'.
	self assert: (locator basename endsWith: '.bak')	
%

category: 'compatibility tests'
method: FileLocatorTest
testFileSystem
	locator := FileLocator image.
	self assert: (locator fileSystem isKindOf: FileSystem)
%

category: 'resolution tests'
method: FileLocatorTest
testImageDirectory
	locator := FileLocator image.
	self assert: locator resolve equals: FileLocator image resolve
%

category: 'compatibility tests'
method: FileLocatorTest
testIsAbsolute
	locator := FileLocator image.
	self assert: locator isAbsolute
%

category: 'compatibility tests'
method: FileLocatorTest
testIsNotRoot
	locator := FileLocator image.
	self deny: locator isRoot
%

category: 'compatibility tests'
method: FileLocatorTest
testIsRelative
	locator := FileLocator image.
	self deny: locator isRelative
%

category: 'compatibility tests'
method: FileLocatorTest
testIsRoot
	locator := FileLocator image.
	(locator resolve path size) timesRepeat: [locator := locator / '..'].
	self assert: locator canonicalize isRoot
%

category: 'compatibility tests'
method: FileLocatorTest
testMoveTo
	| old new |
	[
		old := FileLocator imageDirectory / 'testMoveTo_old'.
		old ensureCreateFile.
		
		new := FileLocator home / 'testMoveTo_new'.
		old moveTo: new.
		
		self deny: old exists.
		self assert: new exists.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]
%

category: 'compatibility tests'
method: FileLocatorTest
testOriginBasename
	locator := FileLocator image.
	self assert: locator basename = FileLocator image resolve basename
%

category: 'compatibility tests'
method: FileLocatorTest
testParent
	locator := FileLocator extent1.
	self assert: locator parent resolve = FileLocator extent1Directory resolve
%

category: 'resolution tests'
method: FileLocatorTest
testResolveAbsoluteReference
	| result reference |
	locator := FileLocator image / 'plonk'.
	reference := FileSystem memory / 'griffle'.
	result := locator resolve: reference.
	self assert: result == reference
%

category: 'resolution tests'
method: FileLocatorTest
testResolveCompoundString
	| result compound |
	locator := FileLocator image / 'plonk'.
	compound := 'griffle', locator fileSystem delimiter asString, 'nurp'.
	result := locator resolve: compound.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = ((Path * 'plonk') / 'griffle' / 'nurp')
%

category: 'resolution tests'
method: FileLocatorTest
testResolvePath
	| result path |
	locator := FileLocator image / 'plonk'.
	result := locator resolve: (Path * 'griffle').
	path := (Path * 'plonk') / 'griffle'.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = path.
%

category: 'resolution tests'
method: FileLocatorTest
testResolveRelativeReference
	| result reference |
	locator := FileLocator image / 'plonk'.
	self flag: 'this is a bit weird...'.
	reference := FileSystem memory * 'griffle'.
	result := locator resolve: reference.
	self assert: result class equals: locator class.
	self assert: result origin equals: locator origin.
	self assert: result path equals: reference path
%

category: 'resolution tests'
method: FileLocatorTest
testResolveString
	| result path |
	locator := FileLocator image / 'plonk'.
	result := locator resolve: 'griffle'.
	path := (Path * 'plonk') / 'griffle'.
	self assert: result class = locator class.
	self assert: result origin = locator origin.
	self assert: result path = path.
%

category: 'compatibility tests'
method: FileLocatorTest
testSlash
	locator := FileLocator image / 'griffle'.
	self assert: locator = (FileLocator image / 'griffle')
%

category: 'compatibility tests'
method: FileLocatorTest
testWithExtensionAddsExtension
	locator := FileLocator image / 'griffle'.
	self assert: (locator withExtension: 'plonk') basename = 'griffle.plonk'
%

category: 'compatibility tests'
method: FileLocatorTest
testWithExtensionReplacesExtension
	locator := FileLocator image / 'griffle.nurp'.
	self assert: (locator withExtension: 'plonk') basename = 'griffle.plonk'
%

! Class implementation for 'FileReferenceAttributeTests'

!		Class methods for 'FileReferenceAttributeTests'

category: 'resources'
classmethod: FileReferenceAttributeTests
resources

	^Array with: DiskFileAttributesTestsResources.
%

!		Instance methods for 'FileReferenceAttributeTests'

category: 'helper methods'
method: FileReferenceAttributeTests
tempFileResource

	^DiskFileAttributesTestsResources current
%

category: 'tests'
method: FileReferenceAttributeTests
testCreationTime

	| resource creationTime x |
	resource := self tempFileResource.
	creationTime := resource file creationTime.
	self assert: (x := resource beforeCreationTime) <= creationTime.
	self assert: (x := resource afterCreationTime) >= creationTime.
%

category: 'tests'
method: FileReferenceAttributeTests
testExists

	self assert: self tempFileResource file exists
%

category: 'tests'
method: FileReferenceAttributeTests
testFileSize

	self assert: self tempFileResource file size equals: 72.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsCharacter

	self deny: self tempFileResource file isCharacter
%

category: 'tests'
method: FileReferenceAttributeTests
testIsDirectory

	self deny: self tempFileResource file isDirectory.
	self assert: FileLocator temp isDirectory.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsFile

	self assert: self tempFileResource file isFile.
	self deny: FileLocator temp isFile.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsReadable

	self assert: self tempFileResource file isReadable.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsSymlink

	self deny: self tempFileResource file resolve isSymlink.
%

category: 'tests'
method: FileReferenceAttributeTests
testIsWriteable

	self assert: self tempFileResource file isWritable.
%

category: 'tests'
method: FileReferenceAttributeTests
testModificationTime

	| resource modificationTime |
	resource := self tempFileResource.
	modificationTime := resource file modificationTime.
	self assert: resource beforeCreationTime <= modificationTime.
	self assert: resource afterCreationTime >= modificationTime.
%

! Class implementation for 'FileReferenceTest'

!		Instance methods for 'FileReferenceTest'

category: 'support'
method: FileReferenceTest
createFile: aPath
	filesystem ensureCreateDirectory: aPath parent.
	(filesystem resolve: aPath) writeStream close
%

category: 'running'
method: FileReferenceTest
setUp
	super setUp.
	filesystem := FileSystem memory
%

category: 'tests'
method: FileReferenceTest
testAllChildren
	"allChildren returns all the files and folders recursively nested in a reference"

	<publicTest>
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref allChildren.
	"all children returns the nodes: '/alpha', '/alpha/beta',  '/alpha/beta/delta', and '/alpha/gamma'."
	self assert: children size equals: 4.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (ref = child or: [ ref contains: child ]) ].
	self assert: (children collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testAllDirectories
	"allDirectories returns all folders recursively nested in a reference"

	<publicTest>
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref allDirectories.
	"all children returns the directories: '/alpha', '/alpha/beta', and '/alpha/gamma'."
	self assert: children size equals: 4.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (ref = child or: [ ref contains: child ]) ].
	self assert: (children collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testAllEntries
	| ref entries |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	entries := ref allEntries.
	self assert: entries size equals: 4.
	entries
		do: [ :entry | 
			self assert: entry class equals: FileSystemDirectoryEntry.
			self assert: (ref = entry reference or: [ ref contains: entry reference ]) ].
	self assert: (entries collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testAsAbsoluteConverted
	"Converts a relative reference to absolute, and asserts
	that it's absolute and still has the same path."

	| ref absolute |
	ref := filesystem * 'plonk'.
	absolute := ref asAbsolute.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) equals: 'plonk'
%

category: 'tests'
method: FileReferenceTest
testAsAbsoluteIdentity
	| ref |
	ref := filesystem / 'plonk'.
	self assert: ref asAbsolute == ref
%

category: 'tests'
method: FileReferenceTest
testAsReference
	| ref |
	ref := filesystem * 'plonk'.
	self assert: ref asFileReference == ref
%

category: 'tests'
method: FileReferenceTest
testBaseAndExtension
	| noExtension simpleExtension complexExtension |
	noExtension := filesystem * 'plonk'.
	self assert: noExtension extension equals: ''.

	"We create a reference to the plonk/griffle.taz in the context of filesystem"
	simpleExtension := filesystem * 'plonk' / 'griffle.taz'.
	self assert: simpleExtension base equals: 'griffle'.
	self assert: simpleExtension extension equals: 'taz'.

	"Note that the extension of a complex extension starts after the last extension delimiter"
	complexExtension := filesystem * 'plonk' / 'griffle.taz.txt'.
	self assert: complexExtension base equals: 'griffle.taz'.
	self assert: complexExtension extension equals: 'txt'
%

category: 'tests'
method: FileReferenceTest
testBasename
	| ref |
	ref := filesystem root.
	self assert: ref basename equals: '/'.
	ref := filesystem * 'plonk' / 'griffle'.
	self assert: ref basename equals: 'griffle'.
	ref := filesystem * 'plonk' / 'griffle.taz'.
	self assert: ref basename equals: 'griffle.taz'
%

category: 'tests'
method: FileReferenceTest
testBasenameWithoutExtension
	
	| ref |	
	ref := filesystem root.
	self assert: ref basename equals: '/'.
	
	ref := filesystem * 'plonk' / 'griffle'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	ref := filesystem * 'plonk' / 'griffle.taz'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	ref := filesystem * 'plonk' / 'griffle.taz.zork'.
	self assert: ref basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.
%

category: 'tests'
method: FileReferenceTest
testCanonicalization

	| ref |

	ref := 'a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b' 'c').

	ref := '/a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b' 'c').

	ref := '../a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('..' 'a' 'b' 'c').

	ref := 'a/b/c/..' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b').

	ref := '/a/b/c/..' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b').

	ref := 'a/b/../c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'c').

	ref := '/a/b/../c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'c').
%

category: 'tests'
method: FileReferenceTest
testChildDirectories
	| childDirectories |
	[ filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.
	filesystem / 'delta' writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	filesystem / 'epsilon' writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childDirectories := filesystem root directories.
	self assert: childDirectories size equals: 2.
	childDirectories
		do: [ :each | 
			self assert: each class equals: FileReference.
			self
				assert: each isDirectory
				description: 'Collection should not contain references to files.' ] ]
		ensure: [ (filesystem / 'beta') ensureDeleteAll.
			(filesystem / 'gamma') ensureDeleteAll.
			(filesystem / 'delta') ensureDelete.
			(filesystem / 'epsilon') ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testChildFiles
	| childFiles |
	[ filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.
	filesystem / 'delta' writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	filesystem / 'epsilon' writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childFiles := filesystem root files.
	self assert: childFiles size equals: 2.
	childFiles
		do: [ :each | 
			self assert: each class equals: FileReference.
			self assert: each isFile description: 'Collection should not contain references to directories.' ] ]
		ensure: [ (filesystem / 'beta') ensureDeleteAll.
			(filesystem / 'gamma') ensureDeleteAll.
			(filesystem / 'delta') ensureDelete.
			(filesystem / 'epsilon') ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testChildOfPath
	| parent  child |
	parent := Path / 'griffle'.
	child := filesystem / 'griffle' / 'nurb'.
	self deny: (child isChildOf: parent).
	self deny: (parent isChildOf: child).
%

category: 'tests'
method: FileReferenceTest
testChildOfReference
	| parent  child |
	parent := filesystem / 'griffle'.
	child := filesystem / 'griffle' / 'nurb'.
	self assert: (child isChildOf: parent).
	self deny: (parent isChildOf: child).
%

category: 'tests'
method: FileReferenceTest
testChildren
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref children.
	self assert: children size equals: 2.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (child isChildOf: ref).
			self assert: (#('beta' 'gamma') includes: child basename) ] ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testCommaAddsExtension
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref , 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: FileReferenceTest
testCommaAddsExtensionAgain
	| ref result |
	ref := filesystem * 'plonk.griffle'.
	result := ref , 'nurp'.
	self assert: result basename equals: 'plonk.griffle.nurp'
%

category: 'tests'
method: FileReferenceTest
testContainsLocator
	| ref |
	ref := FileLocator imageDirectory resolve parent.
	self assert: (ref contains: FileLocator image)
%

category: 'tests'
method: FileReferenceTest
testContainsPath
	| ref |
	ref := filesystem  * 'griffle'.
	self assert: (ref contains: (ref / 'nurp') path)
%

category: 'tests'
method: FileReferenceTest
testContainsReference
	| ref |
	ref := filesystem * 'griffle'.
	self assert: (ref contains: ref / 'nurp')
%

category: 'tests'
method: FileReferenceTest
testContents
	| ref contents |
	contents := '12345 abcdf!'.
	ref := filesystem * 'file'.
	[ ref writeStreamDo: [ :stream | stream nextPutAll: contents ].
	self assert: ref contents asString equals: contents ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testDeleteAll
	"allChildren returns all the files and folders recursively nested in a reference"

	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	ref deleteAll.
	self deny: ref exists.
	self deny: (ref / 'beta') exists.
	self deny: (ref / 'beta' / 'delta') exists.
	self deny: (ref / 'beta' / 'gamma') exists ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testDeleteAllChildren
	"allChildren returns all the files and folders recursively nested in a reference"

	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	ref deleteAllChildren.
	self assert: ref exists.
	self deny: (ref / 'beta') exists.
	self deny: (ref / 'beta' / 'delta') exists.
	self deny: (ref / 'beta' / 'gamma') exists ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testDeleteIfAbsent
	| flag reference |
	flag := false.
	reference := filesystem / 'plonk'.
	reference ensureCreateFile.
	[ reference exists
		ifFalse: [ self error ].
	reference deleteIfAbsent: [ flag := true ].
	self deny: flag.
	reference exists
		ifTrue: [ self error ].
	reference deleteIfAbsent: [ flag := true ].
	self assert: flag ]
		ensure: [ reference ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testDoesContainReferenceFileSystem
	| ref other |
	ref := filesystem * 'griffle'.
	other := filesystem / 'griffle' / 'nurp'.
	self assert: (ref contains: other)
%

category: 'tests'
method: FileReferenceTest
testDoesNotContainReferenceWhenUsingDifferentInstancesOfMemoryFileSystem
	| ref other |
	ref := filesystem * 'griffle'.
	other := FileSystem memory / 'griffle' / 'nurp'.
	self deny: (ref contains: other)
%

category: 'tests'
method: FileReferenceTest
testDoesntContainLocator
	| ref |
	ref := filesystem * 'griffle'. 
	self deny: (ref contains: FileLocator image)
%

category: 'tests'
method: FileReferenceTest
testDoesntContainPath
	| ref |
	ref := filesystem * 'griffle'.
	self deny: (ref contains: (Path * 'nurp'))
%

category: 'tests'
method: FileReferenceTest
testDoesntContainReferencePath
	| ref other |
	ref := filesystem * 'griffle'.
	other := filesystem * 'nurp'.
	self deny: (ref contains: other)
%

category: 'tests'
method: FileReferenceTest
testEnsureDelete
	| reference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateFile.
	[ self assert: reference exists.
	reference ensureDelete.
	self deny: reference exists	"No-op if file does not exist" ]
		ensure: [ reference ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testEnsureDeleteAll
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	[ self assert: reference exists.
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
	reference ensureDeleteAll.
	self deny: childReference exists.
	self deny: reference exists ]
		ensure: [ reference ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testEnsureDeleteNonEmptyDirectory
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	[ self assert: reference exists.
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
	self should: [ reference ensureDelete ] raise: DirectoryIsNotEmpty ]
		ensure: [ reference ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testEntries
	[ | ref entries |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	entries := ref entries.
	self assert: entries size equals: 2.
	entries
		do: [ :entry | 
			self assert: entry class equals: FileSystemDirectoryEntry.
			self assert: (entry reference isChildOf: ref).
			self assert: (#('beta' 'gamma') includes: entry reference basename) ] ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testEqual
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'plonk'.
	self deny: a == b.
	self assert: a equals: b
%

category: 'tests'
method: FileReferenceTest
testEqualityRelativeVsAbsolute

	| f1 f2 |

	f1 := FileLocator workingDirectory / 'pharo-local'.
	f2 := f1 asAbsolute.
	self assert: f1 equals: f2
%

category: 'tests'
method: FileReferenceTest
testExists
	| reference |
	reference := filesystem / 'plonk'.
	reference ensureCreateFile.
	[ self assert: reference exists.
	reference delete.
	self deny: reference exists ]
		ensure: [ reference ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testGlob
	[ | ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem root.
	children := ref glob: [ :node | true ].
	self assert: children size == 4.	"including root"
	children := ref glob: [ :node | node basename size > 1 ].
	self assert: children size == 3.	"without root"
	children := ref glob: [ :node | node basename = 'gamma' ].
	self assert: children size == 1.	"gamma"
	self assert: children first basename equals: 'gamma' ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testGrandchildOfReference
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'griffle' / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: FileReferenceTest
testHasChildren
	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self assert: ref hasChildren.
	self assert: (ref / 'beta') hasChildren.
	self deny: (ref / 'beta' / 'delta') hasChildren.
	self deny: (ref / 'beta' / 'gamma') hasChildren ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testHasDirectories
	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self assert: ref hasDirectories.
	self deny: (ref / 'beta') hasDirectories.
	self deny: (ref / 'beta' / 'gamma') hasDirectories ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testHasFiles
	<publicTest>
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	filesystem createDirectory: '/alpha/beta/eta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self deny: ref hasFiles.
	self assert: (ref / 'beta') hasFiles.
	self deny: (ref / 'beta' / 'gamma') hasFiles ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
%

category: 'tests'
method: FileReferenceTest
testIndicator
	| ref |
	ref := filesystem * 'plonk' / 'griffle'.
	self deny: ref exists.
	self assert: ref indicator equals: '?'.
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	ref createDirectory.
	[ self assert: ref exists.
	self assert: ref isDirectory.
	self assert: ref indicator equals: '/' ]
		ensure: [ ref delete ].
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	[ ref writeStreamDo: [ :stream | stream nextPutAll: 'foo' ] ifPresent: [ self fail ].
	self assert: ref exists.
	self assert: ref isFile.
	self assert: ref indicator equals: '' ]
		ensure: [ ref delete ]
%

category: 'tests'
method: FileReferenceTest
testIsAbsolute
	self assert: (filesystem / 'plonk') isAbsolute
%

category: 'tests'
method: FileReferenceTest
testIsNotAbsolute
	self deny: (filesystem * 'plonk') isAbsolute
%

category: 'tests'
method: FileReferenceTest
testIsNotRelative
	self deny: (filesystem / 'plonk') isRelative
%

category: 'tests'
method: FileReferenceTest
testIsNotRoot
	self deny: (filesystem / 'plonk') isRoot
%

category: 'tests'
method: FileReferenceTest
testIsRelative
	self assert: (filesystem * 'plonk') isRelative
%

category: 'tests'
method: FileReferenceTest
testIsRoot
	self assert: (filesystem root) isRoot
%

category: 'tests'
method: FileReferenceTest
testMakeRelative

	| parent child relative |
	parent := filesystem / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := parent makeRelative: child.
	self assert: relative equals: (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testParent
	| ref parent |
	ref := filesystem * 'plonk' / 'griffle'.
	parent := ref parent.
	self assert: parent class = ref class.
	self assert: (parent path at: 1) = 'plonk'
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithAbsoluteReference
	| base relative absolute |
	base := filesystem / '/plonk' / 'pinto'.
	relative := filesystem / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute fileSystem == relative fileSystem.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) equals: 'griffle'.
	self assert: (absolute path at: 2) equals: 'zonk'
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithPath
	| base relative absolute |
	base := filesystem / 'plonk' / 'pinto'.
	relative := Path parent / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
	
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithReference
	| base relative absolute |
	base := (filesystem / 'plonk' / 'pinto').
	relative := (filesystem referenceTo: '../griffle/zonk').
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
	
	
%

category: 'tests'
method: FileReferenceTest
testParentResolutionWithRemoteReference
	| base relative absolute |
	base := filesystem / 'plonk' / 'pinto'.
	relative := filesystem referenceTo: '../griffle/zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk')
%

category: 'tests'
method: FileReferenceTest
testParentUpTo
	| base a b c |
	[ 
		base := filesystem workingDirectory.
		(base / 'testParentUpTo') ensureCreateDirectory.
		a := (base / 'testParentUpTo' / 'A') ensureCreateDirectory.
		b := (base / 'testParentUpTo' / 'A' / 'B') ensureCreateDirectory.
		c := (base / 'testParentUpTo' / 'A' / 'B' / 'C') ensureCreateDirectory.
		self assert: b equals: (c parentUpTo: 'B').
		self assert: a equals: (c parentUpTo: 'A').
		self assert: (base / 'testParentUpTo') equals: (c parentUpTo: 'testParentUpTo').
		self assert: base equals: (c parentUpTo: 'notAParent') 
	] ensure: [ 
		(base / 'testParentUpTo') ensureDeleteAll 
	]
%

category: 'tests'
method: FileReferenceTest
testPathRelativeTo
	| parent childPath relative |
	parent := filesystem / 'griffle'.
	childPath := Path / 'griffle' / 'plonk' / 'nurb'.
	relative := childPath relativeTo: parent.
	self assert: relative = (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testReadStream
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref readStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref delete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamDo
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref readStreamDo: [ :stream | self deny: stream isNil ] ]
		ensure: [ ref delete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamDoifAbsent
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref readStreamDo: [ :stream | self deny: stream isNil ] ifAbsent: [ self signalFailure: 'The file exists!' ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamDoifAbsentNot
	| ref pass |
	pass := false.
	ref := filesystem * 'plonk'.
	ref 
		readStreamDo: [:stream|]
		ifAbsent: [pass := true].
	self assert: pass
%

category: 'tests stream'
method: FileReferenceTest
testReadStreamDoNotFound
	| ref |
	ref := filesystem / 'plonk'.
	self
		should: [ref readStreamDo: [:s|]]
		raise: FileDoesNotExistException
%

category: 'tests'
method: FileReferenceTest
testReadStreamIfAbsent
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref readStreamIfAbsent: [ self signalFailure: 'Should not reach here.' ] ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testReadStreamNotFound
	| ref  |
	ref := filesystem * 'plonk'.
	self
		should: [ref readStream ]
		raise: FileDoesNotExistException
%

category: 'tests'
method: FileReferenceTest
testRelativeToPath
	| parentPath child relative |
	parentPath := Path / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := child relativeTo: parentPath.
	self assert: relative = (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testRelativeToReference
	| parent child relative |
	parent := filesystem / 'griffle'.
	child := filesystem  / 'griffle' / 'plonk' / 'nurb'.
	relative := child relativeTo: parent.
	self assert: relative = (Path * 'plonk' / 'nurb')
%

category: 'tests'
method: FileReferenceTest
testRename
	| file tmp originalFullName |
	[ file := (FileLocator imageDirectory / 'oldName') ensureCreateFile.
	originalFullName := file fullName.
	tmp := (FileLocator imageDirectory / 'tmp') ensureCreateDirectory.
	file renameTo: 'newName'.
	self deny: originalFullName asFileReference exists.
	self assert: file basename equals: 'newName'.
	self assert: (originalFullName asFileReference parent / 'newName') exists ]
		ensure: [ file delete.
			tmp deleteAll ]
%

category: 'tests'
method: FileReferenceTest
testRenameTargetExists

	| existingFile fileToRename |
	[
		existingFile := 'existingFile' asFileReference ensureCreateFile.
		fileToRename := 'fileToRename' asFileReference ensureCreateFile.
		self should: [ fileToRename renameTo: existingFile basename ] raise: FileExists ]
		ensure: [
			existingFile delete.
			fileToRename delete ].
%

category: 'tests'
method: FileReferenceTest
testResolve
	| ref |
	ref := filesystem / 'griffle'.
	self assert: ref resolve == ref
%

category: 'tests'
method: FileReferenceTest
testRootParent
	| root |
	root := filesystem root.
	self assert: root parent == root
%

category: 'tests'
method: FileReferenceTest
testSiblingOfReference
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: FileReferenceTest
testSimpleResolution
	| base relative absolute |
	base := filesystem / 'plonk'.
	relative := (Path * 'griffle') / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) = 'plonk'.
	self assert: (absolute path at: 2) = 'griffle'.
	self assert: (absolute path at: 3) = 'zonk'.
	
	
%

category: 'tests'
method: FileReferenceTest
testSlash
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref / 'griffle'.
	self assert: result class = ref class.
	self assert: result  isRelative.
	self assert: (result path at: 1) = 'plonk'.
	self assert: (result path at: 2) = 'griffle'.
%

category: 'tests'
method: FileReferenceTest
testTempFilePrefixSuffix
	| fileRef |
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	self assert: (fileRef isKindOf: FileReference).
	self assert: fileRef exists not.
%

category: 'tests'
method: FileReferenceTest
testUnequalContent
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: FileReferenceTest
testUnequalSize
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem / 'plonk' / 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: FileReferenceTest
testUpToAll
	| testString fileRef |
	testString := 'ße'.
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.

	[  
	fileRef
		writeStreamEncoded: 'utf8' do: [ :stream | 
			stream nextPutAll: testString ].

	fileRef
		readStreamDo: [ :stream |
			self assert: (stream upToAll: 'e') equals: 'ß'.
		] ]
	ensure: [ fileRef ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWithExtentionAddsExtension
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref withExtension: 'griffle'.
	self assert: result isRelative.
	self assert: result basename = 'plonk.griffle'
%

category: 'tests'
method: FileReferenceTest
testWithExtentionReplacesExtension
	| ref result |
	ref := filesystem * 'plonk.griffle'.
	result := ref withExtension: 'nurp'.
	self assert: result isRelative.
	self assert: result basename = 'plonk.nurp'
%

category: 'tests'
method: FileReferenceTest
testWithoutExtension
	| ref newRef |
	ref := filesystem * 'plonk' / 'griffle.txt'.
	newRef := ref withoutExtension.
	self assert: newRef parent equals: ref parent.
	self assert: newRef basename equals: 'griffle'
%

category: 'tests'
method: FileReferenceTest
testWorkingDirectoryParent
	| wd |
	wd := filesystem referenceTo: Path workingDirectory.
	self assert: wd parent path size = 1.
	self assert: (wd parent path at: 1) = '..'.
%

category: 'tests'
method: FileReferenceTest
testWriteStream
	| ref stream |
	ref := filesystem / 'plonk'.
	[ [ stream := ref writeStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDo
	| ref s |
	ref := filesystem / 'plonk'.
	[ ref
		writeStreamDo: [ :stream | 
			s := stream.
			self deny: stream isNil ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDoExists
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamDo: [ :stream | self deny: stream isNil ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDoifPresent
	| ref s |
	ref := filesystem / 'plonk'.
	[ ref
		writeStreamDo: [ :stream | 
			s := stream.
			self deny: stream isNil ]
		ifPresent: [ self signalFailure: 'The file does not exist!' ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamDoifPresentNot
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamDo: [ :stream |  ] ifPresent: [ pass := true ].
	self assert: pass ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamExists
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref writeStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamifPresent
	| ref stream |
	ref := filesystem / 'plonk'.
	[ [ stream := ref writeStreamIfPresent: [ self signalFailure: 'Should not reach here' ] ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
%

category: 'tests'
method: FileReferenceTest
testWriteStreamifPresentExists
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamIfPresent: [ pass := true ].
	self assert: pass ]
		ensure: [ ref ensureDelete ]
%

! Class implementation for 'FileSystemHandleTest'

!		Class methods for 'FileSystemHandleTest'

category: 'testing'
classmethod: FileSystemHandleTest
isAbstract
	^ self name = #FileSystemHandleTest
%

category: 'testing'
classmethod: FileSystemHandleTest
shouldInheritSelectors 
	^ true
%

!		Instance methods for 'FileSystemHandleTest'

category: 'running'
method: FileSystemHandleTest
createFileSystem
	self subclassResponsibility 
%

category: 'running'
method: FileSystemHandleTest
setUp
	super setUp.
	filesystem := self createFileSystem.
	reference := filesystem * 'plonk'.
	handle := reference openWritable: true
%

category: 'running'
method: FileSystemHandleTest
tearDown
	handle ensureClosed.
	reference ensureDelete.
	super tearDown
%

category: 'tests'
method: FileSystemHandleTest
testAt
	handle at: 1 write: (ByteArray with: 3) startingAt: 1 count: 1.
	self assert: (handle at: 1) = 3
%

category: 'tests'
method: FileSystemHandleTest
testAtPut
	| in |
	handle at: 1 put: 3.
	in := ByteArray new: 1.
	handle at: 1 read: in startingAt: 1 count: 1.
	self assert: in first = 3
%

category: 'tests'
method: FileSystemHandleTest
testAtPutBinaryAscii
	handle at: 1 put: 32.
	handle at: 1 put: Character space
%

category: 'tests'
method: FileSystemHandleTest
testAtWriteBinaryAscii
	handle
		at: 1
		write: #[32]
		startingAt: 1
		count: 1.
	handle
		at: 1
		write: (String with: Character space)
		startingAt: 1
		count: 1
%

category: 'tests'
method: FileSystemHandleTest
testClose
	handle close.
	self deny: handle isOpen
	
%

category: 'tests'
method: FileSystemHandleTest
testCreatedOpen
	
	
	self flag: 'TODO: activated once FileHandle is in use again!'.
	"self assert: handle isOpen"
%

category: 'tests'
method: FileSystemHandleTest
testEnsureClosed
	handle ensureClosed.
	self deny: handle isOpen.
	handle ensureClosed.
	reference ensureDelete.
	handle reference exists
		ifTrue: [ self error ].
	handle ensureClosed
%

category: 'tests'
method: FileSystemHandleTest
testIO
	| out in |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 3.
	handle at: 1 write: out startingAt: 1 count: 3.
	handle at: 1 read: in startingAt: 1 count: 3.
	self assert: out = in.
%

category: 'tests'
method: FileSystemHandleTest
testReadBufferTooLarge
	| out in result |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 5.
	in atAllPut: 9.
	handle at: 1 write: out startingAt: 1 count: 3.
	result := handle at: 1 read: in startingAt: 2 count: 4.
	self assert: result = 3.
	self assert: in = #(9 1 2 3 9) asByteArray.
%

category: 'tests'
method: FileSystemHandleTest
testReadOnly
	handle close.
	handle := reference openWritable: false.
	self 
		should: 
			[ handle 
				at: 1
				write: #(1 2 3 )
				startingAt: 1
				count: 3 ]
		raise: Error
%

category: 'tests'
method: FileSystemHandleTest
testReference
	self assert: handle reference = reference asAbsolute
%

category: 'tests'
method: FileSystemHandleTest
testSizeAfterGrow
	| out |
	out := #(1 2 3) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 3.
	self assert: handle size = 3
%

category: 'tests'
method: FileSystemHandleTest
testSizeNoGrow
	| bytes |
	bytes := #(1 2 3 4) asByteArray.
	handle at: 1 write: bytes startingAt: 1 count: 3.
	handle at: 4 write: bytes startingAt: 4 count: 1.
	self assert: handle size = 4
%

category: 'tests'
method: FileSystemHandleTest
testTruncate
	| out |
	out := #(1 2 3 4 5) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 5.
	handle truncateTo: 3.
	self assert: handle size = 3
%

category: 'tests'
method: FileSystemHandleTest
testWriteStream
	| stream |
	stream := handle binaryWriteStream.
	self assert: (stream respondsTo: #nextPut:)
%

! Class implementation for 'FileHandleTest'

!		Instance methods for 'FileHandleTest'

category: 'running'
method: FileHandleTest
createFileSystem
	^ FileSystem store: DiskStore activeClass createDefault
%

! Class implementation for 'MemoryHandleTest'

!		Instance methods for 'MemoryHandleTest'

category: 'running'
method: MemoryHandleTest
createFileSystem
	^ FileSystem memory
%

! Class implementation for 'FileSystemResolverTest'

!		Class methods for 'FileSystemResolverTest'

category: 'testing'
classmethod: FileSystemResolverTest
isAbstract
	^ self name = #FileSystemResolverTest
%

!		Instance methods for 'FileSystemResolverTest'

category: 'asserting'
method: FileSystemResolverTest
assertOriginResolves: aSymbol
	| reference |
	reference := resolver resolve: aSymbol.
	self assert: (reference isKindOf: FileReference).
	self assert: reference exists.
	self assert: reference isAbsolute.
	^ reference
%

category: 'running'
method: FileSystemResolverTest
createResolver
	self subclassResponsibility 
%

category: 'running'
method: FileSystemResolverTest
setUp
	super setUp.
	resolver := self createResolver.
%

! Class implementation for 'InteractiveResolverTest'

!		Instance methods for 'InteractiveResolverTest'

category: 'running'
method: InteractiveResolverTest
createResolver
	^ InteractiveResolver new
%

category: 'accessing'
method: InteractiveResolverTest
home
	^ FileLocator imageDirectory resolve
%

category: 'tests'
method: InteractiveResolverTest
testCached
	[ resolver resolve: #home ]
		on: ResolutionRequest
		do: [ :req | req resume: self home ].
	self assertOriginResolves: #home
%

category: 'tests'
method: InteractiveResolverTest
testNew
	[self assertOriginResolves: #home]
		on: ResolutionRequest
		do: [:req | req resume: self home].
	
%

! Class implementation for 'PlatformResolverTest'

!		Instance methods for 'PlatformResolverTest'

category: 'running'
method: PlatformResolverTest
createResolver
	^ PlatformResolver forCurrentPlatform
%

category: 'tests'
method: PlatformResolverTest
testCache
	| cache |
	cache := self assertOriginResolves: #cache
%

category: 'tests'
method: PlatformResolverTest
testHome
	| home |
	home := self assertOriginResolves: #home.
	self assert: home isDirectory
%

! Class implementation for 'SystemResolverTest'

!		Instance methods for 'SystemResolverTest'

category: 'running'
method: SystemResolverTest
createResolver
	^ SystemResolver new
%

category: 'testing'
method: SystemResolverTest
testDbfScratchDir
	self assertOriginResolves: #dbfScratchDir
%

category: 'testing'
method: SystemResolverTest
testExtent1
	self assertOriginResolves: #extent1
%

category: 'testing'
method: SystemResolverTest
testExtent1Directory
	self assertOriginResolves: #extent1Directory
%

category: 'testing'
method: SystemResolverTest
testImage
	self assertOriginResolves: #image
%

category: 'testing'
method: SystemResolverTest
testImageDirectory
	self assertOriginResolves: #imageDirectory
%

category: 'testing'
method: SystemResolverTest
testTranlog
	self assertOriginResolves: #tranlog
%

! Class implementation for 'FileSystemTest'

!		Class methods for 'FileSystemTest'

category: 'accessing'
classmethod: FileSystemTest
defaultTimeLimit
	^10 seconds
%

category: 'testing'
classmethod: FileSystemTest
isAbstract
	^ self name = #FileSystemTest
%

category: 'accessing'
classmethod: FileSystemTest
packageNamesUnderTest
	^ #('FileSystem')
%

category: 'testing'
classmethod: FileSystemTest
shouldInheritSelectors
	^ true
	
%

!		Instance methods for 'FileSystemTest'

category: 'initialize-release'
method: FileSystemTest
createFileSystem
	self subclassResponsibility 
%

category: 'initialize-release'
method: FileSystemTest
markForCleanup: anObject
	toDelete add: (filesystem resolve: anObject)
%

category: 'running'
method: FileSystemTest
setUp
	super setUp.
	filesystem := self createFileSystem.
	toDelete := OrderedCollection new.
%

category: 'running'
method: FileSystemTest
tearDown
	toDelete
		select: [ :path | filesystem exists: path ]
		thenDo: [ :path | filesystem delete: path ].
	super tearDown	
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ reference binaryReadStream ] raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self 
		should: [ reference binaryReadStreamDo: [ :stream | self assert: false ] ] 
		raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference readStreamDo: [ :stream | stream contents asString ]) 
		= 'griffle'
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStreamDoIfAbsent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		binaryReadStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		binaryReadStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])
%

category: 'tests-streams-compatibility'
method: FileSystemTest
testBinaryReadStreamIfAbsent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference binaryReadStreamIfAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStreamIfAbsent: [ false ].
	self assert: stream contents asString = 'griffle'.
	stream close
%

category: 'tests'
method: FileSystemTest
testChildrenAt
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self markForCleanup: directory / 'griffle'.
	self markForCleanup: directory / 'bint'.
	self markForCleanup: directory.
	
	entries := filesystem childrenAt: directory.
	
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: Path).
		self assert: ea parent = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea basename) ]
%

category: 'tests'
method: FileSystemTest
testChildrenSorting
	| directory sorted |
	
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'alfa'.
	filesystem createDirectory: directory / 'beta'.
	
	self markForCleanup: directory / 'alfa'.
	self markForCleanup: directory / 'beta'.
	self markForCleanup: directory.
	
	sorted := (filesystem childrenAt: directory) sort.
	self assert: sorted size = 2.
	self assert: (sorted at: 1) basename = 'alfa'.
	self assert: (sorted at: 2) basename = 'beta'.
%

category: 'tests'
method: FileSystemTest
testChildrenSortingRoot
	| directory1 directory2 |
	"self skip."
	
	directory1 := Path * 'plonk1'.
	directory2 := Path * 'plonk2'.
	
	filesystem createDirectory: directory1.
	filesystem createDirectory: directory2.
	
	self markForCleanup: directory1.
	self markForCleanup: directory2.
	
	self assert: filesystem workingDirectory children sort size = filesystem workingDirectory children size
%

category: 'tests'
method: FileSystemTest
testCopy
	| out in contents |
	
	self
		markForCleanup: 'gooly';
		markForCleanup: 'plonk'.
		
	out := (filesystem workingDirectory / 'gooly') writeStream.
	[ out nextPutAll: 'gooly' ] ensure: [ out close ].
	filesystem copy: 'gooly' to: 'plonk'.
	
	in := (filesystem workingDirectory / 'plonk') readStream.
	contents := [ in contents asString ] ensure: [ in close ].
	self assert: contents equals: 'gooly'
%

category: 'tests'
method: FileSystemTest
testCopyAndDelete
	"Check that FileSystem>>copyAndDelete:to: works within a filesystem.
	DiskFileSystemTest>>testMoveMemoryToDisk checks that #copyAndDelete:to: works across filesystems."
	| folder testString f1 f1s f2 |
	
	folder := filesystem workingDirectory.
	testString := 'To be copied and deleted'.

	f1 := folder / 'f1'.
	f1s := f1 writeStream.
	[ f1s nextPutAll: testString ] ensure: [ f1s close ].
	f2 := folder / 'f2'.
	
	"Cleanup after running"
	self 
		markForCleanup: f1;
		markForCleanup: f2.	
	
	filesystem copyAndDelete: f1 to: f2.
	self deny: f1 exists.
	self assert: f2 exists.
	self assert: f2 contents equals: testString.
%

category: 'tests'
method: FileSystemTest
testCopyDestExists
	| out |
	
	self 
		markForCleanup: 'gooly'; 
		markForCleanup: 'plonk'.
		
	out := (filesystem  workingDirectory / 'gooly') writeStream.
	[out nextPutAll: 'gooly'] ensure: [out close].

	(filesystem  workingDirectory / 'plonk') writeStream close.
	
	self 
		should: [filesystem copy: 'gooly' to: 'plonk']
		raise: FileExists
%

category: 'tests'
method: FileSystemTest
testCopySourceDoesntExist
	self 
		should: [filesystem copy: 'plonk' to: 'griffle']
		raise: FileDoesNotExistException
%

category: 'tests'
method: FileSystemTest
testCopyWithCorrectBasename
        | directory |
        self
                markForCleanup: 'gooly';
                markForCleanup: 'plonk'.
        directory := filesystem workingDirectory.
        (directory / 'gooly') ensureCreateFile.
        directory / 'gooly' copyTo: directory / 'plonk'.
        self assert: (directory / 'plonk') exists.
        self assert: (directory childNames includes: 'plonk')
%

category: 'tests'
method: FileSystemTest
testCreateDirectory
	| path directory |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffle'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffle'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [path createDirectory] raise:Error.
	self assert: path exists.
	(directory / 'plonk' ) deleteAll.
%

category: 'tests'
method: FileSystemTest
testCreateDirectoryExists
	| path |
	
	path := Path * 'griffle'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	self 
		should: [ filesystem createDirectory: path]
		raise: DirectoryExists.
%

category: 'tests'
method: FileSystemTest
testCreateDirectoryNoParent
	| path |
	path := Path * 'griffle' / 'nurp'.
	self 
		should: [filesystem createDirectory: path]
		raise: DirectoryDoesNotExist.
	
	
%

category: 'tests'
method: FileSystemTest
testCreateDirectoryNotCreateParent
	| path |
	path := filesystem workingDirectory / 'plonk' / 'griffle'.
	self should:[path createDirectory] raise: DirectoryDoesNotExist.
	self assert: path exists not.
%

category: 'tests'
method: FileSystemTest
testCreateFile
	| directory path |
	directory := filesystem workingDirectory.
 	self markForCleanup: directory / 'plonk' / 'griffles'.
	self markForCleanup: directory / 'plonk'.
	path := directory / 'plonk' / 'griffles'.
	(directory / 'plonk') ensureCreateDirectory.
	self shouldnt: [ path createFile] raise:Error.
	self assert:path exists .
	(directory / 'plonk') deleteAll.
%

category: 'tests'
method: FileSystemTest
testCreateFileNotCreateParent
	| path |
	path := '/plonk/griffles' asFileReference.
	self should:[path createFile] raise: DirectoryDoesNotExist .
	self assert: path exists not.
%

category: 'tests'
method: FileSystemTest
testDefaultWorkingDirectory
	self assert: filesystem workingDirectory isRoot
%

category: 'tests'
method: FileSystemTest
testDelete
	"Unlike ensureDelete, delete raises an exception if the file does not exist."
	| reference |
	
	reference := filesystem workingDirectory / 'does-not-exist'.
	self deny: reference exists.
	
	self 
		should: [ reference delete ]
		raise: FileDoesNotExistException.
		
		
	reference := ( filesystem workingDirectory / 'file') ensureCreateFile.
	reference delete.
	
	self deny: reference exists. 
		
%

category: 'tests'
method: FileSystemTest
testDelimiter
	self assert: filesystem delimiter isCharacter
%

category: 'tests'
method: FileSystemTest
testDirectory
	| path |
	
	path := Path * 'plonk'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	
	self assert: (filesystem exists: path).
	self assert: (filesystem isDirectory: path).
	
	filesystem delete: path.
	self deny: (filesystem isFile: path).
	self deny: (filesystem exists: path)
%

category: 'tests'
method: FileSystemTest
testEnsureDirectory
	| path |
	
	path := Path * 'plonk'.
	self markForCleanup: path.
	
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: path).
%

category: 'tests'
method: FileSystemTest
testEnsureDirectoryCreatesParent
	| path |
	path := Path * 'plonk' / 'griffle'.
	self markForCleanup: path.
	self markForCleanup: path parent.
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: Path * 'plonk').
	self assert: (filesystem isDirectory: path)
%

category: 'tests'
method: FileSystemTest
testEnsureDirectoryExists
	| path |
	path := Path * 'plonk'.
	self markForCleanup: path.
	filesystem createDirectory: path.
	filesystem ensureCreateDirectory: path
%

category: 'tests'
method: FileSystemTest
testEntriesAt
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self 
		markForCleanup: directory / 'griffle';
		markForCleanup: directory / 'bint';
		markForCleanup: directory.
	
	entries := filesystem entriesAt: directory.
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: FileSystemDirectoryEntry).
		self assert: ea reference parent path = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea reference basename).
		self assert: ea isDirectory ]
%

category: 'tests'
method: FileSystemTest
testEntryAt
	| path1 path2 entry1 entry2  |

	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	self markForCleanup: path2.
	
	filesystem createDirectory: path1.
	(Delay forSeconds: 2) wait. "#creationTime seems limited to 1 second resolution"
	filesystem createDirectory: path2.
	
	entry1 := filesystem entryAt: path1.
	entry2 := filesystem entryAt: path2.
	
	self assert: entry1 isDirectory.
	self assert: entry2 isDirectory.
	self assert: entry1 reference = (filesystem referenceTo: path1) asAbsolute.
	self assert: entry2 reference = (filesystem referenceTo: path2) asAbsolute.

	self assert: entry1 creationTime < entry2 creationTime.
	self assert: entry1 modificationTime < entry2 modificationTime.
%

category: 'tests'
method: FileSystemTest
testFile
	| path |
	
	path := Path * 'gooly'.
	self markForCleanup: path.
	
	(filesystem workingDirectory resolve: path) writeStream close.
	self assert: (filesystem exists: path).
	self deny: (filesystem isDirectory: path).
	self assert: (filesystem isFile: path).
	
	filesystem delete: path.
	self deny: (filesystem exists: path)
%

category: 'tests'
method: FileSystemTest
testFileNames
	| reference |
	#('test one' 'test with two' 'test-äöü' 'test.äöü') do: [ :each |
		reference := filesystem workingDirectory / each.
		self assert: reference basename = each.
		self deny: reference exists.
		reference
			writeStreamDo: [ :stream | stream nextPutAll: 'gooly' ]
			ifPresent: [ self fail ].
		[	self assert: reference exists.
			self assert: (filesystem workingDirectory children
				anySatisfy: [ :ref | ref = reference ]) ]
		ensure: [ reference delete ] ]
%

category: 'tests'
method: FileSystemTest
testMoveTo
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	file moveTo: (folder / 'newFile').
	self deny: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testMoveToFailingExistingDestination
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	(folder / 'newFile') ensureCreateFile.
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	"Destination exists already"
	self should: [ file moveTo: (folder / 'newFile') ] raise: Error.
	self assert: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testMoveToFailingMissingDestination
	| base file |
	
	base := filesystem workingDirectory.
	
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	"Destination exists already"
	self deny: (base / 'folder') exists.
	self should: [ file moveTo: (base / 'folder' / 'newFile') ] raise: Error.
	self assert: (base / 'file') exists.
	self deny: (base / 'folder' / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testMoveToFailingMissingSource
	| base folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder').
	
	self deny: (base / 'file') exists.
	"Destination exists already"
	self should: [ (base / 'file') moveTo: (folder / 'newFile') ] raise: Error.
	self deny: (base / 'file') exists.
	self deny: (folder / 'newFile') exists.
%

category: 'tests'
method: FileSystemTest
testNonExistentEntryAt
	| path1 path2  |
	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	filesystem createDirectory: path1.

	self shouldnt: [ filesystem entryAt: path1 ] raise: FileDoesNotExistException.
	self should: [ filesystem entryAt: path2 ] raise: FileDoesNotExistException
%

category: 'tests'
method: FileSystemTest
testNonExistentFileSize
	| base file1 file2 |
	
	base := filesystem workingDirectory.
	file1 := (base / 'file1') ensureCreateFile.
	file2 := (base / 'file2').
	self markForCleanup: base / 'file1'.
	
	self shouldnt: [ file1 size ] raise: FileDoesNotExistException.
	self should: [ file2 size ] raise: FileDoesNotExistException
%

category: 'tests-streams'
method: FileSystemTest
testReadingAfterWriting
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self deny: reference exists.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents equals: 'griffle'.
	stream close
%

category: 'tests-streams'
method: FileSystemTest
testReadStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ reference readStream ] raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close
%

category: 'tests-streams'
method: FileSystemTest
testReadStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self 
		should: [ reference readStreamDo: [ :stream | self assert: false ] ] 
		raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference readStreamDo: [ :stream | stream contents asString ]) 
		= 'griffle'
%

category: 'tests-streams'
method: FileSystemTest
testReadStreamDoIfAbsent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		readStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		readStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])
%

category: 'tests-streams'
method: FileSystemTest
testReadStreamIfAbsent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference readStreamIfAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStreamIfAbsent: [ false ].
	self assert: stream contents asString = 'griffle'.
	stream close
%

category: 'tests'
method: FileSystemTest
testReferenceTo
	|path|
	"use a relative path since absolute path behavior differs on mac, linux vs win native filesystems"
	path := Path * 'a' / 'b'.
	self assert: (filesystem referenceTo: 'a/b') path = path.
%

category: 'tests'
method: FileSystemTest
testRoot
	self assert: filesystem root fileSystem = filesystem.
	self assert: filesystem root path = Path root.
	self assert: filesystem root isRoot.
%

category: 'tests'
method: FileSystemTest
testRootExists
	self assert: (filesystem exists: Path root)
%

category: 'tests'
method: FileSystemTest
testRootIsDirectory
	self assert: (filesystem isDirectory: Path root)
%

category: 'tests'
method: FileSystemTest
testRootIsNotAFile
	self deny: (filesystem isFile: Path root)
%

category: 'tests'
method: FileSystemTest
testWorking
	self assert: filesystem workingDirectory fileSystem = filesystem.
	self assert: filesystem workingDirectory path = filesystem workingDirectoryPath
%

category: 'tests-streams'
method: FileSystemTest
testWriteStream
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStream.
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	stream := reference writeStream.
	stream close
%

category: 'tests-streams'
method: FileSystemTest
testWriteStreamDo
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference writeStreamDo: [ :stream |
		stream nextPutAll: 'griffle'.
		true ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamDo: [ :stream | true ])
%

category: 'tests-streams'
method: FileSystemTest
testWriteStreamDoIfPresent
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		writeStreamDo: [ :stream |
			stream nextPutAll: 'griffle'.
			true ]
		ifPresent: [ false ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference 
		writeStreamDo: [ :stream | true ]
		ifPresent: [ true ])
%

category: 'tests-streams'
method: FileSystemTest
testWriteStreamIfPresent
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStreamIfPresent: [ false ].
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamIfPresent: [ true ])
%

! Class implementation for 'DiskFileSystemTest'

!		Instance methods for 'DiskFileSystemTest'

category: 'initialize-release'
method: DiskFileSystemTest
createFileSystem
	^ FileSystem store: (DiskStore activeClass createDefault)
%

category: 'tests'
method: DiskFileSystemTest
testDefaultWorkingDirectory
	| ref x y |
	ref := filesystem workingDirectory.
	self assert: ((x := GsFile _expandEnvVariable: 'PWD' isClient:false) beginsWith: (y := ref pathString))
%

category: 'tests'
method: DiskFileSystemTest
testEqual
	| other |
	other := self createFileSystem.
	self assert: filesystem = other
%

category: 'tests'
method: DiskFileSystemTest
testIsDirectory
	self assert: (filesystem isDirectory: FileLocator imageDirectory resolve path)
%

category: 'tests'
method: DiskFileSystemTest
testIsDiskFileSystem
	self assert: filesystem isDiskFileSystem.
	
%

category: 'tests'
method: DiskFileSystemTest
testMoveMemoryToDisk
	"Test moving a file from the memory filesystem to the disk filesystem.
	This ensures that the copyAndDelete:to: is called correctly."
	| memfs out testString old new |
	[
		memfs := FileSystem memory.
		old := memfs / 'testMoveMemoryToDisk_old'.
		out := old writeStream.
		testString := 'To be moved to disk'.
		[ out nextPutAll: testString ] ensure: [ out close ].
		
		new := FileLocator imageDirectory / 'testMoveMemoryToDisk_new'.
		old moveTo: new.
		
		self deny: (memfs / 'testMoveMemoryToDisk_old') exists.
		self assert: new exists.
		self assert: new contents equals: testString.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]
%

! Class implementation for 'MemoryFileSystemTest'

!		Instance methods for 'MemoryFileSystemTest'

category: 'initialize-release'
method: MemoryFileSystemTest
createFileSystem
	^ FileSystem memory
%

category: 'tests'
method: MemoryFileSystemTest
lastModificationTimeOf: fileReference
	"DateAndTime primUTCMicrosecondsClock is not precise across all OS so put in slight delay between calling modificationTime"

	^ [ fileReference modificationTime ]
		ensure: [ (Delay forMilliseconds: 100) wait ]
%

category: 'tests'
method: MemoryFileSystemTest
testCurrentEqual
	| other another |
	another := FileSystem currentMemoryFileSystem.
	other := FileSystem currentMemoryFileSystem.
	self assert: other equals: another
%

category: 'tests'
method: MemoryFileSystemTest
testIsMemoryFileSystem
	self assert: filesystem isMemoryFileSystem.
	
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileCreated
	self assert: (filesystem / 'file.txt') ensureCreateFile modificationTime notNil
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileModifiedByWriteStream
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference writeStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileModifiedWithBinaryWriteStream
	| modifiedTime fileReference data |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	data := 'some data'.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: data ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime.
	self
		assert: data asByteArray
		equals: (fileReference binaryReadStreamDo: [ :aStream | aStream upToEnd ]).
	self assert: data equals: (fileReference readStreamDo: [ :aStream | aStream contents ])
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenFileWrittenTo
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
%

category: 'tests'
method: MemoryFileSystemTest
testModifiedTimeWhenHandleTruncated
	| modifiedTime fileReference handle |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	handle := fileReference openWritable: true.
	modifiedTime := self lastModificationTimeOf: fileReference.
	handle truncate.
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
%

category: 'tests'
method: MemoryFileSystemTest
testNotEqualWhenCreatingNewMemoryFileSystem
	| other |
	other := FileSystem memory.
	self deny: other equals: filesystem
%

category: 'tests'
method: MemoryFileSystemTest
testNotEqualWhenRequestingMemoryFileSystem
	| other |
	other := self createFileSystem.
	self deny: other equals: filesystem
%

! Class implementation for 'FileSystemTreeTest'

!		Class methods for 'FileSystemTreeTest'

category: 'testing'
classmethod: FileSystemTreeTest
isAbstract
	^ self name = #FileSystemTreeTest
%

!		Instance methods for 'FileSystemTreeTest'

category: 'running'
method: FileSystemTreeTest
createDirectory: aString
	self subclassResponsibility
%

category: 'running'
method: FileSystemTreeTest
createFile: aString
	self subclassResponsibility
%

category: 'running'
method: FileSystemTreeTest
setUpGreek
	self 
		createDirectory: '/alpha';
		createDirectory: '/alpha/beta';
		createFile: '/alpha/beta/gamma';
		createFile: '/alpha/beta/delta';
		createDirectory: '/alpha/epsilon';
		createFile: '/alpha/epsilon/zeta'
%

! Class implementation for 'CopyVisitorTest'

!		Instance methods for 'CopyVisitorTest'

category: 'running'
method: CopyVisitorTest
createDirectory: aString 
	source createDirectory: (source store pathFromString: aString)
%

category: 'running'
method: CopyVisitorTest
createFile: aString 
	source store createFile: (source store pathFromString: aString)
%

category: 'running'
method: CopyVisitorTest
setUp
	super setUp.
	source := FileSystem memory.
	dest := FileSystem memory
%

category: 'tests'
method: CopyVisitorTest
testAll
	self setUpGreek.
	CopyVisitor 
		copy: (source / 'alpha') 
		to: (dest / 'alpha').
	self assert: (dest isDirectory: '/alpha').
	self assert: (dest isFile: '/alpha/beta/gamma').
%

! Class implementation for 'SingleTreeTest'

!		Class methods for 'SingleTreeTest'

category: 'testing'
classmethod: SingleTreeTest
isAbstract
	^ self name = #SingleTreeTest
%

!		Instance methods for 'SingleTreeTest'

category: 'running'
method: SingleTreeTest
createDirectory: aString 
	filesystem createDirectory: (filesystem pathFromString: aString)
%

category: 'running'
method: SingleTreeTest
createFile: aString 
	filesystem store createFile: (filesystem pathFromString: aString)
%

category: 'running'
method: SingleTreeTest
setUp
	super setUp.
	filesystem := FileSystem memory
%

! Class implementation for 'AbstractEnumerationVisitorTest'

!		Class methods for 'AbstractEnumerationVisitorTest'

category: 'as yet unclassified'
classmethod: AbstractEnumerationVisitorTest
isAbstract
	^ self name = #AbstractEnumerationVisitorTest
%

!		Instance methods for 'AbstractEnumerationVisitorTest'

category: 'utilities'
method: AbstractEnumerationVisitorTest
assertEntries: references are: expected
	| strings |
	self assert: references _isArray.
	references do: [ :ea | self assert: ea class equals: FileSystemDirectoryEntry ].
	strings := references collect: [ :ea | ea reference pathString ].
	self assert: strings equals: expected
%

category: 'accessing'
method: AbstractEnumerationVisitorTest
root
	^ filesystem / 'alpha'
%

category: 'running'
method: AbstractEnumerationVisitorTest
setUp
	super setUp.
	self setUpGreek.
%

! Class implementation for 'CollectVisitorTest'

!		Instance methods for 'CollectVisitorTest'

category: 'tests'
method: CollectVisitorTest
testBreadthFirst
	| entries |
	entries := CollectVisitor breadthFirst: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/epsilon'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: CollectVisitorTest
testPostorder
	| entries |
	entries := CollectVisitor postorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha/epsilon'
				'/alpha'
			)
%

category: 'tests'
method: CollectVisitorTest
testPreorder
	| entries |
	entries := CollectVisitor preorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon'
				'/alpha/epsilon/zeta'
			)
%

! Class implementation for 'SelectVisitorTest'

!		Instance methods for 'SelectVisitorTest'

category: 'tests'
method: SelectVisitorTest
testBreadthFirst
	| entries |
	entries := SelectVisitor breadthFirst: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/epsilon'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: SelectVisitorTest
testBreadthFirstSelect
	| entries |
	entries := SelectVisitor breadthFirst: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: SelectVisitorTest
testPostorder
	| entries |
	entries := SelectVisitor postorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha/epsilon'
				'/alpha'
			)
%

category: 'tests'
method: SelectVisitorTest
testPostorderSelect
	| entries |
	entries := SelectVisitor postorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/beta'
				'/alpha/epsilon/zeta'
				'/alpha'
			)
%

category: 'tests'
method: SelectVisitorTest
testPreorder
	| entries |
	entries := SelectVisitor preorder: self root.
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon'
				'/alpha/epsilon/zeta'
			)
%

category: 'tests'
method: SelectVisitorTest
testPreorderSelect
	| entries |
	entries := SelectVisitor preorder: self root select: [ :node| node name endsWith: #a].
	self 
		assertEntries: entries
		are: #(
				'/alpha'
				'/alpha/beta'
				'/alpha/beta/delta'
				'/alpha/beta/gamma'
				'/alpha/epsilon/zeta'
			)
%

! Class implementation for 'DeleteVisitorTest'

!		Instance methods for 'DeleteVisitorTest'

category: 'tests'
method: DeleteVisitorTest
testBeta
	self setUpGreek.
	DeleteVisitor delete: (filesystem / 'alpha' / 'beta').
	self assert: (filesystem isDirectory: '/alpha').
	self assert: (filesystem isDirectory: '/alpha/epsilon').
	self deny: (filesystem exists: '/alpha/beta').
	
%

! Class implementation for 'GuideTest'

!		Class methods for 'GuideTest'

category: 'testing'
classmethod: GuideTest
isAbstract
	^ self name = #GuideTest
%

!		Instance methods for 'GuideTest'

category: 'asserting'
method: GuideTest
assertVisitedIs: anArray
	visited with: anArray do:
		[:entry :basename | 
		self assert: entry reference basename = basename]
%

category: 'running'
method: GuideTest
setUp
	super setUp.
	visited := OrderedCollection new.
	filesystem := FileSystem memory.
	self setUpGreek
%

category: 'visitor'
method: GuideTest
visitDirectory: aReference
	visited add: aReference.
%

category: 'visitor'
method: GuideTest
visitFile: aReference
	visited add: aReference.
%

! Class implementation for 'BreadthFirstGuideTest'

!		Instance methods for 'BreadthFirstGuideTest'

category: 'tests'
method: BreadthFirstGuideTest
testAll
	guide := BreadthFirstGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'alpha'
			'beta'
			'epsilon'
			'delta'
			'gamma'
			'zeta'
		)
%

! Class implementation for 'PostorderGuideTest'

!		Instance methods for 'PostorderGuideTest'

category: 'tests'
method: PostorderGuideTest
testAll
	guide := PostorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'delta'
			'gamma'
			'beta'
			'zeta'
			'epsilon'
			'alpha'
		)
%

! Class implementation for 'PreorderGuideTest'

!		Instance methods for 'PreorderGuideTest'

category: 'tests'
method: PreorderGuideTest
testAll
	guide := PreorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'alpha'
			'beta'
			'delta'
			'gamma'
			'epsilon'
			'zeta'
		)
%

! Class implementation for 'FileTest'

!		Instance methods for 'FileTest'

category: 'running'
method: FileTest
tearDown

	'asd.txt' asFileReference ensureDelete.
	super tearDown
%

category: 'tests'
method: FileTest
testCheckExistenceOfExistingFileThrowsException

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	self should: [file checkDoesNotExist] raise: FileAlreadyExistsException.
%

category: 'tests'
method: FileTest
testCheckExistenceOfNonExistingFileDoesNotThrowException

	| file |
	file := File named: 'asd.txt'.
	self shouldnt: [file checkDoesNotExist] raise: Error.
%

category: 'tests'
method: FileTest
testFileDelete

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	file delete.
	self deny: file exists.
%

category: 'tests'
method: FileTest
testFileExists

	| file |
	file := File named: 'asd.txt'.
	file writeStream close.
	self assert: file exists.
	file delete.
%

category: 'tests'
method: FileTest
testOpenFileForReadDoesNotDeleteExistingFile

	| size |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].

	size := (File named: 'asd.txt') size.
	self assert: size equals: 3.
%

category: 'tests'
method: FileTest
testOpeningFileNormallyDoesNotDeleteExistingFile

	| size |
	(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].

	size := (File named: 'asd.txt') size.
	self assert: size equals: 3.
%

category: 'tests'
method: FileTest
testOpeningFileObjectCreatesFile

	(File named: 'asd.txt') writeStream close.
	"Our File implementation should not cache anything.
	This test is based on this assumption."
	self assert: (File named: 'asd.txt') exists.
%

category: 'tests'
method: FileTest
testOpeningFileSetsPositionAtBeggining

	| file |
	[(File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].
	file := (File named: 'asd.txt') openForWrite.
	self assert: file position equals: 0 
		] ensure: [ file ifNotNil: [ file close ] ]
%

category: 'tests'
method: FileTest
testOpeningForAppendSetsPositionAtEnd

	| file |
	[ (File named: 'asd.txt') writeStreamDo: [ :stream | stream nextPutAll: 'aaa' ].
	file := (File named: 'asd.txt') openForAppend.
	self assert: file position equals: 3
		] ensure: [ file ifNotNil: [ file close ] ]
%

! Class implementation for 'FSGemStoneKernelTests'

!		Instance methods for 'FSGemStoneKernelTests'

category: 'private'
method: FSGemStoneKernelTests
collectionMoreThan1NoDuplicates

	^self collectionWithoutEqualElements
%

category: 'private'
method: FSGemStoneKernelTests
collectionWithoutEqualElements

	^ {1.1. 4.4. 6.5. 2.4. 3.1.}
%

category: 'private'
method: FSGemStoneKernelTests
collectionWithSameAtEndAndBegining

	^ {1.5. 5.5. 1.5 copy}.
%

category: 'private'
method: FSGemStoneKernelTests
elementNotInForIndexAccessing

	^ 9
%

category: 'private'
method: FSGemStoneKernelTests
empty

	^ #().
%

category: 'private'
method: FSGemStoneKernelTests
indexInForCollectionWithoutDuplicates
	^ 2.
%

category: 'private'
method: FSGemStoneKernelTests
indexInNonEmpty
	"Return an index between bounds of 'nonEmpty'"
	
	^ 2
%

category: 'private'
method: FSGemStoneKernelTests
moreThan3Elements

	^ #(1 2 3 4 5) copy.
%

category: 'private'
method: FSGemStoneKernelTests
nonEmpty

	^ self moreThan3Elements
%

category: 'tests'
method: FSGemStoneKernelTests
testAllButFirst

	| abf col |
	col := self moreThan3Elements.
	abf := col allButFirst.
	self deny: abf first = col first.
	self assert: abf size + 1 = col size
%

category: 'tests'
method: FSGemStoneKernelTests
testAllButFirstNElements

	| allButFirst collection |
	collection := self moreThan3Elements.
	allButFirst := collection allButFirst: 2.
	allButFirst withIndexDo: 
		 [:el :i | self assert: el equals: (collection at: i + 2) ].
	self assert: allButFirst size + 2 equals: collection size
%

category: 'tests'
method: FSGemStoneKernelTests
testCopyAfterLast

	| result index collection |
	collection := self collectionWithoutEqualElements .
	index:= self indexInForCollectionWithoutDuplicates .
	result := collection copyAfterLast: (collection  at:index ).
	
	"Verify content"
	result withIndexDo: 
		[:el :i | self assert: (collection at: (i + index )) equals: (result at: i)].

	"Verify size"
	self assert: result size equals: (collection size - index)
%

category: 'tests'
method: FSGemStoneKernelTests
testCopyUpToLast

	| result index collection |
	collection := self collectionWithoutEqualElements.
	index:= self indexInForCollectionWithoutDuplicates.
	result := collection copyUpToLast: (collection at:index).
	
	"Verify content"
	result withIndexDo: [:el :i| self assert: (collection at:i) equals: (result at: i)].
	
	"Verify size"
	self assert: result size equals: (index-1)
%

category: 'tests'
method: FSGemStoneKernelTests
testCopyWithFirst

	| index element result |
	index:= self indexInNonEmpty .
	element:= self nonEmpty at: index.
	
	result := self nonEmpty copyWithFirst: element.	
	
	self assert: result size = (self nonEmpty size + 1).
	self assert: result first = element .
	
	2 to: result size do:
	[ :i |
	self assert: (result at: i) = ( self nonEmpty at: ( i - 1 ))].
%

category: 'tests'
method: FSGemStoneKernelTests
testDifference
	"Answer the set theoretic difference of two collections."
	
	| difference |
	self assert: (self collectionWithoutEqualElements difference: self collectionWithoutEqualElements) isEmpty.
	self assert: (self empty difference: self collectionWithoutEqualElements) isEmpty.
	difference := (self collectionWithoutEqualElements difference: self empty).
	self assert: difference size = self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each |
		self assert: (difference includes: each) ].
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfIfAbsent

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection 
			lastIndexOf: element
			ifAbsent: [ 99 ]) equals: 1.
	self assert: (collection 
			lastIndexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 99 ]) equals: 99
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfIfAbsentDuplicate

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection first.

	"floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			ifAbsent: [ 55 ]) equals: collection size
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfStartingAt

	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection last.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 99 ]) equals: collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 99 ]) equals: 99.
	self assert: (collection 
			lastIndexOf: self elementNotInForIndexAccessing
			startingAt: collection size
			ifAbsent: [ 99 ]) equals: 99
%

category: 'tests'
method: FSGemStoneKernelTests
testLastIndexOfStartingAtDuplicate

	| collection element |
	collection := self collectionWithSameAtEndAndBegining.
	element := collection last.

	" floatCollectionWithSameAtEndAndBegining first and last elements are equals 
	'lastIndexOf:ifAbsent:startingAt: should return the position of the last occurrence :'"
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 55 ]) equals: collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 55 ]) equals: 1
%

! Class implementation for 'PathTest'

!		Instance methods for 'PathTest'

category: 'tests'
method: PathTest
testAbsolutePath

	| path |

	self assert: (AbsolutePath new isAbsolute).
	self assert: (Path root isAbsolute).
	
	path := AbsolutePath from: 'parent/child/grandChild' delimiter: $/.
	self assert: path size equals: 3.
	self assert: (path at: 1) equals: 'parent'.
	self assert: (path at: 2) equals: 'child'.
	self assert: (path at: 3) equals: 'grandChild'.
	
	path := AbsolutePath from: '/' delimiter: $/.
	self assert: path equals: Path root.
	
%

category: 'tests'
method: PathTest
testAbsolutePrintString
	
	| path actual |
	path := Path / 'plonk' / 'griffle'.
	actual := path printString.
	self assert: actual equals: 'Path / ''plonk'' / ''griffle'''
%

category: 'tests'
method: PathTest
testAbsoluteWithParents
	| path allPaths |
	path := Path / 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 4.
	self assert: allPaths first isRoot.
	self assert: allPaths second basename equals: 'plonk'.
	self assert: allPaths second size equals: 1.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'griffle'.
	self assert: allPaths third size equals: 2.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths fourth basename equals: 'nurb'.
	self assert: allPaths fourth size equals: 3.
	self assert: (allPaths fourth isChildOf: allPaths third).
	
	self assert: allPaths fourth equals: path.
	self assert: allPaths fourth == path
%

category: 'tests'
method: PathTest
testAsReference
	| path reference |
	path := Path * 'plonk'.
	reference := path asFileReference.
	self assert: reference class equals: FileReference.
	self assert: reference path equals: path
%

category: 'tests'
method: PathTest
testBasename
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basename equals: 'griffle'
%

category: 'tests'
method: PathTest
testBasenameNoParent
	| path |

	path := Path / 'griffle'.
	self assert: path parent basename equals: '/'.

	path := Path * 'griffle'.
	self assert: path parent basename equals: '.'.
%

category: 'tests'
method: PathTest
testBasenameWithoutExtension
	
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	path := Path * 'plonk' / 'griffle.taz'.
	self assert: path basenameWithoutExtension equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	path := Path * 'plonk' / 'griffle.taz.zork'.
	self assert: path basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (path basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (path basenameWithoutExtension: 'girffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (path basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.
%

category: 'tests'
method: PathTest
testCanonicalization

	| ref |

	ref := (Path * 'a/b/c') canonicalize.
	self assert: ref segments equals: #('a' 'b' 'c').

	ref := (Path / 'a/b/c') canonicalize.
	self assert: ref segments equals: #('a' 'b' 'c').

	ref := (Path * '../a/b/c') canonicalize.
	self assert: ref segments equals: #('..' 'a' 'b' 'c').

	ref := (Path * 'a/b/c/..') canonicalize.
	self assert: ref segments equals: #('a' 'b').

	ref := (Path / 'a/b/c/..') canonicalize.
	self assert: ref segments equals: #('a' 'b').

	ref := (Path * 'a/b/../c') canonicalize.
	self assert: ref segments equals: #('a' 'c').

	ref := (Path / 'a/b/../c') canonicalize.
	self assert: ref segments equals: #('a' 'c').
%

category: 'tests'
method: PathTest
testCommaAddsExtension
	| path result |
	path := Path * 'plonk' .
	result := path, 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: PathTest
testCommaAddsExtensionAgain
	| path result |
	path := Path * 'plonk.griffle'.
	result := path, 'nurp'.
	self assert: result basename equals: 'plonk.griffle.nurp'
%

category: 'tests'
method: PathTest
testContains
	| ancestor descendent |
	ancestor := Path / 'plonk'.
	descendent := Path / 'plonk' / 'griffle' / 'bork'.
	self assert: (ancestor contains: descendent).
	self deny: (descendent contains: ancestor)
%

category: 'tests'
method: PathTest
testContainsLocator
	| ancestor descendent |
	ancestor := FileLocator imageDirectory resolve path.
	descendent := FileLocator image / 'griffle'.
	self deny: (ancestor contains: descendent).
	self deny: (descendent contains: ancestor)
%

category: 'tests'
method: PathTest
testEqual
	| a b |
	a := Path * 'plonk'.
	b := Path * 'plonk'.
	self deny: a == b.
	self assert: a equals: b.
%

category: 'tests'
method: PathTest
testExtendingPath

	| ref |

	self should: [ '/a/b' asPath / '' ] raise: Error.
	self should: [ '/a/b' asPath / nil ] raise: Error.

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').

	ref := '/a/b/c' asPath / 'd/e'.
	self assert: ref parent segments equals: #('a' 'b' 'c' 'd').

	ref := '/a/b/c' asPath / '../d'.
	self assert: ref segments equals:  #('a' 'b' 'c' '..' 'd').

	ref := '/a/b/c' asPath / 'd/..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..').

	ref := '/a/b/c' asPath / 'd/../e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' '..' 'e').

	ref := '/a/b/c' asPath / './d'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/.'.
	self assert: (ref segments = #('a' 'b' 'c' 'd')).

	ref := '/a/b/c' asPath / 'd/./e'.
	self assert: ref segments equals: #('a' 'b' 'c' 'd' 'e').

%

category: 'tests'
method: PathTest
testExtensions
	self 
		assertCollection: (Path from: 'foo') extensions asArray
		equals: #().
	self
		assertCollection: (Path from: 'foo.tar') extensions asArray
		equals: #( 'tar' ).
	self
		assertCollection: (Path from: 'foo.tar.gz') extensions asArray
		equals: #( 'tar' 'gz').
	self
		assertCollection: (Path from: 'foo.1.tar.gz') extensions asArray
		equals: #( '1' 'tar' 'gz').
%

category: 'tests'
method: PathTest
testFullName

	| path |

	path := (FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
				relativeToReference: FileSystem workingDirectory.
	self assert: path fullName equals: 'book-result/W01-Welcome'
%

category: 'tests'
method: PathTest
testGrandchildOfPath
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := griffle / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: PathTest
testIsAbsolute
	self assert: (Path / 'plonk') isAbsolute
%

category: 'tests'
method: PathTest
testIsAbsoluteWindowsPathReturnsFalseWhenNoWindowsAbsolutePathProvided
  
	self deny: (Path isAbsoluteWindowsPath: 'tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp').
	self deny: (Path isAbsoluteWindowsPath: '/tmp/test').
%

category: 'tests'
method: PathTest
testIsAbsoluteWindowsPathReturnsTrueWhenWindowsAbsolutePathProvided
  
	self assert: (Path isAbsoluteWindowsPath: 'A:\').
	self assert: (Path isAbsoluteWindowsPath: 'c:\').
	self assert: (Path isAbsoluteWindowsPath: 'c:\test').
%

category: 'tests'
method: PathTest
testIsChildOfPath
	| parent child |
	parent := Path / 'griffle'.
	child := parent / 'nurb'.
	self assert: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
%

category: 'tests'
method: PathTest
testIsChildOfReference
	| parent child |
	parent := Path / 'griffle'.
	child := FileSystem memory referenceTo: parent / 'nurb'.
	self deny: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
%

category: 'tests'
method: PathTest
testIsEmpty
	self assert: (Path workingDirectory) isEmpty
%

category: 'tests'
method: PathTest
testIsNotAbsolute
	self deny: (Path * 'plonk') isAbsolute
%

category: 'tests'
method: PathTest
testIsNotRelative
	self deny: (Path / 'plonk') isRelative
%

category: 'tests'
method: PathTest
testIsNotRoot
	self deny: (Path / 'plonk') isRoot
%

category: 'tests'
method: PathTest
testIsRelative
	self assert: (Path * 'plonk') isRelative
%

category: 'tests'
method: PathTest
testIsRoot
	self assert: Path root isRoot
%

category: 'tests'
method: PathTest
testMakeRelative
	
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := parent makeRelative: child.
	self assert: relative equals: (Path parent / 'plonk' / 'nurp')
%

category: 'tests'
method: PathTest
testMakeRelativeFrom2RelativePaths
	"Related to issue: 14846 MakeRelative-method-applied-on-two-relative-paths-is-not-working"

	| parent child relative |
	parent := RelativePath new / 'griffle' / 'bibb'.
	child := RelativePath new / 'griffle' / 'plonk' / 'nurp'.
	relative := parent makeRelative: child.
	self assert: relative equals: Path parent / 'plonk' / 'nurp'
%

category: 'tests'
method: PathTest
testParent
	| path parent |
	path := (Path * 'plonk') / 'griffle'.
	parent := path parent.
	self assert: parent isRelative.
	self assert: (parent at: 1) equals: 'plonk'
%

category: 'tests'
method: PathTest
testParentParent
	| path  |
	path := (Path * '..') parent.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: '..'.
	self assert: (path at: 2) equals: '..'.
%

category: 'tests'
method: PathTest
testParentResolution
	| base relative absolute |
	base := Path / 'plonk' / 'pinto'.
	relative := Path parent / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
%

category: 'tests'
method: PathTest
testParentUpTo
	| a b c |
	a := Path / 'testParentUpTo' / 'A'.
	b := Path / 'testParentUpTo' / 'A' / 'B'.
	c := Path / 'testParentUpTo' / 'A' / 'B' / 'C'.
	self assert: b equals: (c parentUpTo: 'B').
	self assert: a equals: (c parentUpTo: 'A').
	self assert: Path / 'testParentUpTo' equals: (c parentUpTo: 'testParentUpTo').
	self assert: Path root equals: (c parentUpTo: 'notAParent')
%

category: 'tests'
method: PathTest
testParse

	| path |
	path := Path from: 'parent/child/grandChild' delimiter: $/.
	self assert: path size equals: 3.
	self assert: (path at: 1) equals: 'parent'.
	self assert: (path at: 2) equals: 'child'.
	self assert: (path at: 3) equals: 'grandChild'.
	
%

category: 'tests'
method: PathTest
testParseBogus
	
	| path |
	path := Path from: 'parent?<>~ \child/grandChild' delimiter: $/.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: 'parent?<>~ \child'.
	self assert: (path at: 2) equals: 'grandChild'.
	
%

category: 'tests'
method: PathTest
testParseTrailingSlash
	| path |
	path := Path from: 'griffle/' delimiter: $/.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: 'griffle'
%

category: 'tests'
method: PathTest
testParseWindowsPathWithUnixDelimiters
	
	| path |
	path := WindowsStore new pathFromString: 'C:\a/b/c'.
	self assert: path segments size equals: 4.
	self
		assertCollection: path segments
		equals: #('C:' 'a' 'b' 'c')
%

category: 'tests'
method: PathTest
testPathString

	| path |

	path := (FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
				relativeToReference: FileSystem workingDirectory.
	self assert: path isRelative.
	self assert: path pathString equals: 'book-result/W01-Welcome'
%

category: 'tests'
method: PathTest
testPrintPathOn

	| pathString pathSrc path |

	"Test a Relative path"
	pathSrc := 'one/two/three'.
	path := Path from: pathSrc.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	pathSrc := '/one/two/three'.
	path := Path from: pathSrc.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString
%

category: 'tests'
method: PathTest
testPrintPathOnDelimiter

	| pathString pathSrc path |

	"Test a Relative path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := 'one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := '|one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString
%

category: 'tests'
method: PathTest
testPrintRelativeWithParent
	| path |
	path := Path parent / 'foo'.
	self assert: (path printWithDelimiter: $/) equals: '../foo'
%

category: 'tests'
method: PathTest
testPrintWithDelimiter
	| path |
	path := (Path * 'plonk') / 'griffle'.
	self assert: (path printWithDelimiter: $%) equals: 'plonk%griffle'
%

category: 'tests'
method: PathTest
testRedundantSeparators

	| ref |

	ref := '/a/b/c' asPath / 'foo/'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo').

	ref := '/a/b/c' asPath / 'foo//..'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..').

	ref := '/a/b/c' asPath / '..//foo'.
	self assert: ref segments equals: #('a' 'b' 'c' '..' 'foo').

	ref := '/a/b/c' asPath / 'foo//..//bar'.
	self assert: ref segments equals: #('a' 'b' 'c' 'foo' '..' 'bar')
%

category: 'tests'
method: PathTest
testRelativeFromString

	| path |
	
	path := Path from: 'plonk/griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: 'plonk'.
	self assert: (path at: 2) equals: 'griffle'.
%

category: 'tests'
method: PathTest
testRelativeFromStringNormalization

	| path |
	
	path := Path from: 'plonk/../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 3.
	self assert: path segments equals: #('plonk' '..' 'griffle').
%

category: 'tests'
method: PathTest
testRelativeFromStringNormalizationParent

	| path |
	
	path := Path from: 'plonk/../../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 4.
	self assert: path segments equals: #('plonk' '..' '..' 'griffle')
%

category: 'tests'
method: PathTest
testRelativeFromStringParent

	| path |
	
	path := Path from: '../..'.
	
	self assert: path isRelative.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: '..'.
	self assert: (path at: 2) equals: '..'.
%

category: 'tests'
method: PathTest
testRelativePrintString
	| path actual |
	path := Path * 'plonk' / 'griffle'.
	actual := path printString.
	self assert: actual equals: 'Path * ''plonk'' / ''griffle'''
%

category: 'tests'
method: PathTest
testRelativeTo
	"aPath relativeTo: aParent returns a new path relative to the parent"
	
	| parent child relative |
	parent := Path / 'griffle'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative equals: (Path * 'plonk' / 'nurp')
%

category: 'tests'
method: PathTest
testRelativeToBranch
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative  equals: (Path parent / 'plonk' / 'nurp')
%

category: 'tests'
method: PathTest
testRelativeWithParents
	| path allPaths |
	path := Path * 'plonk' / 'griffle' / 'nurb'.
	allPaths := path withParents.
	
	self assert: allPaths size equals: 3.
	self assert: allPaths first basename equals: 'plonk'.
	self assert: allPaths first size equals: 1.
	self assert: allPaths second basename equals: 'griffle'.
	self assert: allPaths second size equals: 2.
	self assert: (allPaths second isChildOf: allPaths first).
	self assert: allPaths third basename equals: 'nurb'.
	self assert: allPaths third size equals: 3.
	self assert: (allPaths third isChildOf: allPaths second).
	self assert: allPaths third == path
%

category: 'tests'
method: PathTest
testResolveAbsolute
	| path |
	path := Path / 'griffle'.
	self assert: path resolve == path.
	self assert: path isAbsolute
%

category: 'tests'
method: PathTest
testResolvePath
		
	| path |
	path := Path / 'grandfather' / 'father' / 'child'.
	self 
		assert: (path resolvePath: Path / 'grandfather') 
		equals: (Path / 'grandfather').
	self 
		assert: (path resolvePath: Path / 'child')
		equals: (Path / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert: (path resolvePath: Path * 'grandfather')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'grandfather').
	self
		assert: (path resolvePath: Path * 'child')
		equals: (Path / 'grandfather' / 'father' / 'child' / 'child').
	self
		assert:  (path resolvePath: (Path parent) / '..') canonicalize
		equals: (Path / 'grandfather')
%

category: 'tests'
method: PathTest
testResolveRelative
	| path |
	path := Path * 'griffle'.
	self assert: path resolve == path.
	self assert: path isRelative
%

category: 'tests'
method: PathTest
testResolveString	

	| path result |
	path := Path * 'plonk'.
	result := path resolve: 'griffle'.
	self assert: result class equals: path class.
	self assert: result size equals: 2.
	self assert: (result at: 1) equals: 'plonk'.
	self assert: (result at: 2) equals: 'griffle'.
%

category: 'tests'
method: PathTest
testRootParent
	| root |
	root := Path root.
	self assert: root parent == root
%

category: 'tests'
method: PathTest
testRootPrintString
	| path actual |
	path := Path root.
	actual := path printString.
	self assert: actual equals: 'Path root'
%

category: 'tests'
method: PathTest
testSiblingOfPath
	| griffle  nurb |
	griffle := Path / 'griffle'.
	nurb := Path / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
%

category: 'tests'
method: PathTest
testSimpleResolution
	| base relative absolute |
	base := Path / 'plonk'.
	relative := (Path * 'griffle') / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute at: 1) equals: 'plonk'.
	self assert: (absolute at: 2) equals: 'griffle'.
	self assert: (absolute at: 3) equals: 'zonk'.
%

category: 'tests'
method: PathTest
testSlash
	| path actual |
	path := Path * 'plonk'.
	actual := path / 'griffle'.
	self assert: actual class equals: path class.
	self assert: (actual printWithDelimiter: $/) equals: 'plonk/griffle'
%

category: 'tests'
method: PathTest
testUnequalContent
	| a b |
	a := Path * 'plonk'.
	b := Path * 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: PathTest
testUnequalSize
	| a b |
	a := Path * 'plonk'.
	b := (Path * 'plonk') / 'griffle'.
	self deny: a = b.
%

category: 'tests'
method: PathTest
testUnixAbsolutePathName

	self assert: (Path from: '/test') isAbsolute.
	self assert: (Path from: '/etc/bin') isAbsolute.
%

category: 'tests'
method: PathTest
testWindowsAbsolutePathName
  
	self assert: (Path from: 'A:\') isAbsolute.
	self assert: (Path from: 'c:\') isAbsolute.
	self assert: (Path from: 'c:\test') isAbsolute.
%

category: 'tests'
method: PathTest
testWithExtentionAddsExtension
	| path result |
	path := Path * 'plonk'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: PathTest
testWithExtentionReplacesExtension
	| path result |
	path := Path * 'plonk.griffle'.
	result := path withExtension: 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
%

category: 'tests'
method: PathTest
testWorkingDirectoryParent
	| path |
	path := Path workingDirectory parent.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: '..'
%

category: 'tests'
method: PathTest
testWorkingDirPrintString
	| path actual |
	path := Path workingDirectory. 
	actual := path printString.
	self assert: actual equals: 'Path workingDirectory'
%

! Class implementation for 'RwGemStoneVersionNumberTestCase'

!		Class methods for 'RwGemStoneVersionNumberTestCase'

category: 'Tests'
classmethod: RwGemStoneVersionNumberTestCase
shouldInheritSelectors

	^true
%

!		Instance methods for 'RwGemStoneVersionNumberTestCase'

category: 'test alpha/numeric version numbers'
method: RwGemStoneVersionNumberTestCase
testAlphaNumericVersion1

	"Use numeric comparison for pure numbers. If you non-numeric version separate with '-'"
	
	| x y |
	self assert: ((x := self versionClass fromString: '2.9.0') < (y := self versionClass fromString: '2.10.0')).
%

category: 'test approximately greater than'
method: RwGemStoneVersionNumberTestCase
testApproxVersion01

	self assert: '1.1.1' asRwGemStoneVersionNumber ~> '1.1' asRwGemStoneVersionNumber
%

category: 'test approximately greater than'
method: RwGemStoneVersionNumberTestCase
testApproxVersion02

	self deny: '1.1' asRwGemStoneVersionNumber ~> '1' asRwGemStoneVersionNumber
%

category: 'test approximately greater than'
method: RwGemStoneVersionNumberTestCase
testApproxVersion03

	self assert: '1.1' asRwGemStoneVersionNumber ~> '1.1' asRwGemStoneVersionNumber
%

category: 'test approximately greater than'
method: RwGemStoneVersionNumberTestCase
testApproxVersion04

	self assert: '3.2.0' asRwGemStoneVersionNumber approximateBase asString = '4'.
	self assert: '3.2' asRwGemStoneVersionNumber approximateBase asString = '4'.
%

category: 'test approximately greater than'
method: RwGemStoneVersionNumberTestCase
testApproxVersion05

	self assert: '3.2.16' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.
	self deny: '3.2.14' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.
	self deny: '3.3' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.
	self deny: '3.3.0' asRwGemStoneVersionNumber ~> '3.2.15' asRwGemStoneVersionNumber.

	self assert: '3.2.15.5' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.2.15.0' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.2.16' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.3' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.
	self deny: '3.3.0' asRwGemStoneVersionNumber ~> '3.2.15.1' asRwGemStoneVersionNumber.

	self assert: '3.3' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.
	self assert: '3.3.0' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.
	self assert: '3.3.1' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.
	self assert: '3.4' asRwGemStoneVersionNumber ~> '3.2' asRwGemStoneVersionNumber.

	self assert: '3.3.1' asRwGemStoneVersionNumber ~> '3.2.0' asRwGemStoneVersionNumber.
	self assert: '3.3.0' asRwGemStoneVersionNumber ~> '3.2.0' asRwGemStoneVersionNumber.
	self assert: '3.3' asRwGemStoneVersionNumber ~> '3.2.0' asRwGemStoneVersionNumber.
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testCollapseZeros

	self assert: (RwGemStoneVersionNumber fromString: '1.0') collapseZeros printString = '1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0') collapseZeros printString = '1'.
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0') collapseZeros printString = '1'.
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion01

	self assert: ((self versionClass fromString: '1.1.1') versionString = '1.1.1')
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion02

	| v1 v2 |
	v1 := self versionClass fromString: '1.1.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 = v1).	
	self assert: (v2 = v2).
	self assert: (v1 > v2)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion03

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion04

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion05

	| v1 v2 |
	v1 := self versionClass fromString: '3'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion06

	| v1 v2 |
	v1 := self versionClass fromString: '3.'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion07

	| v1 v2 |
	v1 := self versionClass fromString: '3.0.0'.
	v2 := self versionClass fromString: '2'.
	self assert: (v1 > v2)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion09

	| v1 v2 |
	v1 := self versionClass fromString: '1.0'.
	v2 := self versionClass fromString: '0.7'.
	self assert: (v1 >= v2).
	self assert: (v2 <= v1)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion10

	| x y |
	self assert: ((x := (({
		self versionClass fromString: '1.0'.
		self versionClass fromString: '0.7'.
		self versionClass fromString: '0.8'.
		self versionClass fromString: '0.9'.
		self versionClass fromString: '1.0.1'
	} sort: [:a :b | a <= b ]) collect: [:each | each versionString ]) asArray) = (y := #( '0.7' '0.8' '0.9' '1.0' '1.0.1')))
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion11

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1b'.
	v2 := self versionClass fromString: '1.0.1a'.
	self assert: (v1 >= v2).
	self assert: (v2 <= v1)
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion12

	self deny: ((self versionClass fromString: '1.0') <= (self versionClass fromString: '0.7'))
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion13

	self deny: ((self versionClass fromString: '0.8') <= (self versionClass fromString: '0.7')).
	self deny: ((self versionClass fromString: '0.8.1.8') <= (self versionClass fromString: '0.7.0.5')).
	
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion17

	self assert: ((self versionClass fromString: '1.0') = (self versionClass fromString: '1.0.0')).
	self assert: ((self versionClass fromString: '1') = (self versionClass fromString: '1.0')).
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion18

	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1')).
	self deny: ((self versionClass fromString: '1.0') < (self versionClass fromString: '1-0')).
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion20

	self assert: (RwGemStoneVersionNumber fromString: '') printString = ''.
%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion21

	self deny: (RwGemStoneVersionNumber fromString: '') > (RwGemStoneVersionNumber fromString: '0').
	self assert: (RwGemStoneVersionNumber fromString: '') < (RwGemStoneVersionNumber fromString: '0').
	self assert: (RwGemStoneVersionNumber fromString: '') = (RwGemStoneVersionNumber fromString: '').

%

category: 'tests'
method: RwGemStoneVersionNumberTestCase
testVersion24

	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1.0.0') = (RwGemStoneVersionNumber fromString: '1.0.0.1').
	self assert: (RwGemStoneVersionNumber fromString: '1.0.0.1') ~= (RwGemStoneVersionNumber fromString: '1..1').
%

category: 'private'
method: RwGemStoneVersionNumberTestCase
versionClass

	^RwGemStoneVersionNumber
%

! Class implementation for 'RwLoadSpecificationV2Test'

!		Instance methods for 'RwLoadSpecificationV2Test'

category: 'tests'
method: RwLoadSpecificationV2Test
testBasic_1
	"excercise basic functionality"

	| projectName loadSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		componentNames: #('Default');
		groupNames: #('core');
		projectSpecFile: 'rowan/xxx.ston';
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	self assert: loadSpecification  _validate
%

category: 'tests'
method: RwLoadSpecificationV2Test
testBasic_2
	"excercise basic functionality"

	| projectName loadSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		componentNames: #('Default');
		groupNames: #('core');
		projectSpecFile: 'rowan/xxx.ston';
		revision: 'master';
		gitUrl: 'file://x/y/z';
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	self assert: loadSpecification  _validate
%

category: 'tests'
method: RwLoadSpecificationV2Test
testBasic_3
	"excercise basic functionality"

	| projectName loadSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	self assert: loadSpecification  _validate
%

category: 'tests'
method: RwLoadSpecificationV2Test
testComparison_1
	| projectName loadSpecification specName projectSpecCopy stonString stonStringCopy x |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	self
		assert:
			(x := RwLoadSpecificationV2 allInstVarNames)
				=
					#(#'specName' #'projectName' #'projectAlias' #'gitUrl' #'diskUrl' #'mercurialUrl' #'svnUrl' #'revision' #'projectSpecFile' #'componentNames' #'groupNames' #'customConditionalAttributes' #'platformProperties' #'comment' #'projectsHome' #'repositoryResolutionPolicy').	"If inst vars don't match, copy and hash methods have to change"

	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = loadSpecification.
	self assert: projectSpecCopy hash = loadSpecification hash
%

category: 'tests'
method: RwLoadSpecificationV2Test
testComparison_2
	"compare equal even if lazy initialization has taken place"

	| projectName loadSpecification specName projectSpecCopy stonString stonStringCopy stonStringLazy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	projectSpecCopy platformProperties.	"trigger the selectors that cause lazy initialization"

	stonStringLazy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = loadSpecification.
	self assert: projectSpecCopy hash = loadSpecification hash.
	self deny: stonStringLazy = stonStringCopy
%

category: 'tests'
method: RwLoadSpecificationV2Test
testInvalidPropertyValue
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 'boom';
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Value of property (#''defaultMethodEnv''->''boom'') is expected to be class ''SmallInteger'' not class ''String'''.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testInvalidPropertyValueForUser
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 'boom';
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Value of property (#''defaultMethodEnv''->''boom'') is expected to be class ''SmallInteger'' not class ''String'''.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testInvalidRevision
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		revision: 'boom';
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Invalid revision ''boom''. Should be nil for disk-based repository'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testIssue_530_1
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"original has no predefined platformSpec dictionary"

	| projectName loadSpecification stonStrings specName projectSpecCopy stonString stonStringCopy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.
	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = loadSpecification.

	projectSpecCopy
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	self deny: projectSpecCopy = loadSpecification.

	stonStrings := {
	(STON toStringPretty: loadSpecification).
	(STON toStringPretty: projectSpecCopy)}	"useful in case of test failure"
%

category: 'tests'
method: RwLoadSpecificationV2Test
testIssue_530_2
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"predefine platformSpec dictionary in original"

	| projectName loadSpecification stonStrings specName projectSpecCopy stonString |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultSymbolDictNameForUser: 'Bozo'
			to: self _sampleSymbolDictionaryName2;
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	self assert: projectSpecCopy = loadSpecification.

	projectSpecCopy
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultMethodEnvForUser: 'PharoGs' to: 2;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultSymbolDictNameForUser: 'DataCurator'
			to: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: false;
		gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: 'DataCurator'
			to: true;
		yourself.

	self deny: projectSpecCopy = loadSpecification.

	stonStrings := {stonString.	"original loadSpec"
	(STON toStringPretty: loadSpecification).	"origiinal after copy modified"
	(STON toStringPretty: projectSpecCopy)	"copy"}.
	self assert: stonString = (stonStrings at: 1).	"duh"
	self assert: stonString = (stonStrings at: 2).	"point of test"
	self deny: stonString = (stonStrings at: 3)	"duh"
%

category: 'tests'
method: RwLoadSpecificationV2Test
testIssue_530_3
	"https://github.com/GemTalk/Rowan/issues/530"

	"state changes to a copy of a loadSpec should not affect oriinal load spec"

	"componentNames and groupNames need to be isolated"

	| projectName loadSpecification stonStrings specName projectSpecCopy stonString |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		componentNames: #('Default');
		groupNames: #('core');
		projectSpecFile: 'rowan/xxx.ston';
		revision: 'master';
		gitUrl: 'file://x/y/z';
		yourself.
	stonString := STON toStringPretty: loadSpecification.

	projectSpecCopy := loadSpecification copy.

	self assert: projectSpecCopy = loadSpecification.

	projectSpecCopy componentNames add: 'Boom'.
	projectSpecCopy groupNames add: 'boom'.

	self deny: projectSpecCopy = loadSpecification.

	stonStrings := {stonString.	"original loadSpec"
	(STON toStringPretty: loadSpecification).	"origiinal after copy modified"
	(STON toStringPretty: projectSpecCopy)	"copy"}.
	self assert: stonString = (stonStrings at: 1).	"duh"
	self assert: stonString = (stonStrings at: 2).	"point of test"
	self deny: stonString = (stonStrings at: 3)	"duh"
%

category: 'tests'
method: RwLoadSpecificationV2Test
testMissingRevision
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: The instance variable ''revision'' must be set for the''gitUrl'''.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testNilInstanceVariable
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		projectSpecFile: nil;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The instance variable ''projectSpecFile'' cannot be nil'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testOnlyOneRepositoryUrl
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		diskUrl: 'ftp://$ROWAN_PROJECTS_HOME/' , projectName;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Only one of (gitUrl diskUrl mercurialUrl svnUrl) must be be set'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testRevisionMustBeSet
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		gitUrl: 'https://github.com/user/' , projectName;
		yourself.

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The instance variable ''revision'' must be set for the''gitUrl'''.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testUknownPlatform
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.

	hitError := false.
	loadSpecification platformProperties at: 'boom' put: Dictionary new.
	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Unknown platform name ''boom'' in platform properties'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwLoadSpecificationV2Test
testUnknownPropertyKey
	"error coverage for invalid load specs"

	| projectName loadSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	loadSpecification := RwLoadSpecificationV2 new
		projectName: projectName;
		specName: specName;
		gemstoneSetDefaultMethodEnvTo: 0;
		gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
		gemstoneSetDefaultUseSessionMethodsForExtensionsTo: true;
		yourself.
	(loadSpecification platformProperties at: 'gemstone')
		at: 'BOOM'
		put:
			(Dictionary new
				at: #'boom' put: nil;
				yourself).

	stonString := STON toStringPretty: loadSpecification.	"useful in case of error"
	hitError := false.
	[ loadSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description) = 'Error: Unknown platform property key #''boom'''.
			hitError := true ].
	self assert: hitError
%

category: 'private'
method: RwLoadSpecificationV2Test
_sampleSymbolDictionaryName1

	^ #'RowanSample9_1'
%

category: 'private'
method: RwLoadSpecificationV2Test
_sampleSymbolDictionaryName2

	^ #'RowanSample9_2'
%

! Class implementation for 'RwProjectComponentVisitorV2Test'

!		Instance methods for 'RwProjectComponentVisitorV2Test'

category: 'tests'
method: RwProjectComponentVisitorV2Test
testBasicVisit_independent
	"test of RwProjectLoadComponentVisitorV2 as it would be used without a RwResolvedProject."

	| platformConditionalAttributes groupNames visitor componentNamesToLoad projectAlias projectPath projectSpecUrl projectSpec |
	platformConditionalAttributes := {'common'.
	'gemstone'.
	('3.5.0' asRwGemStoneVersionNumber)}.
	projectAlias := 'RowanSample9_DiskConfig_Test'.
	componentNamesToLoad := #('Core').
	groupNames := #('core').

	projectPath := self _cloneRowanSample9: projectAlias.	"clone https://github.com/dalehenrich/RowanSample9"

	visitor := RwIndependentComponentVisitorV2 new
		platformConditionalAttributes: platformConditionalAttributes;
		groupNames: groupNames;
		yourself.

	self assert: visitor packageNames isEmpty.
	projectSpecUrl := 'file:' , projectPath , '/rowan/project.ston'.
	projectSpec := RwSpecification fromUrl: projectSpecUrl.

	componentNamesToLoad
		do: [ :componentName | 
			| component url |
			url := 'file:' , projectPath , '/' , projectSpec componentsPath , '/'
				, componentName , '.ston'.
			component := RwAbstractProjectLoadComponentV2 fromUrl: url.
			component projectName: projectAlias.

			visitor visit: component ].
	self
		assert: visitor packageNames sort
		equals:
			#('RowanSample9-Core' 'RowanSample9-Extensions' 'RowanSample9-GemStone') sort.
	self assert: visitor projectLoadSpecs isEmpty
%

category: 'tests'
method: RwProjectComponentVisitorV2Test
testVisitVastTonelDemo_555_independent
	"test of RwProjectLoadComponentVisitorV2 as it would be used without a RwResolvedProject."

	"https://github.com/GemTalk/Rowan/issues/555"

	| visitor projectAlias projectPath componentNamesToLoad groupNames |

	projectAlias := 'tonel-demo_DiskConfig_Test'.
	componentNamesToLoad := #('Core').
	groupNames := #('core').

	projectPath := self _cloneVastTonelDemo_555: projectAlias deleteClone: true.

"vast"
	visitor := self
		_visitVastTonelDemo_555:
			{'common'.
			'vast'}
		projectAlias: projectAlias
		projectPath: projectPath.
	self
		assert: visitor packageNames sort
		equals:
			#('TonelExampleAnotherSubSubApp' 'TonelAnotherShadowSubSubApp' 'TonelExampleApp' 'TonelExampleShadowSubSubApp' 'TonelExampleSubApp' 'TonelExampleShadowSubSubSubApp' 'TonelExampleSubSubApp' 'TonelExampleForVastPharoApp')
				sort.

"pharo"
	visitor := self
		_visitVastTonelDemo_555:
			{'common'.
			'pharo'}
		projectAlias: projectAlias
		projectPath: projectPath.
	self
		assert: visitor packageNames sort
		equals: #('TonelExampleApp' 'TonelExampleForPharoApp' 'TonelExampleForVastPharoApp') sort.

"gemstone"
	visitor := self
		_visitVastTonelDemo_555:
			{'common'.
			'gemstone'}
		projectAlias: projectAlias
		projectPath: projectPath.
	self assert: visitor packageNames sort equals: #('TonelExampleApp' 'TonelExampleForGemStoneApp') sort
%

category: 'private'
method: RwProjectComponentVisitorV2Test
_visitVastTonelDemo_555: platformConditionalAttributes projectAlias: projectAlias projectPath: projectPath
	| groupNames visitor componentNamesToLoad projectSpecUrl projectSpec |
	componentNamesToLoad := #('Core').
	groupNames := #('core').

	visitor := RwIndependentComponentVisitorV2 new
		platformConditionalAttributes: platformConditionalAttributes;
		groupNames: groupNames;
		yourself.

	self assert: visitor packageNames isEmpty.
	projectSpecUrl := 'file:' , projectPath , '/rowan/project.ston'.
	projectSpec := RwSpecification fromUrl: projectSpecUrl.

	componentNamesToLoad
		do: [ :componentName | 
			| component url |
			url := 'file:' , projectPath , '/' , projectSpec componentsPath , '/'
				, componentName , '.ston'.
			component := RwAbstractProjectLoadComponentV2 fromUrl: url.
			component projectName: projectAlias.

			visitor visit: component ].
	^ visitor
%

! Class implementation for 'RwProjectLoadComponentV2Test'

!		Instance methods for 'RwProjectLoadComponentV2Test'

category: 'tests'
method: RwProjectLoadComponentV2Test
testBasic
	"excercise basic functionality for RwProjectLoadComponentV2"

	self
		_testComponentClass: RwProjectLoadComponentV2
		projectName: 'RowanSample9'
		componentName: 'Core'
		projectRef: 'Project'
		componentRef: 'Nested'
		projectRef: 'Project1'
%

category: 'tests'
method: RwProjectLoadComponentV2Test
testBasic_nested
	"excercise basic functionality for RwNestedProjectLoadComponentV2"

	self
		_testComponentClass: RwNestedProjectLoadComponentV2
		projectName: 'RowanSample9'
		componentName: 'Core'
		projectRef: 'Project'
		componentRef: 'Nested'
		projectRef: 'Project1'
%

category: 'tests'
method: RwProjectLoadComponentV2Test
testInvalidConditionalGroupName
	"error coverage for invalid components"

	| componentName projectName component packageName stonString conditionalPackages hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.

	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	conditionalPackages := component conditionalProperties.
	(conditionalPackages at: #('common')) at: 'boom' put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Conditional packages includes group name ''boom'' that is not a defined group'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwProjectLoadComponentV2Test
testInvalidGroupName
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component defineGroupNamed: 'core' toIncludeGroups: #('boom').

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The group ''boom'' is not a defined group'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwProjectLoadComponentV2Test
testUknownPlatform
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	component conditionalPackageMapSpecs at: 'boom' put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Unknown platform name ''boom'' in conditional package map specs'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwProjectLoadComponentV2Test
testUknownPlatformPropertiesKey
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	((component conditionalPackageMapSpecs at: 'gemstone') at: 'SystemUser')
		at: #'boom'
		put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Unknown platformPropertiesMap key #''boom'''.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwProjectLoadComponentV2Test
testUndefinedPackageName
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	(((component conditionalPackageMapSpecs at: 'gemstone') at: 'SystemUser')
		at: #'packageNameToPlatformPropertiesMap') at: 'boom' put: Dictionary new.

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						=
							'Error: Undefined package name ''boom'' used in plaform properties map'.
			hitError := true ].
	self assert: hitError
%

category: 'tests'
method: RwProjectLoadComponentV2Test
testUnknownPackagePropertName
	"error coverage for invalid components"

	| componentName projectName component packageName stonString hitError |
	projectName := 'RowanSample9'.
	componentName := 'Core'.
	component := RwProjectLoadComponentV2 newNamed: componentName for: projectName.

	self assert: component validate.

	packageName := projectName , '-Core'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName
			setSymbolDictNameTo: 'UserGlobals'.
	(((component conditionalPackageMapSpecs at: 'gemstone') at: 'SystemUser')
		at: #'packageNameToPlatformPropertiesMap')
		at: packageName
		put:
			(Dictionary new
				at: #'boom' put: 'boom';
				yourself).

	stonString := STON toStringPretty: component.	"useful in case of error"
	hitError := false.
	[ component validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: Unknown package property name #''boom'''.
			hitError := true ].
	self assert: hitError
%

category: 'private'
method: RwProjectLoadComponentV2Test
_testComponentClass: componentClass projectName: projectName componentName: componentName projectRef: projectRef componentRef: componentRef projectRef: projectRef1
	"excercise basic functionality"

	| component packageName1 packageName2 stonString conditionalProperties |
	component := componentClass newNamed: componentName for: projectName.

	self assert: component validate.

	packageName1 := projectName , '-Core'.
	packageName2 := projectName , '-Extension'.
	component
		defineGroupNamed: 'core';
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addPackageNames: {packageName1};
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addComponentNames: {componentRef};
		conditionalPropertiesAtConditions: {'common'}
			andGroup: 'core'
			addProjectNames: {projectRef};
		conditionalPropertiesAtConditions: {'gemstone'}
			andGroup: 'core'
			addComponentNames: {componentRef};
		conditionalPropertiesAtConditions: {'gemstone'}
			andGroup: 'core'
			addPackageNames: {packageName2};
		conditionalPropertiesAtConditions: {'vast'}
			andGroup: 'core'
			addProjectNames: {projectRef1};
		conditionalPropertiesAtConditions: {'vast'}
			andGroup: 'core'
			addPackageNames: {packageName2};
		conditionalPackageMapSpecsAtGemStoneUserId: 'SystemUser'
			andPackageName: packageName1
			setSymbolDictNameTo: 'UserGlobals'.


	stonString := STON toStringPretty: component.	"useful in case of error"
	self assert: component validate.
	self assert: component definedGroupNames keys asArray sort = {'core'}.
	conditionalProperties := component copy initializeForExport
		conditionalProperties.	"sorted in canonical key order"
	self
		assert:
			conditionalProperties keys
				=
					{{'common'}.
					{'gemstone'}.
					{'vast'}}.

	self
		_validateComponent: component
			condition: {'common'}
			group: 'core'
			componentNames: {componentRef}
			packageNames: {packageName1}
			projectNames: {projectRef};
		_validateComponent: component
			condition: {'gemstone'}
			group: 'core'
			componentNames: {componentRef}
			packageNames: {packageName2}
			projectNames: {};
		_validateComponent: component
			condition: {'vast'}
			group: 'core'
			componentNames: {}
			packageNames: {packageName2}
			projectNames: {projectRef1};
		yourself.

	component
		removePackageNamed: packageName1;
		removeProjectNamed: projectRef1;
		removeComponentNamed: componentRef;
		yourself.

	stonString := STON toStringPretty: component.	"useful in case of error"
	self assert: component validate.

	self
		_validateComponent: component
			condition: {'common'}
			group: 'core'
			componentNames: {}
			packageNames: {}
			projectNames: {projectRef};
		_validateComponent: component
			condition: {'gemstone'}
			group: 'core'
			componentNames: {}
			packageNames: {packageName2}
			projectNames: {};
		_validateComponent: component
			condition: {'vast'}
			group: 'core'
			componentNames: {}
			packageNames: {packageName2}
			projectNames: {};
		yourself
%

category: 'private'
method: RwProjectLoadComponentV2Test
_validateComponent: component condition: conditionArray group: groupName componentNames: componentNames packageNames: packageNames projectNames: projectNames
	| x |
	self
		assert:
			(x := component
				conditionalComponentsAtConditions: conditionArray
				andGroup: groupName) asArray = componentNames.
	self
		assert:
			(x := component
				conditionalPackagesAtConditions: conditionArray
				andGroup: groupName) asArray = packageNames.
	self
		assert:
			(x := component
				conditionalProjectsAtConditions: conditionArray
				andGroup: groupName) asArray = projectNames
%

! Class implementation for 'RwProjectSpecificationV2Test'

!		Instance methods for 'RwProjectSpecificationV2Test'

category: 'tests'
method: RwProjectSpecificationV2Test
testBasic
	"excercise basic functionality"

	| projectName projectSpecification stonString specName |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.

	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	self assert: projectSpecification  _validate
%

category: 'tests'
method: RwProjectSpecificationV2Test
testComparison_1
	| projectName projectSpecification specName projectSpecCopy stonString stonStringCopy x |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.

	self
		assert:
			(x := RwProjectSpecificationV2 allInstVarNames)
				=
					#(#'specName' #'projectName' #'projectSpecPath' #'componentsPath' #'packagesPath' #'projectsPath' #'specsPath' #'packageFormat' #'packageConvention' #'comment' #'repoType' #'loadedCommitId').	"If inst vars don't match, copy and hash methods have to change"

	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = projectSpecification.
	self assert: projectSpecCopy hash = projectSpecification hash
%

category: 'tests'
method: RwProjectSpecificationV2Test
testComparison_2
	"compare equal even if lazy initialization has taken place"

	| projectName projectSpecification specName projectSpecCopy stonString stonStringCopy stonStringLazy |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		specName: specName;
		yourself.
	stonString := STON toStringPretty: projectSpecification.

	projectSpecCopy := projectSpecification copy.

	stonStringCopy := STON toStringPretty: projectSpecCopy.

	projectSpecCopy repoType.	"trigger the selectors that cause lazy initialization"

	stonStringLazy := STON toStringPretty: projectSpecCopy.

	self assert: stonString = stonStringCopy.
	self assert: projectSpecCopy = projectSpecification.
	self assert: projectSpecCopy hash = projectSpecification hash.
	self deny: stonStringLazy = stonStringCopy
%

category: 'tests'
method: RwProjectSpecificationV2Test
testNilInstanceVariable
	"error coverage for invalid load specs"

	| projectName projectSpecification stonString specName hitError |
	projectName := 'RowanSample9'.
	specName := projectName , 'Core'.
	projectSpecification := RwProjectSpecificationV2 new
		projectName: projectName;
		projectsPath: nil;
		yourself.

	stonString := STON toStringPretty: projectSpecification.	"useful in case of error"
	hitError := false.
	[ projectSpecification _validate ]
		on: Error
		do: [ :ex | 
			| x |
			self
				assert:
					(x := ex description)
						= 'Error: The instance variable ''projectsPath'' cannot be nil'.
			hitError := true ].
	self assert: hitError
%

! Class implementation for 'RwSemanticVersionNumber200TestCase'

!		Instance methods for 'RwSemanticVersionNumber200TestCase'

category: 'tests'
method: RwSemanticVersionNumber200TestCase
testGitDescribe

	"https://github.com/GemTalk/Rowan/issues/381#issuecomment-450502212"

	"
`git describe --match v0.0.1` can produce a version number that looks like the following v0.0.1-1-g832d2b5 ... ensure that we can use this result to compare version numbers ... the leading `-` needs to be transformed to `+`, but other than that I think that the following comparisons are correct for what we are trying to do o
	"
	| s1 s2 s3 s4 v1 v2 v3 v4 |
	s1 := '0.0.1'.
	s2 := '0.0.1+1-g832d2b5'. "git describe output .. compares equal to 0.0.1, which is acceptable - I think"
	s3 := '0.0.1+2-g59a4bdf'.	"git describe output .. compares equal to 0.0.1, which is acceptable - I think"
	s4 := '0.0.2'.

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.
	v4 := s4 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.
	self assert: v4 printString = s4.

	self assert: v1 = v2.	"acceptable, I think"

	self assert: v1 = v3.	"acceptable, I think"
	self assert: v2 = v3.	"acceptable, I think"

	self assert: v1 < v4.
	self assert: v2 < v4.
	self assert: v3 < v4.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.
	self assert: v4 = v4.
%

category: 'tests'
method: RwSemanticVersionNumber200TestCase
testSpec_02

	"
A normal version number MUST take the form X.Y.Z where X, Y, and Z are
non-negative integers, and MUST NOT contain leading zeroes. X is the
major version, Y is the minor version, and Z is the patch version.
Each element MUST increase numerically. For instance: 1.9.0 -> 1.10.0 -> 1.11.0.
	"
	| s1 s2 s3 v1 v2 v3 |
	s1 := '1.9.0' .
	s2 := '1.10.0' .
	s3 := '1.11.0' .

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.

	self assert: v1 < v2.
	self assert: v2 < v3.
	self assert: v1 < v3.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.
%

category: 'tests'
method: RwSemanticVersionNumber200TestCase
testSpec_09

	"
A pre-release version MAY be denoted by appending a hyphen and a
series of dot separated identifiers immediately following the patch
version. Identifiers MUST comprise only ASCII alphanumerics and hyphen
[0-9A-Za-z-]. Identifiers MUST NOT be empty. Numeric identifiers MUST
NOT include leading zeroes. Pre-release versions have a lower
precedence than the associated normal version. A pre-release version
indicates that the version is unstable and might not satisfy the
intended compatibility requirements as denoted by its associated
normal version. Examples: 1.0.0-alpha, 1.0.0-alpha.1, 1.0.0-0.3.7,
1.0.0-x.7.z.92.
	"
	| s1 s2 s3 s4 s5 v1 v2 v3 v4 v5 |
	s1 := '1.0.0-alpha' .
	s2 := '1.0.0-alpha.1' .
	s3 := '1.0.0-0.3.7' .
	s4 := '1.0.0-x.7.z.92' .

	s5 := '1.0.0' .

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.
	v4 := s4 asRwSemanticVersionNumber.
	v5 := s5 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.
	self assert: v4 printString = s4.

	self assert: v1 < v5.
	self assert: v2 < v5.
	self assert: v3 < v5.
	self assert: v4 < v5.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.
	self assert: v4 = v4.
	self assert: v5 = v5.
%

category: 'tests'
method: RwSemanticVersionNumber200TestCase
testSpec_10

	"
Build metadata MAY be denoted by appending a plus sign and a series of dot
separated identifiers immediately following the patch or pre-release version.
Identifiers MUST comprise only ASCII alphanumerics and hyphen [0-9A-Za-z-].
Identifiers MUST NOT be empty. Build metadata MUST be ignored when determining
version precedence. Thus two versions that differ only in the build metadata,
have the same precedence. Examples: 1.0.0-alpha+001, 1.0.0+20130313144700,
1.0.0-beta+exp.sha.5114f85.
	"

	| s1 s2 s3 s4 v1 v2 v3 v4 |
	s1 := '1.0.0-alpha+001' .
	s2 := '1.0.0-beta+exp.sha.5114f85' .
	s3 := '1.0.0+20130313144700' .
	s4 := '1.0.0+99999999999999' .

	v1 := s1 asRwSemanticVersionNumber.
	v2 := s2 asRwSemanticVersionNumber.
	v3 := s3 asRwSemanticVersionNumber.
	v4 := s4 asRwSemanticVersionNumber.

	self assert: v1 printString = s1.
	self assert: v2 printString = s2.
	self assert: v3 printString = s3.
	self assert: v4 printString = s4.

	self assert: v1 < v2.
	self assert: v2 < v3.
	self assert: v1 < v3.
	self assert: v1 < v4.

	self assert: v1 = v1.
	self assert: v2 = v2.
	self assert: v3 = v3.
	self assert: v4 = v4.
	self assert: v4 = v3.
%

category: 'tests'
method: RwSemanticVersionNumber200TestCase
testSpec_11

	"
Precedence refers to how versions are compared to each other when ordered.
Precedence MUST be calculated by separating the version into major, minor, patch
and pre-release identifiers in that order (Build metadata does not figure
into precedence). Precedence is determined by the first difference when
comparing each of these identifiers from left to right as follows: Major, minor,
and patch versions are always compared numerically. Example: 1.0.0 < 2.0.0 <
2.1.0 < 2.1.1. When major, minor, and patch are equal, a pre-release version has
lower precedence than a normal version. Example: 1.0.0-alpha < 1.0.0. Precedence
for two pre-release versions with the same major, minor, and patch version MUST
be determined by comparing each dot separated identifier from left to right
until a difference is found as follows: identifiers consisting of only digits
are compared numerically and identifiers with letters or hyphens are compared
lexically in ASCII sort order. Numeric identifiers always have lower precedence
than non-numeric identifiers. A larger set of pre-release fields has a higher
precedence than a smaller set, if all of the preceding identifiers are equal.
Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta <
1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0.
	"

	| vrsns vrsna vrsnb |
	vrsns := #( '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-alpha.beta' '1.0.0-beta' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0').
	vrsns
		do: [:str |
			vrsnb := str asRwSemanticVersionNumber.
			self assert: vrsnb printString = str.
			self assert: vrsnb = vrsnb.
			vrsna ifNotNil: [ self assert: vrsna < vrsnb ].
			vrsna := vrsnb ].
	vrsna := nil.
	vrsns reverse
		do: [:str |
			vrsnb := str asRwSemanticVersionNumber.
			self assert: vrsnb printString = str.
			self assert: vrsnb = vrsnb.
			vrsna ifNotNil: [ self assert: vrsna > vrsnb ].
			vrsna := vrsnb ].

	self deny: '1.0.0-alpha.beta' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber
%

! Class implementation for 'RwSemanticVersionNumberTestCase'

!		Class methods for 'RwSemanticVersionNumberTestCase'

category: 'Tests'
classmethod: RwSemanticVersionNumberTestCase
shouldInheritSelectors

	^true
%

!		Instance methods for 'RwSemanticVersionNumberTestCase'

category: 'tests'
method: RwSemanticVersionNumberTestCase
sampleVersionStrings
    ^ #('1.0.0+-' '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-0.3.7' '1.0.0-x.7.z.92')
        , #('1.0.0+build.1' '1.3.7+build.11.e0f985a')
        ,
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0-rc.1+build.1' '1.0.0' '1.0.0+0.3.7' '1.3.7+build' '1.3.7+build.2.b8f12d7' '1.3.7+build.11.e0f985a')
        , #('1.0.0-alp-h-a' '1.0.0-r-c.1' '1.0.0+alp-h-a' '1.0.0+r-c.1')
%

category: 'test alpha/numeric version numbers'
method: RwSemanticVersionNumberTestCase
testAlphaNumericVersion1

	"Use numeric comparison for pure numbers. If you non-numeric version separate with '-'"
	
	| x y |
	self assert: ((x := self versionClass fromString: '2.9.0') < (y := self versionClass fromString: '2.10.0')).
%

category: 'test alpha/numeric version numbers'
method: RwSemanticVersionNumberTestCase
testAlphaNumericVersion2

	self assert: ((self versionClass fromString: '2.9.0-alpha.2') < (self versionClass fromString: '2.9.0-alpha.3')).
%

category: 'test alpha/numeric version numbers'
method: RwSemanticVersionNumberTestCase
testAlphaNumericVersion3

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.10')).
%

category: 'test alpha/numeric version numbers'
method: RwSemanticVersionNumberTestCase
testAlphaNumericVersion4

	self assert: ((self versionClass fromString: '2.9.9-alpha.2') < (self versionClass fromString: '2.9.9')).
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testEqualityComparison
    self deny: '1.0.0+-' asRwSemanticVersionNumber = '1.0.0--' asRwSemanticVersionNumber.
	self assert: '1.0.0+-' asRwSemanticVersionNumber = '1.0.0+a' asRwSemanticVersionNumber.
	self sampleVersionStrings
        do: [ :versionString | self assert: versionString asRwSemanticVersionNumber = versionString asRwSemanticVersionNumber ]
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testLessThanComparison
    self assert: '1.0.0-rc.1+build.1' asRwSemanticVersionNumber < '1.0.0' asRwSemanticVersionNumber.
    self
        assert: '1.0.0-rc.1+build.1' asRwSemanticVersionNumber < '1.0.0+build.0' asRwSemanticVersionNumber.
    self assert:  '1.0.0-0.3.7' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber.
    self assert: '1.0.0-alpha' asRwSemanticVersionNumber < '1.0.0-alpha.1' asRwSemanticVersionNumber.
    self assert: '1.0.0-0.3.7' asRwSemanticVersionNumber < '1.0.0-x.7.z.92' asRwSemanticVersionNumber
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testOfficialInvalidSemanticVersions
	"https://github.com/semver/semver/issues/232#issuecomment-430813095"

	"https://github.com/GemTalk/Rowan/issues/381"

    | vrsn |
    {
		'1'.
		'1.2'.
		'1.2.3-0123'.
		'1.2.3-0123.0123'.
		'1.1.2+.123'.
		'+invalid'.
		'-invalid'.
		'-invalid+invalid'.
		'-invalid.01'.
		'alpha'.
		'alpha.beta'.
		'alpha.beta.1'.
		'alpha.1'.
		'alpha+beta'.
		'alpha_beta'.
		'alpha.'.
		'alpha..'.
		'beta'.
		'1.0.0-alpha_beta'.
		'-alpha.'.
		'1.0.0-alpha..'.
		'1.0.0-alpha..1'.
		'1.0.0-alpha...1'.
		'1.0.0-alpha....1'.
		'1.0.0-alpha.....1'.
		'1.0.0-alpha......1'.
		'1.0.0-alpha.......1'.
		'01.1.1'.
		'1.01.1'.
		'1.1.01'.
		'1.2'.
		'1.2.3.DEV'.
		'1.2-SNAPSHOT'.
		'1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788'.
		'1.2-RC-SNAPSHOT'.
		'-1.0.3-gamma+b7718'.
		'+justmeta'.
		'9.8.7+meta+meta'.
		'9.8.7-whatever+meta+meta'.
		'99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12'.
	}
	do: [ :versionString |
		self should: [ vrsn := versionString asRwSemanticVersionNumber ] raise: Error ]
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testOfficialValidSemanticVersions
	"https://github.com/semver/semver/issues/232#issuecomment-430813095"

	"https://github.com/GemTalk/Rowan/issues/381"

	| x vrsn |
    {
		'0.0.0'.
		'0.0.1'.
		'0.0.4'.
		'1.2.3'.
		'10.20.30'.
		'1.1.2-prerelease+meta'.
		'1.1.2+meta'.
		'1.1.2+meta-valid'.
		'1.0.0-alpha'.
		'1.0.0-beta'.
		'1.0.0-alpha.beta'.
		'1.0.0-alpha.beta.1'.
		'1.0.0-alpha.1'.
		'1.0.0-alpha0.valid'.
		'1.0.0-alpha.0valid'.
		'1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay'.
		'1.0.0-rc.1+build.1'.
		'2.0.0-rc.1+build.123'.
		'1.2.3-beta'.
		'10.2.3-DEV-SNAPSHOT'.
		'1.2.3-SNAPSHOT-123'.
		'1.0.0'.
		'2.0.0'.
		'1.1.7'.
		'2.0.0+build.1848'.
		'2.0.1-alpha.1227'.
		'1.0.0-alpha+beta'.
		'1.2.3----RC-SNAPSHOT.12.9.1--.12+788'.
		'1.2.3----R-S.12.9.1--.12+meta'.
		'1.2.3----RC-SNAPSHOT.12.9.1--.12'.
		'1.0.0+0.build.1-rc.10000aaa-kk-0.1'.
		'99999999999999999999999.999999999999999999.99999999999999999'.
		'1.0.0-0A.is.legal'.
		}
	do: [ :versionString | self assert: versionString = (x := (vrsn := versionString asRwSemanticVersionNumber) printString) ]
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testPrinting
    | x vrsn |
    self sampleVersionStrings
        do: [ :versionString | self assert: versionString = (x := (vrsn := versionString asRwSemanticVersionNumber) printString) ]
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testSemanticVersioning
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0-rc.1.0' '1.0.0-rc.2').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0').
    self validateSemanticVersionStrings: #('1.0.0-1' '1.0.0-alpha').
    self validateSemanticVersionStrings: #('1.0.0-alpha' '1.0.0+1').
    self validateSemanticVersionStrings: #('1.0.0' '1.0.1').
    self validateSemanticVersionStrings: #('1.0.0--' '1.0.0-a').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0' '1.0.1').
    self validateSemanticVersionStrings: #('1.0.0-rc.1' '1.0.0-rc.2' '1.0.0-rc.3').
    self validateSemanticVersionStrings: #('1.0.0-10000' '1.0.0-a')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testSemanticVersioningSpecItem10
    "[Semantic Versioning 2.0.0-rc.1](http://semver.org/)"

    self validateSemanticVersionStrings: #( '1.0.0-0.3.7' '1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-x.7.z.92')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testSemanticVersioningSubsetCompliance
    "subset of sample versions that are compatible with MetacellVersionNumber syntax"

    self
        validateSemanticVersionStrings:
            #('1.0.0-alpha' '1.0.0-alpha.1' '1.0.0-beta.2' '1.0.0-beta.11' '1.0.0-rc.1' '1.0.0')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion01

	self assert: ((self versionClass fromString: '1.1.1') versionString = '1.1.1')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion02

	| v1 v2 |
	v1 := self versionClass fromString: '1.1.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 = v1).	
	self assert: (v2 = v2).
	self assert: (v1 > v2)
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion04

	| v1 v2 |
	v1 := self versionClass fromString: '1.0.1'.
	v2 := self versionClass fromString: '1.0.0'.
	self assert: (v1 > v2)
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion05
    | v1 v2 |
    v1 := self versionClass fromString: '3.0.0'.
    v2 := self versionClass fromString: '2.0.0'.
    self assert: v1 > v2
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion09
    | v1 v2 |
    v1 := self versionClass fromString: '1.0.0'.
    v2 := self versionClass fromString: '0.7.0'.
    self assert: v1 >= v2.
    self assert: v2 <= v1
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion10
    | x y |
    self
        assert:
            (x := (({(self versionClass fromString: '1.0.0').
            (self versionClass fromString: '0.7.0').
            (self versionClass fromString: '0.8.0').
            (self versionClass fromString: '0.9.0').
            (self versionClass fromString: '1.0.1')} sort: [ :a :b | a <= b ]) collect: [ :each | each versionString ])
                asArray) = (y := #('0.7.0' '0.8.0' '0.9.0' '1.0.0' '1.0.1'))
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion12
    self deny: (self versionClass fromString: '1.0.0') <= (self versionClass fromString: '0.7.0')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion14

	self assert: ((self versionClass fromString: '2.9.0-alpha02') < (self versionClass fromString: '2.9.0-alpha03')).
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion15
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0-beta.1')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion16
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion17
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-0').
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-beta.0').
    self assert: (self versionClass fromString: '1.0.0') > (self versionClass fromString: '1.0.0-beta')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion18
    | x y |
    self deny: (x := self versionClass fromString: '1.0.0') < (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') > (y := self versionClass fromString: '1.0.0-0').
    self assert: (x := self versionClass fromString: '1.0.0') = (y := self versionClass fromString: '1.0.0+0').
 
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
testVersion19
    self assert: (self versionClass fromString: '1.0.0-beta.0') < (self versionClass fromString: '1.0.0')
%

category: 'tests'
method: RwSemanticVersionNumberTestCase
validateSemanticVersionStrings: versionStrings
    | versions version |
    versions := versionStrings collect: [ :each | each asRwSemanticVersionNumber ].
    version := versions at: 1.
    2 to: versions size do: [ :index | 
        | nextVersion |
        nextVersion := versions at: index.
        self assert: version < nextVersion.
        version := nextVersion ]
%

category: 'private'
method: RwSemanticVersionNumberTestCase
versionClass
    ^ RwSemanticVersionNumber
%

! Class implementation for 'STONReaderTests'

!		Class methods for 'STONReaderTests'

category: 'Testing'
classmethod: STONReaderTests
shouldInheritSelectors
  "Me and my subclasses inherit selectors"

  ^ true
%

!		Instance methods for 'STONReaderTests'

category: 'private'
method: STONReaderTests
materialize: string
	^ STON reader 
		on: string readStream;
		next
%

category: 'private'
method: STONReaderTests
sumOf: aCollection
 | sum sample |
  sample := aCollection detect: [:each | true ].
  sum := aCollection inject: sample into: [ :accum :each | accum + each ].
  ^ sum - sample
%

category: 'tests'
method: STONReaderTests
testBoolean
	self assert: (self materialize: 'true') = true.
	self assert: (self materialize: 'false') = false
%

category: 'tests'
method: STONReaderTests
testByteArray
	self assert: (self materialize: 'ByteArray[''010203'']') = #(1 2 3) asByteArray
%

category: 'tests'
method: STONReaderTests
testCharacter
	self assert: (self materialize: 'Character[''A'']') == $A.
%

category: 'tests'
method: STONReaderTests
testDate
	| date |
	date := Date newDay: 1 month:  'January' year: 2012.
	self assert: (self materialize: 'Date[''2012-01-01'']') = date
%

category: 'tests'
method: STONReaderTests
testDateAndTime
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: (Duration seconds: 60*60).
	self assert: (self materialize: 'DateAndTime[''2012-01-01T06:30:15+01:00'']') = dateAndTime
%

category: 'tests'
method: STONReaderTests
testDictionary
	| collection |
	collection := STON mapClass new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: '{1:1,2:2}') = collection.
	self assert: (self materialize: '{}') = STON mapClass new.
%

category: 'tests'
method: STONReaderTests
testDictionaryWithComplexKeys
	| collection reader |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	(reader := STONReader on: '{true:1,[#foo]:2}' readStream)
		allowComplexMapKeys: true.
	self assert: reader next = collection
%

category: 'tests'
method: STONReaderTests
testError
	#( 'foo' '{foo:}' '{foo,}' '[1,]' '+1' ']' '#' '' '  ' '	' 'nul' 'tru' 'fals' ) do: [ :each |
		self 
			should: [ self materialize: each ] 
			raise: STONReaderError ]
%

category: 'tests'
method: STONReaderTests
testFloat
	self assert: (self materialize: '1.5') = 1.5.
	self assert: (self materialize: '-1.5') = -1.5.
	self assert: (self materialize: '0.0') = 0.0.
false ifTrue: [ 
	self assert: (Float pi closeTo: (self materialize: '3.14149')).
	self assert: (1/3 closeTo: (self materialize: '0.333333'))].
	self assert: (self materialize: '1.0e100') = (10 raisedTo: 100) asFloat.
	self assert: (self materialize: '1.0e-100') = (10 raisedTo: -100) asFloat.
	self assert: (self materialize: '-1.0e-100') = (10 raisedTo: -100) asFloat negated.
%

category: 'tests'
method: STONReaderTests
testIdentityDictionary
	| collection |
	collection := IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self materialize: 'IdentityDictionary{1:1,2:2}') = collection.
	self assert: (self materialize: 'IdentityDictionary{}') = IdentityDictionary new.
%

category: 'tests'
method: STONReaderTests
testInteger
	self assert: (self materialize: '1') = 1.
	self assert: (self materialize: '-1') = -1.
	self assert: (self materialize: '0') = 0.
	self assert: (self materialize: '1234567890') = 1234567890.
	self assert: (self materialize: '-1234567890') = -1234567890
%

category: 'tests'
method: STONReaderTests
testJsonString
  "Allow double quotes for backwards JSON compatibility"

  | string |
  self assert: (self materialize: '"foo"') = 'foo'.
  self assert: (self materialize: '"FOO"') = 'FOO'.
  self
    assert:
      (self materialize: '"\u00E9l\u00E8ve en Fran\u00E7ais"') = 'élève en Français'.
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self materialize: '"\"\''\\\t\r\n\f\b"') = string
%

category: 'tests'
method: STONReaderTests
testList
	self assert: STON listClass = Array.
	self assert: (self materialize: '[1,2,3]') = (STON listClass with: 1 with: 2 with: 3).
	self assert: (self materialize: '[]') = STON listClass new
%

category: 'tests'
method: STONReaderTests
testMap
	self assert: (self materialize: '{#foo:1}') = (STON mapClass new at: #foo put: 1; yourself).
	self assert: (self materialize: '{}') = STON mapClass new
%

category: 'tests'
method: STONReaderTests
testMultiple
	| reader |
	reader := STON reader 
		on: '123 -123 nil #foo true [ 0 ] false { #one : 1 }' readStream.
	self deny: reader atEnd.
	self assert: reader next equals: 123. 
	self assert: reader next equals: -123. 
	self assert: reader next equals: nil. 
	self assert: reader next equals: #foo. 
	self assert: reader next equals: true. 
	self assert: reader next equals: { 0 }. 
	self assert: reader next equals: false. 
	self assert: reader next equals: (Dictionary with: #one -> 1). 
	self assert: reader atEnd.
%

category: 'tests'
method: STONReaderTests
testNewSymbol
	| n notASymbol shouldBeSymbol |
	
	"Find a name that has not yet been interned"
	n := 0.
	[ (Symbol _existingWithAll: (notASymbol := 'notASymbol', n printString)) notNil ] 
		whileTrue: [ n := n + 1 ].
	"Parsing the new, not yet interned name should create a new Symbol"
	shouldBeSymbol := self materialize: '#', notASymbol.
	self assert: (shouldBeSymbol isSymbol and: [ notASymbol = shouldBeSymbol asString ])
%

category: 'tests'
method: STONReaderTests
testNil
	self assert: (self materialize: 'nil') = nil
%

category: 'tests'
method: STONReaderTests
testNull
	self assert: (self materialize: 'null') = nil
%

category: 'tests'
method: STONReaderTests
testObject
	self assert: (self materialize: 'Object{}') class == Object.
%

category: 'tests'
method: STONReaderTests
testOrderedCollection
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self materialize: 'OrderedCollection[1,2,3]') = collection.
	self assert: (self materialize: 'OrderedCollection[]') = OrderedCollection new.
%

category: 'tests'
method: STONReaderTests
testReferenceCycle
	| array |
	array := (self materialize: '[1,@1]').
	self assert: array class = STON listClass.
	self assert: array size = 2.
	self assert: array first = 1.
	self assert: array second == array
%

category: 'tests'
method: STONReaderTests
testReferenceSharing
	| one array |
	one := { #one }.
	array := (self materialize: '[[#one],@2,@2]').
	self assert: array = (STON listClass with: one with: one with: one).
	self assert: array first == array second.
	self assert: array first == array third
%

category: 'tests'
method: STONReaderTests
testStreaming
	| reader |
	reader := STON reader 
		on: '1 2 3 4 5 6 7 8 9 10' readStream.
	self 
		assert: (self sumOf: (Array streamContents: [ :stream |
			[ reader atEnd] whileFalse: [ 
				stream nextPut: reader next ] ]))
		equals: (self sumOf: #(1 2 3 4 5 6 7 8 9 10))
%

category: 'tests'
method: STONReaderTests
testString
  | string x |
  self assert: (self materialize: '''foo''') = 'foo'.
  self assert: (self materialize: '''FOO''') = 'FOO'.
  self
    assert:
      (x := self materialize: '''\u00E9l\u00E8ve en Fran\u00E7ais''')
        = 'élève en Français'.
  self
	assert:
		(x := self
			materialize:
				'''\u042F \u043C\u043E\u0436\u0443 \u0457\u0441\u0442\u0438 \u0441\u043A\u043B\u043E, \u0456 \u0432\u043E\u043D\u043E \u043C\u0435\u043D\u0456 \u043D\u0435 \u0437\u0430\u0448\u043A\u043E\u0434\u0438\u0442\u044C.''')
					= self unicode16TestString.
  false
    ifTrue: [ 
      "ambiguous encoding for 32-bit Unicode characters: https://github.com/svenvc/ston/issues/11"
      self
        assert:
          (x := self materialize: '''\u2338F''') = self unicode32TestString ].
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self materialize: '''\"\''\\\t\r\n\f\b''') = string
%

category: 'tests'
method: STONReaderTests
testSymbol
	self assert: (self materialize: '#''foo''') = #foo.
	self assert: (self materialize: '#foo') = #foo
%

category: 'tests'
method: STONReaderTests
testTime
	| time |
	"time := Time hour: 6 minute: 30 second: 15."
	time := Time fromSeconds: (6 * 60 *60) + (30 *60) + 15.
	self assert: (self materialize: 'Time[''06:30:15'']') = time.
%

category: 'tests'
method: STONReaderTests
testUser
	| user x |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (x := self materialize: 'STONTestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user
%

category: 'tests'
method: STONReaderTests
testUser2
	| user |
	(user := STONTestUser2 new)
		username: 'john@foo.com';
		password: 'secret1'.
	self assert: (self materialize: 'STONTestUser2{#username:''john@foo.com'',#password:''secret1'',#enabled:true}') = user
%

category: 'tests'
method: STONReaderTests
testWhitespace
  | whitespace |
  whitespace := String
    withAll:
      {(Character space).
      (Character tab).
      (Character cr).
      (Character lf)}.
  self assert: (self materialize: whitespace , '123') = 123
%

category: 'private'
method: STONReaderTests
unicode16TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8 asString "Normalize string so that it is either a Unicode string or a Legacy string"
%

category: 'private'
method: STONReaderTests
unicode32TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'ð£' decodeFromUTF8
%

! Class implementation for 'STONTests'

!		Class methods for 'STONTests'

category: 'utilities'
classmethod: STONTests
readFromFileNamed: path
	^ path asFileReference
		readStreamDo: [ :stream | 
			STON reader
				on: stream;
				next ]
%

category: 'utilities'
classmethod: STONTests
write: object toFileNamed: path
	^ path asFileReference
		writeStreamDo: [ :stream | 
			STON writer
				on: stream;
				nextPut: object ]
%

!		Instance methods for 'STONTests'

category: 'tests'
method: STONTests
testFromString
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON fromString: '[1,0,-1,true,false,nil]') = object
%

category: 'tests'
method: STONTests
testPrettyPrinting
  | object |
  object := STONTestUser dummy.
  self assert: (STON fromString: (STON toStringPretty: object)) = object.
  object := STONTestDomainObject dummy.
  self assert: (STON fromString: (STON toStringPretty: object)) = object
%

category: 'tests'
method: STONTests
testRoomExitCycles
	| model room1 room2 exit1 exit2 ston object |
	(room1 := STONTestMap new) at: #name put: 'Room 1'.
	(room2 := STONTestMap new) at: #name put: 'Room 2'.
	(exit1 := STONTestMap new)
		at: #name put: 'Exit 1';
		at: #origin put: room1;
		at: #destination put: room2.
	(exit2 := STONTestMap new)
		at: #name put: 'Exit 2';
		at: #origin put: room2;
		at: #destination put: room1.
	room1 at: #exit put: exit1.
	room2 at: #exit put: exit2.
	model := Array with: room1 with: room2.
	ston := STON toString: model.
	object := STON fromString: ston.
	"We can't just compare because this is a recursive datastructure"
	self assert: (object first at: #name) equals: 'Room 1'.
	self assert: (object second at: #name) equals: 'Room 2'.
	self assert: ((object first at: #exit) at: #name) equals: 'Exit 1'.
	self assert: ((object second at: #exit) at: #name) equals: 'Exit 2'.
	self assert: ((object first at: #exit) at: #origin) == object first.
	self assert: ((object first at: #exit) at: #destination) == object second.
	self assert: ((object second at: #exit) at: #origin) == object second.
	self assert: ((object second at: #exit) at: #destination) == object first.
	"Try writing again the parse model" 
	self assert: (STON toString: object) equals: ston
%

category: 'tests'
method: STONTests
testToString
	| object |
	object := STON listClass withAll: { 1. 0. -1. true. false. nil }.
	self assert: (STON toString: object) = '[1,0,-1,true,false,nil]'
%

! Class implementation for 'STONWriteReadTests'

!		Class methods for 'STONWriteReadTests'

category: 'Testing'
classmethod: STONWriteReadTests
shouldInheritSelectors
  "Me and my subclasses inherit selectors"

  ^ true
%

!		Instance methods for 'STONWriteReadTests'

category: 'private'
method: STONWriteReadTests
encodeOnSerialize
  ^ false
%

category: 'private'
method: STONWriteReadTests
include32BitUnicodeStrings
  ^ false
%

category: 'private'
method: STONWriteReadTests
jsonWriter
  ^ self writer
    jsonMode: true;
    yourself
%

category: 'private'
method: STONWriteReadTests
materialize: string
  | str |
  str := string.
  self encodeOnSerialize
    ifTrue: [ str := string decodeFromUTF8 ].
  ^ self reader
    on: str readStream;
    next
%

category: 'private'
method: STONWriteReadTests
prettyWriter
  ^ self writer
    prettyPrint: true;
    yourself
%

category: 'private'
method: STONWriteReadTests
reader
  ^ STONReader new
%

category: 'private'
method: STONWriteReadTests
serialize: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self writer
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'private'
method: STONWriteReadTests
serializeAndMaterialize: object
	| serialization materialization |
	serialization := self serialize: object.
	materialization := self materialize: serialization.
	self assert: object equals: materialization
	
%

category: 'private'
method: STONWriteReadTests
serializeAndMaterializeJsonMode: object
	| serialization materialization |
	serialization := self serializeJson: object.
	materialization := self materialize: serialization.
	self assert: object equals: materialization
%

category: 'private'
method: STONWriteReadTests
serializeJson: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self jsonWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'tests'
method: STONWriteReadTests
testAssociations
	| associations |
	associations := OrderedCollection new.
	1 to: 10 do: [ :each |
		associations add: each -> each printString ].
	self serializeAndMaterialize: associations
%

category: 'tests'
method: STONWriteReadTests
testCharacters
	| characters |
	characters := STON listClass withAll: {$a. $b. $m. $z}, {$A. $B. $M. $Z}.
	self serializeAndMaterialize: characters
%

category: 'tests'
method: STONWriteReadTests
testCollections
  | collections |
  collections := STON listClass
    withAll:
      {#(1 2 3).
      (OrderedCollection withAll: #(1 2 3)).
      (Set withAll: #(1 2 3)).
      (IdentitySet withAll: #(1 2 3)).
      (Bag withAll: #(1 2 2 3)).
      (Dictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (IdentityDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (#(1 2 3) asByteArray)}.
  self serializeAndMaterialize: collections
%

category: 'tests'
method: STONWriteReadTests
testComplexSet
  | collections serialization materialization |
  collections := Set
    withAll:
      {#(1 2 3).
      (OrderedCollection withAll: #(1 2 3)).
      (Set withAll: #(1 2 3)).
      (IdentitySet withAll: #(1 2 3)).
      (Bag withAll: #(1 2 2 3)).
      (Dictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (IdentityDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself).
      (#(1 2 3) asByteArray)}.
  serialization := self serialize: collections.
  materialization := self materialize: serialization.
  collections do: [ :each | self assert: (materialization includes: each) ].
  materialization do: [ :each | self assert: (collections includes: each) ]
%

category: 'tests'
method: STONWriteReadTests
testDomainObject
	| object objects |
	object := STONTestDomainObject dummy.
	self serializeAndMaterialize: object.
	objects := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestDomainObject dummy ] ].
	self serializeAndMaterialize: objects.
	objects := STON mapClass new.
	10 timesRepeat: [ | newObject |
		newObject := STONTestDomainObject dummy.
		objects at: newObject integer put: newObject ].
	self serializeAndMaterialize: objects.
%

category: 'tests'
method: STONWriteReadTests
testEmpty
	| empty |
	empty := STON listClass new.
	self serializeAndMaterialize: empty.
	empty := STON mapClass new.
	self serializeAndMaterialize: empty.
%

category: 'tests'
method: STONWriteReadTests
testFloats
	| floats serialization materialization |
	floats := STON listClass withAll: ((-10 to: 10) collect: [ :each | each * Float pi ]).
	serialization := self serialize: floats.
	materialization := self materialize: serialization.
	self assert: floats size = materialization size.
	1 to: floats size do: [:index | | float |
		float := floats at: index.
		"Use #closeTo: instead of #= to increase portability"
		self assert: (float closeTo: (materialization at: index)) ]
%

category: 'tests'
method: STONWriteReadTests
testJsonMode
	| object |
	object := STON listClass withAll: {
		Float pi.
		'Hello World'.
		true.
		nil.
		STON listClass withAll: #( 1 2 3) asByteArray.
		STON mapClass new 
			at: 'x' put: 1; 
			at: 'y' put: 2; 
			yourself 
	}.
	self serializeAndMaterializeJsonMode: object
%

category: 'tests'
method: STONWriteReadTests
testPrimitives
	| primitives |
	primitives := STON listClass withAll: { true. false. nil }.
	self serializeAndMaterialize: primitives
%

category: 'tests'
method: STONWriteReadTests
testSmallIntegers
	| integers |
	integers := STON listClass withAll: (-10 to: 10).
	self serializeAndMaterialize: integers
%

category: 'tests'
method: STONWriteReadTests
testStrings
	| strings |
	strings := Collection allSubclasses collect: [ :each | each name asString ].
	self serializeAndMaterialize: strings
%

category: 'tests'
method: STONWriteReadTests
testSymbols
	| symbols |
	self serializeAndMaterialize: #( #bytes #'' ).
	symbols := Collection allSubclasses collect: [ :each | each name ].
	self serializeAndMaterialize: symbols
%

category: 'tests'
method: STONWriteReadTests
testUnicodeStrings
  | strings |
  strings := {(String
    withAll:
      {(Character codePoint: 0).
      (Character codePoint: 255).
      (Character codePoint: 256)}).
  (self unicode16TestString).
  (self unicode32TestString).
  'élève en Français'}.
  strings := strings collect: [:each | each asString ].  "Normalize strings so that they are all Unicode or all Legacy string"
  self serializeAndMaterialize: strings
%

category: 'tests'
method: STONWriteReadTests
testUser
	| user users |
	user := STONTestUser dummy.
	self serializeAndMaterialize: user.
	users := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestUser dummy ] ].
	self serializeAndMaterialize: users.
	users := STON mapClass new.
	10 timesRepeat: [ | newUser |
		newUser := STONTestUser dummy.
		users at: newUser username put: newUser ].
	self serializeAndMaterialize: users.
%

category: 'tests'
method: STONWriteReadTests
testUser2
	| user users |
	user := STONTestUser2 dummy.
	self serializeAndMaterialize: user.
	users := STON listClass streamContents: [ :stream |
		10 timesRepeat: [ stream nextPut: STONTestUser2 dummy ] ].
	self serializeAndMaterialize: users.
	users := STON mapClass new.
	10 timesRepeat: [ | newUser |
		newUser := STONTestUser2 dummy.
		users at: newUser username put: newUser ].
	self serializeAndMaterialize: users.
%

category: 'private'
method: STONWriteReadTests
unicode16TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8
%

category: 'private'
method: STONWriteReadTests
unicode32TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ self include32BitUnicodeStrings
    ifTrue: [ 'ð£' decodeFromUTF8 ]
    ifFalse: [ 'abc' ]
%

category: 'private'
method: STONWriteReadTests
writer
  ^ STONWriter new
%

! Class implementation for 'STONLargeWriteReadTests'

!		Instance methods for 'STONLargeWriteReadTests'

category: 'private'
method: STONLargeWriteReadTests
materialize: string
	^ STON reader
		on: string readStream;
		optimizeForLargeStructures;
		next
%

category: 'private'
method: STONLargeWriteReadTests
serialize: anObject
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			prettyPrint: true;
			optimizeForLargeStructures; 
			nextPut: anObject ]
%

category: 'private'
method: STONLargeWriteReadTests
serializeJson: anObject
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			optimizeForLargeStructures; 
			nextPut: anObject ]
%

! Class implementation for 'STONWritePrettyPrinterReadTests'

!		Instance methods for 'STONWritePrettyPrinterReadTests'

category: 'private'
method: STONWritePrettyPrinterReadTests
serialize: anObject
	^ String streamContents: [ :stream |
		STON writer 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
%

category: 'private'
method: STONWritePrettyPrinterReadTests
serializeJson: anObject
	^ String streamContents: [ :stream |
		STON jsonWriter 
			on: stream; 
			prettyPrint: true;
			nextPut: anObject ]
%

! Class implementation for 'STONWriterTests'

!		Class methods for 'STONWriterTests'

category: 'Testing'
classmethod: STONWriterTests
shouldInheritSelectors
  "Me and my subclasses inherit selectors"

  ^ true
%

!		Instance methods for 'STONWriterTests'

category: 'private'
method: STONWriterTests
encodeOnSerialize
  ^ false
%

category: 'private'
method: STONWriterTests
jsonWriter
  ^ self writer
    jsonMode: true;
    yourself
%

category: 'private'
method: STONWriterTests
materialize: string
  | str |
  str := string.
  self encodeOnSerialize
    ifTrue: [ str := string decodeFromUTF8 ].
  ^ self reader
    on: str readStream;
    allowComplexMapKeys: true;
    next
%

category: 'private'
method: STONWriterTests
prettyWriter
  ^ self writer
    prettyPrint: true;
    yourself
%

category: 'private'
method: STONWriterTests
reader
  ^ STONReader new
%

category: 'private'
method: STONWriterTests
serialize: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self writer
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'private'
method: STONWriterTests
serializeJson: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self jsonWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'private'
method: STONWriterTests
serializePretty: anObject
  | str |
  str := String
    streamContents: [ :stream | 
      self prettyWriter
        on: stream;
        nextPut: anObject ].
  self encodeOnSerialize
    ifFalse: [ ^ str ].
  ^ str encodeAsUTF8intoString
%

category: 'tests'
method: STONWriterTests
testBoolean
	self assert: (self serialize: true) = 'true'.
	self assert: (self serialize: false) = 'false'
%

category: 'tests'
method: STONWriterTests
testByteArray
	self assert: (self serialize: #(1 2 3) asByteArray) = 'ByteArray[''010203'']' 
%

category: 'tests'
method: STONWriterTests
testCustomNewline
	| output lf expectedResult |
	lf := String with: Character lf.
	output := String streamContents: [ :out |
		(STON writer on: out)
			newLine: lf;
			prettyPrint: true;
			nextPut: #( 1 ) ].
	expectedResult := String streamContents: [:out |
		out 
			nextPut: $[;
			lf;
			tab;
			nextPut: $1;
			lf;
			nextPut: $];
			yourself ].
	self 
		assert: output 
		equals: expectedResult
%

category: 'tests'
method: STONWriterTests
testDate
	| date |
	date := Date newDay: 1 month:  'January' year: 2012.
	self assert: (self serialize: date) = 'Date[''2012-01-01'']'
%

category: 'tests'
method: STONWriterTests
testDateAndTime
	| dateAndTime |
	dateAndTime := DateAndTime year: 2012 month: 1 day: 1 hour: 6 minute: 30 second: 15 offset: (Duration seconds: 60*60).
	self assert: (self serialize: dateAndTime) = 'DateAndTime[''2012-01-01T06:30:15+01:00'']'
%

category: 'tests'
method: STONWriterTests
testDictionary
	| collection |
	collection := STON mapClass new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self serialize: collection) = '{1:1,2:2}'.
	self assert: (self serialize: STON mapClass new) = '{}'.
%

category: 'tests'
method: STONWriterTests
testDictionaryWithComplexKeys
  "order dependent test"

  | collection newCollection |
  collection := STON mapClass new
    at: true put: 1;
    at: #(#'foo') put: 2;
    yourself.
  newCollection := self materialize: (self serialize: collection).
  self assert: newCollection = collection
%

category: 'tests'
method: STONWriterTests
testDoubleQuotedString
  | string |
  self assert: (self serializeJson: 'foo') = '"foo"'.
  self assert: (self serializeJson: 'FOO') = '"FOO"'.
  self
    assert:
      (self serializeJson: 'élève en Français') = '"\u00E9l\u00E8ve en Fran\u00E7ais"'.
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self serializeJson: string) = '"\"\''\\\t\r\n\f\b"'
%

category: 'tests'
method: STONWriterTests
testEmptyArrayPretty
	self assert: (self serializePretty: STON listClass new) equals: '[ ]'
%

category: 'tests'
method: STONWriterTests
testEmptyDictionaryPretty
	self assert: (self serializePretty: STON mapClass new) equals: '{ }'
%

category: 'tests'
method: STONWriterTests
testFloat
	
	self assert: ((self serialize: 1.5) asFloat closeTo: '1.5' asFloat).
	self assert: ((self serialize: 0.0) asFloat closeTo: '0.0' asFloat).
	self assert: ((self serialize: -1.5)asFloat closeTo: '-1.5' asFloat).
	self assert: ((self serialize: Float pi) beginsWith:  '3.14159').
false ifTrue: [ 	self assert: (((self serialize: 1/3) asFloat closeTo:  '0.333' asFloat)) ].
	self assert: ((self serialize: (10 raisedTo: 100) asFloat) asFloat closeTo: '1.0e100' asFloat).
	self assert: ((self serialize: (10 raisedTo: -50) asFloat) asFloat closeTo: '1.0e-50' asFloat).
	self assert: ((self serialize: (10 raisedTo: -50) asFloat negated) asFloat closeTo: '-1.0e-50' asFloat).
%

category: 'tests'
method: STONWriterTests
testIdentityDictionary
	| collection |
	collection := IdentityDictionary new at: 1 put: 1; at: 2 put: 2; yourself.
	self assert: (self serialize: collection) = 'IdentityDictionary{1:1,2:2}'.
	self assert: (self serialize: IdentityDictionary new) = 'IdentityDictionary{}'.
%

category: 'tests'
method: STONWriterTests
testInteger
	self assert: (self serialize: 1) = '1'.
	self assert: (self serialize: 0) = '0'.
	self assert: (self serialize: -1) = '-1'.
	self assert: (self serialize: 1234567890) = '1234567890'.
	self assert: (self serialize: -1234567890) = '-1234567890'
%

category: 'tests'
method: STONWriterTests
testIsSimpleSymbol
	self assert: (STON writer isSimpleSymbol: #foo).
	self assert: (STON writer isSimpleSymbol: #az).
	self assert: (STON writer isSimpleSymbol: #AZ).
	self assert: (STON writer isSimpleSymbol: #N0123456789).
	self assert: (STON writer isSimpleSymbol: #foo123).
	self assert: (STON writer isSimpleSymbol: #'Foo/Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo.Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo-Bar').
	self assert: (STON writer isSimpleSymbol: #'Foo_Bar').
	self assert: (STON writer isSimpleSymbol: #foo).
	self deny: (STON writer isSimpleSymbol: #'#^&$%')
%

category: 'tests'
method: STONWriterTests
testList
	self assert: (self serialize: (STON listClass withAll: #(1 2 3))) = '[1,2,3]'.
	self assert: (self serialize: STON listClass new) = '[]'.
	self assert: (self serialize: (STON listClass withAll: { 1. -1. 0. #foo. 'a b c'. true. false. nil })) = '[1,-1,0,#foo,''a b c'',true,false,nil]'
%

category: 'tests'
method: STONWriterTests
testMap
  | map ston |
  (map := STON mapClass new)
    at: #'foo' put: 1;
    at: #'bar' put: 2.
  ston := self serialize: map.
  self assert: (ston = '{#foo:1,#bar:2}' or: [ ston = '{#bar:2,#foo:1}' ]).
  self assert: (self serialize: STON mapClass new) = '{}'.
  map removeAllKeys: map keys.
  map at: 'foo bar' put: #'ok'.
  self assert: (self serialize: map) = '{''foo bar'':#ok}'.
  map removeAllKeys: map keys.
  map at: 123 put: 456.
  self assert: (self serialize: map) = '{123:456}'
%

category: 'tests'
method: STONWriterTests
testNil
	self assert: (self serialize: nil) = 'nil'
%

category: 'tests'
method: STONWriterTests
testNull
	self assert: (self serializeJson: nil) equals: 'null'
%

category: 'tests'
method: STONWriterTests
testObject

	self assert: (self serialize: Object new) = 'Object{}'
%

category: 'tests'
method: STONWriterTests
testOrderedCollection
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self serialize: collection) = 'OrderedCollection[1,2,3]'.
	self assert: (self serialize: OrderedCollection new) = 'OrderedCollection[]'.
%

category: 'tests'
method: STONWriterTests
testReferenceCycle
	| array |
	array := STON listClass with: 1 with: nil.
	array at: 2 put: array.
	self assert: (self serialize: array) = '[1,@1]'.
%

category: 'tests'
method: STONWriterTests
testReferenceSharing
	| array one |
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (self serialize: array) = '[[#one],@2,@2]'.
%

category: 'tests'
method: STONWriterTests
testReferenceSharingError
	| serializer array one |
	serializer := [ :object | 
		String streamContents: [ :stream |
			STON writer 
				on: stream;
				referencePolicy: #error; 
				nextPut: object ] ].
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self 
		should: [ (serializer value: array) = '[[#one],[#one],[#one]]' ] 
		raise: STONWriterError
%

category: 'tests'
method: STONWriterTests
testReferenceSharingIgnore
	| serializer array one |
	serializer := [ :object | 
		String streamContents: [ :stream |
			STON writer 
				on: stream;
				referencePolicy: #ignore; 
				nextPut: object ] ].
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (serializer value: array) = '[[#one],[#one],[#one]]'.
%

category: 'tests'
method: STONWriterTests
testRestrictedClassesInJsonMode
	self should: [ self serializeJson: STONTestUser dummy ] raise: STONWriterError.
%

category: 'tests'
method: STONWriterTests
testString
  | x string |
  self assert: (self serialize: 'foo') = '''foo'''.
  self assert: (self serialize: 'FOO') = '''FOO'''.
  self
    assert:
      (x := self serialize: 'élève en Français')
        = '''\u00E9l\u00E8ve en Fran\u00E7ais'''.
  self
    assert:
      (x := self serialize: self unicode16TestString)
        =
          '''\u042F \u043C\u043E\u0436\u0443 \u0457\u0441\u0442\u0438 \u0441\u043A\u043B\u043E, \u0456 \u0432\u043E\u043D\u043E \u043C\u0435\u043D\u0456 \u043D\u0435 \u0437\u0430\u0448\u043A\u043E\u0434\u0438\u0442\u044C.'''.
  false
    ifTrue: [ 
      "ambiguous encoding for 32-bit Unicode characters: https://github.com/svenvc/ston/issues/11"
      self
        assert: (x := self serialize: self unicode32TestString) = '''\u2338F''' ].
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self serialize: string) = '''\"\''\\\t\r\n\f\b'''
%

category: 'tests'
method: STONWriterTests
testSymbol
	self assert: (self serialize: #foo) = '#foo'.
	self assert: (self serialize: #FOO) = '#FOO'.
	self assert: (self serialize: #bytes) = '#bytes'.
	self assert: (self serialize: #'foo.bar') = '#foo.bar'.
	self assert: (self serialize: #'foo-bar') = '#foo-bar'.
	self assert: (self serialize: #'foo_bar') = '#foo_bar'.
	self assert: (self serialize: #'foo/bar') = '#foo/bar'.
	self assert: (self serialize: #'foo bar') = '#''foo bar'''.
	self assert: (self serialize: #foo123) = '#foo123'.
%

category: 'tests'
method: STONWriterTests
testSymbolAsString
	self assert: (self serializeJson: #foo) = '"foo"'.
	self assert: (self serializeJson: #'FOO') = '"FOO"'.
%

category: 'tests'
method: STONWriterTests
testTime
	| time |
	time := Time fromSeconds: (6 * 60 *60) + (30 *60) + 15.
	self assert: (self serialize: time) = 'Time[''06:30:15'']'.
%

category: 'tests'
method: STONWriterTests
testUser
	| user |
	(user := STONTestUser new)
		username: 'john@foo.com';
		password: 'secret1'.
	self 
		assert: (self serialize: user)
		equals: 'TestUser{#username:''john@foo.com'',#password:''secret1'',#enabled:true}'
%

category: 'tests'
method: STONWriterTests
testUser2
	| user |
	(user := STONTestUser2 new)
		username: 'john@foo.com';
		password: 'secret1'.
	self 
		assert: (self serialize: user)
		equals: 'TestUser2{#username:''john@foo.com'',#password:''secret1'',#enabled:true}'
%

category: 'tests'
method: STONWriterTests
testUser3Nil
	| user |
	user := STONTestUser3 new.
	self 
		assert: (self serialize: user) 
		equals: 'TestUser3{#username:nil,#password:nil,#enabled:true}'
%

category: 'tests'
method: STONWriterTests
testUserNil
	| user |
	user := STONTestUser new.
	self assert: (self serialize: user) equals: 'TestUser{#enabled:true}'
%

category: 'private'
method: STONWriterTests
unicode16TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'
    decodeFromUTF8
%

category: 'private'
method: STONWriterTests
unicode32TestString
  "Cannot embed Unicode16 or Unicode32 strings in 2.x methods"

  ^ 'ð£' decodeFromUTF8
%

category: 'private'
method: STONWriterTests
writer
  ^ STONWriter new
%

! Class implementation for 'WindowsStoreTest'

!		Instance methods for 'WindowsStoreTest'

category: 'testing'
method: WindowsStoreTest
testAbsoluteFullName
	| filesystem |
	filesystem := FileSystem store: WindowsStore new.

	"Check dropping trailing slash."	
	self assert: (filesystem referenceTo: 'C:\tmp\') fullName equals: 'C:\tmp'.

	"Check round-trip conversion from String to FileReference back to String again."
	#( 	'C:\'		'D:\'		'\'		'C:\tmp'		'D:\tmp'		'C:\tmp\xx'      ) 
		do: [ :pathString | self assert: (filesystem referenceTo: pathString) fullName equals: pathString ].
	
	
%

category: 'testing'
method: WindowsStoreTest
testAbsolutePath

	#('c:\' 'C:\temp' 'A:\temp\test' '\test\bar' '\') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isAbsolute ] 
	
%

category: 'testing'
method: WindowsStoreTest
testPrintString
	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
	
	"path"
	self assert: (filesystem referenceTo: 'a/b') printString equals: 'File @ a\b'.
	self assert: (filesystem referenceTo: '\test.txt') printString equals: 'File @ \test.txt'.
	
	"root"
	self assert: (filesystem referenceTo: '/') printString equals: 'File @ \'.
	self assert: (filesystem referenceTo: '\') printString equals: 'File @ \'.
	
	"drive root"
	self assert: (filesystem referenceTo: 'c:\') printString equals: 'File @ c:\'.
	
	"empty"
	self assert: (filesystem referenceTo: './') printString equals: 'File @ .'.
	self assert: (filesystem referenceTo: '.\') printString equals: 'File @ .'
%

category: 'testing'
method: WindowsStoreTest
testRelativeFullName

	"FIleSystem currently does not support MS Windows concept of each drive having its own working directory.
	That is too great a change during Pharo 3 Beta, so for now just drop drive from relative references. 
	Follow up in Case 13094"	

	| filesystem |
	filesystem := FileSystem store: WindowsStore new.
		
	self assert: (filesystem referenceTo: 'C:tmp') fullName equals: (filesystem referenceTo: 'tmp') fullName
%

category: 'testing'
method: WindowsStoreTest
testRelativePath

	#('a' 'bin\foo' 'temp\test' 'C:temp\test') do: [:each |
		self assert: (WindowsStore current pathFromString: each) isRelative ] 
	
%

! Class implementation for 'ZnBufferedReadStreamTests'

!		Instance methods for 'ZnBufferedReadStreamTests'

category: 'testing'
method: ZnBufferedReadStreamTests
testBuffering
	| stream |
	stream := ZnBufferedReadStream on: '01234567890123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self assert: stream atEnd	
%

category: 'testing'
method: ZnBufferedReadStreamTests
testPeek
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	'0123456789' do: [ :each |
		self deny: stream atEnd.
		self assert: stream peek equals: each.
		self assert: stream next equals: each ].
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadInto
	| stream buffer count |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	buffer := String new: 6 withAll: Character space.
	stream skip: 1.
	stream readInto: buffer startingAt: 4 count: 3.
	self assert: buffer equals: '   123'.
	stream readInto: buffer startingAt: 1 count: 3.
	self assert: buffer equals: '456123'.
	count := stream readInto: buffer startingAt: 1 count: 100.
	self assert: count equals: 3.
	self assert: buffer equals: '789123'	 
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadIntoLarger
	| stream buffer count |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	buffer := String new: 10.
	count := stream readInto: buffer startingAt: 1 count: 10.
	self assert: count equals: 10.
	self assert: buffer equals: '0123456789'	 
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadUpTo
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self assert: (stream upTo: $5) equals: '01234'.
	self assert: stream upToEnd equals: '6789'.
	self assert: stream atEnd
%

category: 'testing'
method: ZnBufferedReadStreamTests
testReadUpToEnd
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	stream next: 2.
	self assert: stream upToEnd equals: '23456789'.
	self assert: stream atEnd
%

! Class implementation for 'ZnBufferedReadWriteStreamTests'

!		Instance methods for 'ZnBufferedReadWriteStreamTests'

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testBuffering
	| stream |
	stream := ZnBufferedReadWriteStream on: '01234567890123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self assert: stream atEnd
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testNextPutAllStartingAt
	| string |
	string := String streamContents: [ :stringStream | 
		ZnBufferedReadWriteStream on: stringStream do: [ : bufferedStream |
			bufferedStream sizeBuffer: 8.
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6. 
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6.] ].
	self assert: string equals: '01234567890123456789'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testPeek
	| stream |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	'0123456789' do: [ :each |
		self deny: stream atEnd.
		self assert: stream peek equals: each.
		self assert: stream next equals: each ].
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadInto
	| stream buffer count |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	buffer := String new: 6 withAll: Character space.
	stream skip: 1.
	stream readInto: buffer startingAt: 4 count: 3.
	self assert: buffer equals: '   123'.
	stream readInto: buffer startingAt: 1 count: 3.
	self assert: buffer equals: '456123'.
	count := stream readInto: buffer startingAt: 1 count: 100.
	self assert: count equals: 3.
	self assert: buffer equals: '789123'	 
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadIntoLarger
	| stream buffer count |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	buffer := String new: 10.
	count := stream readInto: buffer startingAt: 1 count: 10.
	self assert: count equals: 10.
	self assert: buffer equals: '0123456789'	 
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadThenWrite
	| stream stringStream |
	
	stringStream := ReadWriteStreamPortable with: '0123456789' copy.
	stringStream reset.
	stream := ZnBufferedReadWriteStream on: stringStream.
	stream sizeBuffer: 8.
	
	stream next: 4.
	self assert: stream position equals: 4.
	
	stream nextPutAll: 'ABCD'.
	self assert: stream position equals: 8.
		
	self assert: stream peek equals: $8. 
	self assert: stream upToEnd equals: '89'.
	self assert: stream atEnd.
	
	self assert: stringStream contents equals: '0123ABCD89'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadUpTo
	| stream |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 8.
	self assert: (stream upTo: $5) equals: '01234'.
	self assert: stream upToEnd equals: '6789'.
	self assert: stream atEnd
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testReadUpToEnd
	| stream |
	stream := ZnBufferedReadWriteStream on: '0123456789' readStreamPortable.
	stream sizeBuffer: 4.
	stream next: 2.
	self assert: stream upToEnd equals: '23456789'.
	self assert: stream atEnd
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testWriteThenRead
	| stream stringStream |
	
	stringStream := ReadWriteStreamPortable with: '0123456789' copy.
	stringStream reset.
	stream := ZnBufferedReadWriteStream on: stringStream.
	stream sizeBuffer: 8.
	
	stream nextPutAll: 'ABCD'.
	
	self assert: stream peek equals: $4. 
	self assert: stream position equals: 4.
	self assert: stream upToEnd equals: '456789'.
	self assert: stream position equals: 10.
	self assert: stream atEnd.
	
	self assert: stringStream contents equals: 'ABCD456789'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testWriting
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedReadWriteStream on: stringStream.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream flush ].
	self assert: string = '0123456789'
%

category: 'tests'
method: ZnBufferedReadWriteStreamTests
testWritingOverflow
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedReadWriteStream on: stringStream.
		bufferedStream sizeBuffer: 8.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream nextPutAll: '0123'; nextPutAll: '4567'; nextPutAll: '89'.
		bufferedStream nextPutAll: '0123456789'; nextPutAll: '0123456789'.
		bufferedStream flush ].
	self assert: string = '0123456789012345678901234567890123456789'
%

! Class implementation for 'ZnBufferedStreamByteTests'

!		Instance methods for 'ZnBufferedStreamByteTests'

category: 'accessing'
method: ZnBufferedStreamByteTests
integerEncodingSpec
	^ #(
"<hex-bytes> <integer> <u|s> <be|le> unsigned|signed big-endian|little-endian"
('00' 0 u be)
('00' 0 s be)
('01' 1 u be)
('01' 1 s be)
('FF' -1 s be)
('FF' 255 u be)
('7B' 123 u be)
('7B' 123 s be)
('85' -123 s be)
('7F' 127 u be)
('7F' 127 s be)
('80' -128 s be)
('00' 0 u le)
('00' 0 s le)
('01' 1 u le)
('01' 1 s le)
('FF' -1 s le)
('FF' 255 u le)
('7B' 123 u le)
('7B' 123 s le)
('85' -123 s le)
('7F' 127 u le)
('7F' 127 s le)
('80' -128 s le)
('00000000' 0 u be)
('00000000' 0 s be)
('00000000' 0 u le)
('00000000' 0 s le)
('00000001' 1 u be)
('00000001' 1 s be)
('01000000' 1 u le)
('01000000' 1 s le)
('FFFFFFFF' -1 s be)
('FFFFFFFF' -1 s le)
('000004D2' 1234 u be)
('000004D2' 1234 s be)
('FFFFFB2E' -1234 s be)
('D2040000' 1234 u le)
('D2040000' 1234 s le)
('2EFBFFFF' -1234 s le)
('FFFFFFFF' 4294967295 u be)
('FFFFFFFF' 4294967295 u le)
('7FFFFFFF' 2147483647 u be)
('7FFFFFFF' 2147483647 s be)
('80000000' -2147483648 s be)
('FFFFFF7F' 2147483647 s le)
('00000080' -2147483648 s le)
('499602D2' 1234567890 u be)
('499602D2' 1234567890 s be)
('B669FD2E' -1234567890 s be)
('D2029649' 1234567890 u le)
('D2029649' 1234567890 s le)
('2EFD69B6' -1234567890 s le)
)
%

category: 'testing'
method: ZnBufferedStreamByteTests
testInt16Aliases
	| input writer |
	writer := [ :block | 
		ByteArray streamContents: [ :out | 
			ZnBufferedWriteStream on: out do: block ] ].
	input := ByteArray readHexFrom: '04D2'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int16 equals: 1234.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) uint16 equals: 1234.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) nextWord equals: 1234.
	self assert: ((ZnBufferedReadStream on: input readStreamPortable) nextNumber: 2) equals: 1234.
	self assert: ((ZnBufferedReadStream on: input reversed readStreamPortable) nextLittleEndianNumber: 2) equals: 1234.	
	self assert: (writer value: [ :out | out int16: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out uint16: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextWordPut: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextNumber: 2 put: 1234 ]) equals: input.
	self assert: (writer value: [ :out | out nextLittleEndianNumber: 2 put: 1234 ]) equals: input reversed.
	input := ByteArray readHexFrom: 'FB2E'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int16 equals: -1234.
	self assert: (writer value: [ :out | out int16: -1234 ]) equals: input
%

category: 'testing'
method: ZnBufferedStreamByteTests
testInt32Aliases
	| input writer |
	writer := [ :block | 
		ByteArray streamContents: [ :out | 
			ZnBufferedWriteStream on: out do: block ] ].
	input := ByteArray readHexFrom: '499602D2'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int32 equals: 1234567890.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) uint32 equals: 1234567890.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) nextInt32 equals: 1234567890.
	self assert: ((ZnBufferedReadStream on: input readStreamPortable) nextNumber: 4) equals: 1234567890.
	self assert: ((ZnBufferedReadStream on: input reversed readStreamPortable) nextLittleEndianNumber: 4) equals: 1234567890.	
	self assert: (writer value: [ :out | out int32: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out uint32: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextInt32Put: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextNumber: 4 put: 1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextLittleEndianNumber: 4 put: 1234567890 ]) equals: input reversed.
	input := ByteArray readHexFrom: 'B669FD2E'.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) int32 equals: -1234567890.
	self assert: (ZnBufferedReadStream on: input readStreamPortable) nextInt32 equals: -1234567890.
	self assert: (writer value: [ :out | out int32: -1234567890 ]) equals: input.
	self assert: (writer value: [ :out | out nextInt32Put: -1234567890 ]) equals: input
%

category: 'testing'
method: ZnBufferedStreamByteTests
testInt8
	| bytes readStream |
	bytes := ByteArray streamContents: [ :out |
		ZnBufferedWriteStream on: out do: [ :bout |
			bout int8: 123; uint8: 123; int8: -123 ] ].
	readStream := ZnBufferedReadStream on: bytes readStreamPortable.
	self assert: readStream peek equals: 123.
	self assert: readStream int8 equals: 123.
	self assert: readStream peek equals: 123.
	self assert: readStream uint8 equals: 123.
	self deny: readStream peek = 123.
	self deny: readStream peek = -123.
	self assert: readStream int8 equals: -123
%

category: 'testing'
method: ZnBufferedStreamByteTests
testNextIntegerOfSizeSignedBigEndian
	self integerEncodingSpec do: [ :spec |
		| input integer |
		input := ByteArray readHexFrom: spec first.
		integer := (ZnBufferedReadStream on: input readStreamPortable) 
			nextIntegerOfSize: input 
			size signed: spec third = #s 
			bigEndian: spec fourth = #be.
		self assert: integer equals: spec second ]
%

category: 'testing'
method: ZnBufferedStreamByteTests
testNextIntegerOfSizeSignedBigEndianPut
	self integerEncodingSpec do: [ :spec |
		| input output |
		input := ByteArray readHexFrom: spec first.
		output := ByteArray streamContents: [ :out |
			ZnBufferedWriteStream on: out do: [ :bout | 
				bout 
					nextIntegerOfSize: input size 
					signed: spec third = #s 
					bigEndian: spec fourth = #be 
					put: spec second ] ].
		self assert: output equals: input ]
%

! Class implementation for 'ZnBufferedWriteStreamTests'

!		Instance methods for 'ZnBufferedWriteStreamTests'

category: 'testing'
method: ZnBufferedWriteStreamTests
testNextPutAllStartingAt
	| string |
	string := String streamContents: [ :stringStream | 
		ZnBufferedWriteStream on: stringStream do: [ : bufferedStream |
			bufferedStream sizeBuffer: 8.
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6. 
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6.] ].
	self assert: string equals: '01234567890123456789'
%

category: 'testing'
method: ZnBufferedWriteStreamTests
testWriting
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedWriteStream on: stringStream.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream flush ].
	self assert: string = '0123456789'
%

category: 'testing'
method: ZnBufferedWriteStreamTests
testWritingOverflow
	| string |
	string := String streamContents: [ :stringStream | | bufferedStream |
		bufferedStream := ZnBufferedWriteStream on: stringStream.
		bufferedStream sizeBuffer: 8.
		0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
		bufferedStream nextPutAll: '0123'; nextPutAll: '4567'; nextPutAll: '89'.
		bufferedStream nextPutAll: '0123456789'; nextPutAll: '0123456789'.
		bufferedStream flush ].
	self assert: string = '0123456789012345678901234567890123456789'
%

! Class implementation for 'ZnCharacterEncoderTests'

!		Class methods for 'ZnCharacterEncoderTests'

category: 'accessing'
classmethod: ZnCharacterEncoderTests
asciiCharacterSource
	^ ($A to: $Z), ($a to: $z), ($0 to: $9), '.-_/*+=|,;?!$&<>^%#', '    '
%

category: 'accessing'
classmethod: ZnCharacterEncoderTests
latin1CharacterSource
	^ ($A to: $Z), ($a to: $z), ($0 to: $9), '.-_/*+=|,;?!$&<>^%#', '       ', 'éèçüäßñ'
%

category: 'accessing'
classmethod: ZnCharacterEncoderTests
stringOfSize: size fromSource: source
	"self stringOfSize: 1024 fromSource: self unicodeCharacterSource"
	
	^ String new: size streamContents: [ :out | 
		size timesRepeat: [ out nextPut: source atRandom ] ]
%

category: 'accessing'
classmethod: ZnCharacterEncoderTests
unicodeCharacterSource
	^ ($A to: $Z), ($a to: $z), ($0 to: $9), '.-_/*+=|,;?!$&<>^%#', '         ', 'éèçüäßñα', '€∏'
%

!		Instance methods for 'ZnCharacterEncoderTests'

category: 'public'
method: ZnCharacterEncoderTests
assertCharacterCollection: anObject equals: otherObj
	"allow comparison between unitcode and legacy strings in legacy mode"

	self
		assert: (anObject isEquivalent: otherObj)
		description: anObject printString , ' is not equal to ' , otherObj printString.
%

category: 'private'
method: ZnCharacterEncoderTests
decodeBytes: bytes with: encoder
true 
	ifTrue: [ 
	"GemStone does not support streamed decoding ... hack for tests"
	^ bytes decodeFromUTF8
	] ifFalse: [ 

	| input |
	input := bytes readStream.
	^ String streamContents: [ :stream |
		[ input atEnd ] whileFalse: [ 
			stream nextPut: (encoder nextFromStream: input) ] ] ]
%

category: 'private'
method: ZnCharacterEncoderTests
encodeString: string with: encoder
"
	^ ByteArray streamContents: [ :stream |
		stream nextPutAll: string encodeAsUTF8 ]
"

	^ ByteArray streamContents: [ :stream |
		string do: [ :each |
			encoder nextPut: each toStream: stream ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testByteDecoding
	| encoder bytes |
	encoder := ZnUTF8Encoder new.
	bytes := encoder encodeString: 'élève en Français'.
	self assert: (bytes decodeWith: encoder) equals: (encoder decodeBytes: bytes).
	self assert: (bytes decodeWith: #utf8) equals: (encoder decodeBytes: bytes).
	self assert: bytes utf8Decoded equals: (encoder decodeBytes: bytes)
%

category: 'testing'
method: ZnCharacterEncoderTests
testCodePointEncodingDecoding
	| encoder input output |
	input := {}.
	'Düsseldorf Königsallee' do: [:each | input add: each codePoint ].
	self assert: input isCollection.
	self assert: (input allSatisfy: [:each | each _isInteger ]).
	#(utf8 ) do: [ :each |
		encoder := each asZnCharacterEncoder.
		output := encoder encodeCodePoints: input.
		self assert: output isCollection.
		self assert: (output allSatisfy: [ :e | e _isInteger and: [ e between: 0 and: 255 ] ] ).
		self assert: (encoder encodedByteCountForCodePoints: input) equals: output size.
		self assert: (encoder decodeAsCodePoints: output) equals: input ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testConvencienceMethods
	| encoder string |
	encoder := ZnUTF8Encoder new.
	string := 'élève en Français'.
	self assertCharacterCollection: (encoder decodeBytes: (encoder encodeString: string)) equals: string.
	self assert: (encoder encodedByteCountForString: string) = 20.
	
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' ) do: [ :each |
		self assertCharacterCollection: (encoder decodeBytes: (encoder encodeString: each)) equals: each ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testKnownEncodingIdentifiers
	| all minimal asciiString |
	all := ZnCharacterEncoder knownEncodingIdentifiers asSet.
	minimal := #(utf8) asSet.
	"make sure at least a minimal set is present"
	self assert: (all  select: [ :each | minimal includes: each ]) equals: minimal.
	asciiString := String withAll: ((($a asciiValue to: $z asciiValue) , 
		($A asciiValue to: $Z asciiValue) , ($0 asciiValue to: $9 asciiValue)) collect: [:each | Character codePoint: each ]).
	"make sure that each identifier can be used to instanciate a decoder,
	and that those decoders at least work on a ASCII string in both directions"
	all do: [ :each |
		| encoder bytes |
		encoder := ZnCharacterEncoder newForEncoding: each.
		bytes := encoder encodeString: asciiString.
		self assert: ((encoder decodeBytes: bytes) isEquivalent: asciiString) ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testNextPutAllStartingAtToStream
	| encoder |
	encoder := ZnUTF8Encoder new.
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' 'çç' ) do: [ :each |
		#( ( '' '' ) ( 'ABC' '' ) ( '' 'ABC' ) ( 'ABC' 'ABC' ) 
			( 'AéC' '' ) ( '' 'AèC' ) ( 'AéC' 'AèC' ) 
			( 'AXC' 'AèC' ) ( 'AéC' 'AXC' ) 
			( 'PRE' 'ç' ) ) do: [ :extra |
				| prefix postfix string bytes |
				prefix := extra first.
				postfix := extra last.
				string := prefix, each, postfix.
				bytes := ByteArray streamContents: [ :out |
					encoder next: each size putAll: string startingAt: prefix size + 1 toStream: out ].
				self assertCharacterCollection: (encoder decodeBytes: bytes) equals: each ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testReadIntoStartingAtCountFromStream
	| encoder |
	encoder := ZnUTF8Encoder new.
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' 'çç' ) do: [ :each |
		#( ( '' '' ) ( 'ABC' '' ) ( '' 'ABC' ) ( 'ABC' 'ABC' ) 
			( 'AéC' '' ) ( '' 'AèC' ) ( 'AéC' 'AèC' ) 
			( 'AXC' 'AèC' ) ( 'AéC' 'AXC' ) 
			( 'PRE' 'ç' ) ) do: [ :extra |
				| prefix postfix string bytes buffer read |
				prefix := extra first.
				postfix := extra last.
				string := prefix, each.
				bytes := encoder encodeString: string, postfix.
				buffer := String new: string size.
				read := encoder readInto: buffer startingAt: 1 count: string size fromStream: bytes readStream.
				self assert: buffer equals: string.
				self assert: read equals: string size ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testReadIntoStartingAtCountFromStreamAtEnd
	| input encoder bytes readStream string read |
	encoder := ZnUTF8Encoder new.
	input := 'élève'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	string := String new: 5 withAll: $_.
	read := encoder readInto: string startingAt: 1 count: 10 fromStream: readStream.
	self assert: string equals: input.
	self assert: read equals: 5
%

category: 'testing'
method: ZnCharacterEncoderTests
testReadIntoStartingAtCountFromStreamWithOffset
	| input encoder bytes readStream string read |
	encoder := ZnUTF8Encoder new.
	input := '_élève_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 7 withAll: $_.
	read := encoder readInto: string startingAt: 2 count: 5 fromStream: readStream.
	self assertCharacterCollection: string equals: input.
	self assert: read equals: 5.
	input := '_Français_'.
	bytes := encoder encodeString: input.
	readStream := bytes readStream.
	readStream next.
	string := String new: 10 withAll: $_.
	read := encoder readInto: string startingAt: 2 count: 8 fromStream: readStream.
	self assert: string equals: input.
	self assert: read equals: 8
%

category: 'testing'
method: ZnCharacterEncoderTests
testStringEncoding
	| encoder string |
	encoder := ZnUTF8Encoder new.
	string := 'élève en Français'.
	self assert: (string encodeWith: encoder) equals: (encoder encodeString: string).
	self assert: (string encodeWith: #utf8) equals: (encoder encodeString: string).
	self assert: string encodeAsUTF8 asByteArray equals: (encoder encodeString: string)
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8Back
	| encoder stream |
	encoder := ZnUTF8Encoder new.
	stream := (encoder encodeString: 'Les élèves Françaises') readStream.
	self should: [ encoder backOnStream: stream ] raise: Error.
	4 timesRepeat: [ encoder nextFromStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $é.
	encoder backOnStream: stream.
	self assert: (encoder nextFromStream: stream) equals: $é.
	10 timesRepeat: [ encoder nextFromStream: stream ].
	13 timesRepeat: [ encoder backOnStream: stream ].
	self assert: (encoder nextFromStream: stream) equals: $s
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8Boundaries
	"Test encoding and decoding of the characters at the boundaries between 1, 2, 3, and 4 multi-byte sequences.
	Values taken from http://en.wikipedia.org/wiki/Utf8#Description with the new RFC 3629 limit"
	
	| utf8Encoder string encoded |
	utf8Encoder := ZnUTF8Encoder new.
	#( (0 16r7f) 
		(16r80 16r07FF) 
		(16r0800 16rFFFF) 
		(16r10000 16r10FFFF) 
	) doWithIndex: [ :boundaries :byteCount |
		boundaries do: [ :each |
			string := String with: each asCharacter.
			encoded := utf8Encoder encodeString: string. 
			self assertCharacterCollection: (utf8Encoder decodeBytes: encoded) equals: string.
			self assert: encoded size equals: byteCount ] ]
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8Encoder
	"The examples are taken from http://en.wikipedia.org/wiki/UTF-8#Description"
	
	| encoder inputBytes outputBytes inputString outputString |
	encoder := ZnUTF8Encoder new.
	inputString := String with: $$ with: (16r00A2 asCharacter) with: (16r20AC asCharacter) with: (16r024B62 asCharacter).
	inputBytes := #(16r24 16rC2 16rA2 16rE2 16r82 16rAC 16rF0 16rA4 16rAD 16rA2) asByteArray.
	outputBytes := self encodeString: inputString with: encoder.
	self assert: outputBytes = inputBytes.
	outputString := self decodeBytes: inputBytes with: encoder.
	self assertCharacterCollection: outputString equals: inputString
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderAuto
	| encoder inputString bytes outputString |
	encoder := ZnUTF8Encoder new.
	inputString := String withAll: ((1 to: 3072) collect: [ :each | Character codePoint: each ]).
	bytes := self encodeString: inputString with: encoder. 
	outputString := self decodeBytes: bytes with: encoder.
	self assertCharacterCollection: inputString equals: outputString
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderByteCount	
	| encoder |
	encoder := ZnUTF8Encoder new.
	self assert: (encoder encodedByteCountFor: $$) = 1.
	self assert: (encoder encodedByteCountFor: (16r00A2 asCharacter)) = 2.
	self assert: (encoder encodedByteCountFor: (16r20AC asCharacter)) = 3.
	self assert: (encoder encodedByteCountFor: (16r024B62 asCharacter)) = 4
%

category: 'testing'
method: ZnCharacterEncoderTests
testUTF8EncoderWide
	| encoder |
	encoder := ZnUTF8Encoder new.
	{ 'abc'. 'élève en Français'. 'Pra-ská' copy at: 4 put: (Character codePoint: 382); yourself. '' }
		do: [ :each | | bytes |
			bytes := self encodeString: each with: encoder. 
			self assertCharacterCollection: (encoder decodeBytes: bytes) equals: each ]
%

! Class implementation for 'ZnCharacterStreamTests'

!		Instance methods for 'ZnCharacterStreamTests'

category: 'testing'
method: ZnCharacterStreamTests
assertUpToAll: array
	| utf8Stream |
	utf8Stream := self utf8ReadStreamOn: array first.
	self assert: (array first readStream upToAll: array second) equals: array third.
	self assert: (utf8Stream upToAll: array second) equals: array third
%

category: 'testing'
method: ZnCharacterStreamTests
testNextLine
	| stream |
	stream := ZnCharacterReadStream on: 'abc' asByteArray readStreamPortable.
	self assert: stream nextLine equals: 'abc'.
	self assert: stream nextLine  equals: nil.
	stream := ZnCharacterReadStream on: '' asByteArray readStreamPortable.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: (String withAll: { 
		$a. Character cr. 
		$b. Character lf. 
		$c }) readStreamPortable.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: (String withAll:{ 
		$a. Character cr. Character lf.  
		$b. Character cr. Character lf. 
		$c. Character cr. Character lf }) readStreamPortable.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: 'b'.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
	stream := ZnCharacterReadStream on: (String withAll: { 
		$a. Character cr. Character lf.  
		Character cr. Character lf. 
		$c. Character cr. Character lf }) readStreamPortable.
	self assert: stream nextLine equals: 'a'.
	self assert: stream nextLine equals: ''.
	self assert: stream nextLine equals: 'c'.
	self assert: stream nextLine equals: nil.
%

category: 'testing'
method: ZnCharacterStreamTests
testPeek
	| string bytes readStream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	readStream := ZnBufferedReadStream on: (ZnCharacterReadStream on: bytes readStreamPortable).
	self assert: readStream peek equals: 'é' first.
	self assert: readStream peek equals: 'é' first.
	self assert: readStream next equals: 'é' first.
	readStream skip: 'lève ' size.
	self assert: readStream peek equals: $e.
	self assert: (readStream next: 'en Français' size) equals: 'en Français'
%

category: 'testing'
method: ZnCharacterStreamTests
testReadStream
	| stream |
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream on: 'ABC' asByteArray readStreamPortable).
	stream sizeBuffer: 3.
	self deny: stream atEnd.
	self deny: stream isBinary.
	self assert: stream next = $A.
	self deny: stream atEnd.
	self assert: stream peek = $B.
	self deny: stream atEnd.
	self assert: stream peek = $B.
	self deny: stream atEnd.
	self assert: stream next = $B.
	self deny: stream atEnd.
	self assert: stream next = $C.
	self assert: stream atEnd.
	self assert: stream next isNil.
	self assert: stream peek isNil
%

category: 'testing'
method: ZnCharacterStreamTests
testSimpleUTF8ReadStream
	| string bytes stream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream on: bytes readStreamPortable).
	stream sizeBuffer: string size.
	self 
		assert:stream upToEnd
		equals: string
%

category: 'testing'
method: ZnCharacterStreamTests
testSimpleUTF8WriteStream
	| string bytes stream |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := (ZnCharacterWriteStream on: ByteArray new writeStreamPortable).
	stream nextPutAll: string.
	self 
		assert: stream wrappedStream contents
		equals: bytes asByteArray
%

category: 'testing'
method: ZnCharacterStreamTests
testUpToAll
	true ifTrue: [ "upToAll: not supported for buffered streams ... skip test for now" ^ self ].
	#(
		('' '' '')
		('' 'ß' '')
		('' 'ße' '')
		('ß' '' '')
		('ße' '' '')
		('ß' 'ß' '')
		('ße' 'ß' '')
		('ß' 'ße' 'ß')
		('ß' 'e' 'ß')
		('ße' 'e' 'ß')
		('ßen' 'e' 'ß')
		('ßen' 'en' 'ß')
		('ßend' 'en' 'ß')
		('iße' 'e' 'iß')
		('ißen' 'e' 'iß')
		('ißen' 'en' 'iß')
		('ißend' 'en' 'iß')
		('iß' 'ß' 'i')
		('iße' 'ß' 'i')
		('eißen' 'ßend' 'eißen')
		('abcdefgh' 'cd' 'ab')
		('a' '' '')
		('a' 'a' '')
		('a' 'b' 'a')
		('ab' '' '')
		('ab' 'a' '')
		('ab' 'b' 'a')
		('ab' 'c' 'ab')
		('ab' 'ab' '')
		('abc' '' '')
		('abc' 'a' '')
		('abc' 'b' 'a')
		('abc' 'c' 'ab')
		('abc' 'd' 'abc')
		('abc' 'ab' '')
		('abc' 'bc' 'a')
		('abc' 'cd' 'abc')
	) do: [ :array | self assertUpToAll: array ]
%

category: 'testing'
method: ZnCharacterStreamTests
testUpToAllTwice
	| utf8Stream stream |
	true ifTrue: [ "upToAll: not supported for buffered streams ... skip test for now" ^ self ].
	utf8Stream := self utf8ReadStreamOn: 'eißendeße'.
	self assert: (utf8Stream upToAll: 'ße') equals: 'ei'.
	self assert: (utf8Stream upToAll: 'ße') equals: 'nde'.

	stream := 'eißendeße' readStreamPortable.
	self assert: (stream upToAll: 'ße') equals: 'ei'.
	self assert: (stream upToAll: 'ße') equals: 'nde'
%

category: 'testing'
method: ZnCharacterStreamTests
testUTF8ReadStreamReadInto
	| string bytes stream buffer |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream on: bytes readStreamPortable).
	stream sizeBuffer: string size.
	buffer := String new: string size.
	stream next: string size into: buffer. 
	self assert: buffer equals: string.
	self assert: stream atEnd.
	string := 'Czech in Czech is ', 269 asCharacter asString ,'e', 353 asCharacter asString , 'tina.'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream on: bytes readStreamPortable).
	stream sizeBuffer: string size.
	buffer := String new: string size.
	stream next: string size into: buffer. 
	self assert: buffer equals: string.
	self assert: stream atEnd
%

category: 'testing'
method: ZnCharacterStreamTests
utf8ReadStreamOn: string
	| bytes stream |
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnBufferedReadStream on: (ZnCharacterReadStream
		on: bytes readStreamPortable
		encoding: #utf8).
	stream sizeBuffer: string size.
	^stream
%

! Class implementation for 'DiskFileAttributesTestsResources'

!		Instance methods for 'DiskFileAttributesTestsResources'

category: 'accessing'
method: DiskFileAttributesTestsResources
afterCreationTime
	^ afterCreationTime
%

category: 'accessing'
method: DiskFileAttributesTestsResources
beforeCreationTime
	^ beforeCreationTime
%

category: 'accessing'
method: DiskFileAttributesTestsResources
file
	^ file
%

category: 'running'
method: DiskFileAttributesTestsResources
setUp

	| ws bufferedStream days hours minutes seconds |

	file := FileLocator temp / ('FileAttributesTests-', FastUUIDGenerator next asString, '.txt').
	beforeCreationTime := DateAndTime secondsLocal: DateAndTime secondsSince2001 truncated offset: (Duration seconds: 0).
	beforeCreationTime offset: (Duration seconds: (beforeCreationTime currentTimeZone transitionAtUTC: beforeCreationTime) offsetFromUTC).
	ws := file writeStream.
	bufferedStream := ZnBufferedWriteStream on: ws.
	[
		bufferedStream nextPutAll: 'Created by FileAttributesTestsResources>>setUp '.
		beforeCreationTime printOn: bufferedStream.
	] ensure: [ bufferedStream close ].
	afterCreationTime := DateAndTime now beRounded.
	"Round up to the next second"
	days := afterCreationTime dayOfYear.
	hours := afterCreationTime hour.
	minutes := afterCreationTime minute.
	seconds := afterCreationTime second + 1.
	seconds >= 60
		ifTrue: [ 
			minutes := minutes + 1.
			seconds := seconds - 60.
			minutes >= 60
				ifTrue: [
					hours := hours + 1.
					minutes := minutes - 60.
					hours >=  24
						ifTrue: [ 
							days := days + 1.
							hours := hours - 24 ] ].].
	afterCreationTime := DateAndTime
		year: afterCreationTime year day: days hour:  hours minute: minutes second: seconds
%

category: 'running'
method: DiskFileAttributesTestsResources
tearDown

	file delete.
%

! Class implementation for 'MemoryWriteStream'

!		Instance methods for 'MemoryWriteStream'

category: 'testing'
method: MemoryWriteStream
isReadOnly
	
	^ false
%

category: 'accessing'
method: MemoryWriteStream
readOnlyCopy
	
	^ collection readStreamPortable
%

category: 'positioning'
method: MemoryWriteStream
truncate
	collection truncate
%

category: 'positioning'
method: MemoryWriteStream
truncate: anInteger 
	collection truncate: anInteger 
	
%

! Class extensions for 'AbstractDictionary'

!		Class methods for 'AbstractDictionary'

category: '*ston-gemstonecommon'
classmethod: AbstractDictionary
fromSton: stonReader
	"Instances of STON mapClass will be read directly and won't arrive here.
	Other (sub)classes will use this method."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary
%

!		Instance methods for 'AbstractDictionary'

category: '*ston-gemstonecommon'
method: AbstractDictionary
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
	self class == STON mapClass
		ifTrue: [ 
			stonWriter writeMap: self ]
		ifFalse: [ 
			stonWriter 
				writeObject: self 
				do: [ stonWriter encodeMap: self ] ]
%

category: '*ston-gemstonecommon'
method: AbstractDictionary
stonProcessSubObjects: block
	"Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."
	(self class isVariable and: [ self class isBytes not and: [self class isIndexable]])
		ifTrue: [
			1 to: self _basicSize do: [ :each | |val|			
									val:= (block value: (self basicAt: each)).
									self basicAt: each put: val ] ]"
							super stonProcessSubObjects: block"
%

! Class extensions for 'BinaryFloat'

!		Instance methods for 'BinaryFloat'

category: '*ston-tests'
method: BinaryFloat
closeTo: num
  "are these two numbers close?"

  num _isNumber
    ifFalse: [ ^ [ self = num ] on: Error do: [:ignored | false ] ].
  self = 0.0
    ifTrue: [ ^ num abs < 0.0001 ].
  num = 0
    ifTrue: [ ^ self abs < 0.0001 ].
  ^ self = num asFloat
    or: [ (self - num) abs / (self abs max: num abs) < 0.0001 ]
%

! Class extensions for 'Boolean'

!		Instance methods for 'Boolean'

category: '*ston-core'
method: Boolean
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: Boolean
stonOn: stonWriter
	stonWriter writeBoolean: self
%

! Class extensions for 'ByteArray'

!		Class methods for 'ByteArray'

category: '*ston-core'
classmethod: ByteArray
fromSton: stonReader
  | singletonString |
  singletonString := stonReader parseListSingleton.
  ^ (self new: singletonString size // 2)
    readHexFrom: singletonString readStream
%

category: '*filesystem-gemstone-kernel'
classmethod: ByteArray
readHexFrom: aString
  "Create a byte array from a hexadecimal representation"

  ^ (self new: aString size // 2) readHexFrom: aString readStream
%

!		Instance methods for 'ByteArray'

category: '*filesystem-gemstone-kernel'
method: ByteArray
asString
  "Convert to a String with Characters for each byte"

  ^ String withBytes: self
%

category: '*filesystem-gemstone-kernel'
method: ByteArray
decodeWith: encoding
	"Produce a String that decodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] decodeWith: #utf8"
	
	^ encoding asZnCharacterEncoder decodeBytes: self
%

category: '*ston-gemstonebase'
method: ByteArray
readHexFrom: aStream
  "Initialize the receiver from a hexadecimal string representation"

  | map v ch value |
  map := '0123456789abcdefABCDEF'.
  1 to: self size do: [ :i | 
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := v bitShift: 4.
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := value + v.
    self at: i put: value ]
%

category: '*ston-core'
method: ByteArray
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: ByteArray
stonOn: stonWriter
  "Use a hex representation"

  stonWriter writeObject: self listSingleton: self asHexString
%

category: '*filesystem-gemstone-kernel'
method: ByteArray
utf8Decoded
	"Produce a String decoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."

	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] utf8Decoded"
	
	^ self decodeFromUTF8
%

! Class extensions for 'Character'

!		Class methods for 'Character'

category: '*filesystem-gemstone-kernel'
classmethod: Character
digitValue: x 
	"Answer the Character whose digit value is x. For example,
	 answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| n |
	n := x asInteger.
	^self withValue: (n < 10 ifTrue: [n + 48] ifFalse: [n + 55])
%

category: '*ston-core'
classmethod: Character
fromSton: stonReader
	^ stonReader parseListSingleton first
%

!		Instance methods for 'Character'

category: '*filesystem-gemstone-kernel'
method: Character
isCharacter

	^ true
%

category: '*ston-core'
method: Character
stonOn: stonWriter
	stonWriter writeObject: self listSingleton: self asString
%

! Class extensions for 'CharacterCollection'

!		Class methods for 'CharacterCollection'

category: '*filesystem-gemstone-kernel'
classmethod: CharacterCollection
crlf
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
%

category: '*ston-gemstonecommon'
classmethod: CharacterCollection
findFirstInString: aString inSet: inclusionMap startingAt: start

	"Trivial, non-primitive version"

	| i stringSize ascii |
	inclusionMap size ~= 256
		ifTrue: [ ^ 0 ].

	i := start.
	stringSize := aString size.
	[ 
	i <= stringSize
		and: [ 
			ascii := (aString at: i) asciiValue.
			ascii < 256
				ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
				ifFalse: [ true ] ] ]
		whileTrue: [ i := i + 1 ].

	i > stringSize
		ifTrue: [ ^ 0 ].
	^ i
%

!		Instance methods for 'CharacterCollection'

category: '*filesystem-core'
method: CharacterCollection
asFileReference

	^ FileSystem disk referenceTo: self
%

category: '*filesystem-core'
method: CharacterCollection
asPath
	"convert myself to a path"
	"Examples:
		'.' asPath
		'~/Desktop' asPath
		'/home/foo' asPath
		'../../foo.bar' asPath"
	^ FileSystem disk resolve: self
%

category: '*filesystem-core'
method: CharacterCollection
asPathWith: anObject 
	^ anObject pathFromString: self
%

category: '*filesystem-core'
method: CharacterCollection
asResolvedBy: aFileSystem
	^ aFileSystem resolveString: self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
asRwGemStoneVersionNumber

	^ RwGemStoneVersionNumber fromString: self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
asRwSemanticVersionNumber

	^ RwSemanticVersionNumber fromString: self
%

category: '*rowan-gemstone-url'
method: CharacterCollection
asRwUrl

	""

	^ RwUrl fromString: self
%

category: '*filesystem-gemstone-kernel'
method: CharacterCollection
asZnCharacterEncoder
	"Return a ZnCharacterEncoder instance using the receiver as identifier"
	
	" 'UTF-8' asZnCharacterEncoder "
	
	((self select: [ :each | each isAlphaNumeric ]) asLowercase) = 'utf8' ifFalse: [ self error: 'Only utf8 encoding supported'].
	^ ZnUTF8Encoder new
%

category: '*filesystem-gemstone-kernel'
method: CharacterCollection
encodeWith: encoding
	"Produce a ByteArray that encodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	" 'Les élèves français' encodeWith: #utf8 "
	
	^ encoding asZnCharacterEncoder encodeString: self
%

category: '*rowan-gemstone-url'
method: CharacterCollection
indexOfAnyOf: specialChars startingAt: oldPos

	oldPos to: self size do: [ :i | 
		(specialChars includes: (self at: i))
			ifTrue: [ ^ i ] ].
	^ 0
%

category: '*ston-gemstonebase'
method: CharacterCollection
isString
  ^ true
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher

	^ anRwGemStoneVersionConfigurationPlatformAttributeMatcher matchString: self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher

	^ anRwStringConfigurationPlatformAttributeMatcher matchString: self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwSemanticIntegerLessThanSelf: anInteger

	"integers have greater precedence than strings"
	"anInteger < aString-> true"

  ^ true
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwSemanticStringLessThanSelf: aString

	^ aString < self
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent

	^ aRwSemanticVersonComponent rwSemanticStringLessThanSelf: self
%

category: '*ston-gemstonecommon'
method: CharacterCollection
stonContainSubObjects
  ^ false
%

category: '*ston-gemstonecommon'
method: CharacterCollection
stonOn: stonWriter

        self isSymbol
                ifTrue: [stonWriter writeSymbol: self]
                ifFalse: [stonWriter writeString: self]
%

category: '*rowan-gemstone-components-kernel'
method: CharacterCollection
substrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result sourceStream subStringStream |
	
	(separators isString or: [ separators allSatisfy: [ :element | element isCharacter ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	sourceStream := self readStream.
	result := OrderedCollection new.
	subStringStream := String new writeStreamPortable.
	[ sourceStream atEnd ] whileFalse: [
		| char |
		char := sourceStream next.
		(separators includes: char)
			ifTrue: [
				subStringStream isEmpty ifFalse: [
					result add: subStringStream contents.
					subStringStream := String new writeStreamPortable ] ]
			ifFalse: [
				subStringStream nextPut: char ] ].
	subStringStream isEmpty ifFalse: [
		result add: subStringStream contents ].
	^ result asArray
%

category: '*rowan-gemstone-url'
method: CharacterCollection
unescapePercents

	"change each %XY substring to the character with ASCII value XY in hex.  This is the opposite of #encodeForHTTP"

	| ans c asciiVal pos oldPos specialChars |
	ans _ WriteStream on: String new.
	oldPos _ 1.
	specialChars _ '+%'.

	[ 
	pos _ self indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0 ]
		whileTrue: [ 
			ans nextPutAll: (self copyFrom: oldPos to: pos - 1).
			c _ self at: pos.
			c = $+
				ifTrue: [ ans nextPut: $  ]
				ifFalse: [ 
					(c = $% and: [ pos + 2 <= self size ])
						ifTrue: [ 
							asciiVal _ ((self at: pos + 1) asUppercase digitValueInRadix: 16) * 16
								+ ((self at: pos + 2) asUppercase digitValueInRadix: 16).
							pos _ pos + 2.
							asciiVal > 255
								ifTrue: [ ^ self ].	"not really an escaped string"
							ans nextPut: (Character value: asciiVal) ]
						ifFalse: [ ans nextPut: c ] ].
			oldPos _ pos + 1 ].
	ans nextPutAll: (self copyFrom: oldPos to: self size).
	^ ans contents
%

! Class extensions for 'Class'

!		Instance methods for 'Class'

category: '*ston-core'
method: Class
stonName
	"Override to encode my instances using a different class name."
	
	^ self name
%

! Class extensions for 'Collection'

!		Class methods for 'Collection'

category: '*ston-core'
classmethod: Collection
fromSton: stonReader
	| collection |
	collection := self new.
	stonReader parseListDo: [ :each |
		collection add: each ].
	^ collection
%

!		Instance methods for 'Collection'

category: '*filesystem-gemstone-kernel'
method: Collection
difference: aCollection
  "Answer the set theoretic difference of two collections."

  ^ self reject: [ :each | aCollection includes: each ]
%

category: '*filesystem-gemstone-kernel'
method: Collection
ifEmpty: aBlock
  self size == 0
    ifTrue: [ ^ aBlock value ]
%

category: '*filesystem-gemstone-kernel'
method: Collection
isEmptyOrNil
  "Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

  ^ self size == 0
%

category: '*filesystem-gemstone-kernel'
method: Collection
sort

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]
%

category: '*rowan-gemstone-components-kernel'
method: Collection
sort: aSortBlock

	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	^ self sortWithBlock: aSortBlock
%

category: '*ston-core'
method: Collection
stonOn: stonWriter
	stonWriter writeObject: self do: [
		stonWriter encodeList: self ]
%

! Class extensions for 'CollisionBucket'

!		Instance methods for 'CollisionBucket'

category: '*ston-gemstonecommon'
method: CollisionBucket
stonContainSubObjects 
	^false
%

! Class extensions for 'Date'

!		Class methods for 'Date'

category: '*ston-gemstonecommon'
classmethod: Date
fromSton: stonReader

	^ self fromStream: stonReader parseListSingleton readStream usingFormat: #(3 2 1 $- 1 1)
%

!		Instance methods for 'Date'

category: '*ston-core'
method: Date
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: Date
stonOn: stonWriter
  "Use an ISO style YYYYMMDD representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #(3 2 1 $- 1 1 $: false))
%

! Class extensions for 'DateAndTime'

!		Class methods for 'DateAndTime'

category: '*ston-core'
classmethod: DateAndTime
fromSton: stonReader
  ^ DateAndTime fromString: stonReader parseListSingleton
%

!		Instance methods for 'DateAndTime'

category: '*ston-core'
method: DateAndTime
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: DateAndTime
stonOn: stonWriter
	"Use an ISO representation with all details"
	
	stonWriter writeObject: self listSingleton: 
		(String streamContents: [ :stream |
			self printOn: stream ])
%

! Class extensions for 'DiskFileSystemTest'

!		Instance methods for 'DiskFileSystemTest'

category: '*filesystem-tests-gemstone'
method: DiskFileSystemTest
testGsDeleteDirectoryOnError

	| path |
	path := Path * 'plonk' / 'griffle'.
	self markForCleanup: path.
	self markForCleanup: path parent.
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: Path * 'plonk').
	self assert: (filesystem isDirectory: path).

	self should: [ filesystem delete: path parent ] raise: Error.
%

! Class extensions for 'FileLocator'

!		Class methods for 'FileLocator'

category: '*rowan-components-kernel'
classmethod: FileLocator
rowanProjectsHome
	"Answer the path to $ROWAN_PROJECTS_HOME"

	^ self origin: #rowanProjectsHome
%

! Class extensions for 'FileStreamPortable'

!		Class methods for 'FileStreamPortable'

category: '*FileSystem-Core'
classmethod: FileStreamPortable
onHandle: aFileSystemHandle
	^ self concreteStream new
		open: aFileSystemHandle fullName
		forWrite: aFileSystemHandle isWritable
%

! Class extensions for 'FileSystem'

!		Class methods for 'FileSystem'

category: '*filesystem-disk'
classmethod: FileSystem
* aFileOrDirectoryName
	^ self disk * aFileOrDirectoryName
%

category: '*filesystem-disk'
classmethod: FileSystem
/ aFileOrDirectoryName
	^ self disk / aFileOrDirectoryName
%

category: '*FileSystem-Memory'
classmethod: FileSystem
currentMemoryFileSystem
	^ MemoryStore currentFileSystem
%

category: '*filesystem-disk'
classmethod: FileSystem
disk
	"Answer a filesystem that represents the 'on-disk' filesystem used by the host operating system."

	^ DiskStore currentFileSystem
%

category: '*FileSystem-Memory'
classmethod: FileSystem
memory
	^ self store: MemoryStore new
%

category: '*filesystem-disk'
classmethod: FileSystem
root
	^ self disk root
%

category: '*filesystem-disk'
classmethod: FileSystem
workingDirectory
	^ self disk workingDirectory
%

!		Instance methods for 'FileSystem'

category: '*FileSystem-Disk'
method: FileSystem
isDiskFileSystem
	^ store isDiskFileSystem
%

category: '*FileSystem-Memory'
method: FileSystem
isMemoryFileSystem
	^ store isMemoryFileSystem
%

! Class extensions for 'FileSystemStore'

!		Instance methods for 'FileSystemStore'

category: '*FileSystem-Disk'
method: FileSystemStore
isDiskFileSystem
	^ false
%

category: '*FileSystem-Memory'
method: FileSystemStore
isMemoryFileSystem
	^ false
%

! Class extensions for 'GsFile'

!		Class methods for 'GsFile'

category: '*filesystem-gemstone-kernel-35x'
classmethod: GsFile
_contentsOfServerDirectory: aPathName expandPath: aBoolean

	^ self _contentsOfServerDirectory: aPathName expandPath: aBoolean utf8Results: false
%

! Class extensions for 'Integer'

!		Instance methods for 'Integer'

category: '*filesystem-gemstone-kernel'
method: Integer
<< shiftAmount
	"left shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount
%

category: '*filesystem-gemstone-kernel'
method: Integer
>> shiftAmount
	"right shift"
	
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: 0 - shiftAmount
%

category: '*filesystem-gemstone-kernel'
method: Integer
digitAt: n
	"Answer the value of an apparent byte-indexable field in the receiver,
	 analogous to the large integers, which are organized as bytes."

	n = 1
		ifTrue: [ 
			"Negate carefully in case the receiver is SmallInteger minVal"
			^ self < 0
				ifTrue: [ -256 - self bitAnd: 255 ]
				ifFalse: [ self bitAnd: 255 ] ].
	^ self < 0
		ifTrue: [ (-256 - self bitShift: -8) + 1 digitAt: n - 1 ]
		ifFalse: [ (self bitShift: 8 - (n bitShift: 3)) bitAnd: 255 ]
%

category: '*FileSystem-Core'
method: Integer
humanReadableSIByteSize
	^ String streamContents: [ :s|
		self humanReadableSIByteSizeOn: s ]
%

category: '*FileSystem-Core'
method: Integer
humanReadableSIByteSizeOn: s
	| exponent base |
	"Print a string with an SI binary unit represation of myself."
	base := 1000.
	self < base
		ifTrue: [ ^ s print: self; space; nextPut: $B ].
	exponent := (self log / base log) asInteger.
	(self / (base ** exponent)) printOn: s showingDecimalPlaces: 2.
	s 
		space;
		nextPut: ('kMGTPE' at: exponent);
		nextPut: $B.
%

category: '*rowan-gemstone-components-kernel'
method: Integer
rwSemanticIntegerLessThanSelf: anInteger

	^ anInteger < self
%

category: '*rowan-gemstone-components-kernel'
method: Integer
rwSemanticStringLessThanSelf:  aString
  "integers have greater precedence than strings"
	" aString < anInteger -> false"

  ^ false
%

category: '*rowan-gemstone-components-kernel'
method: Integer
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent

	^ aRwSemanticVersonComponent rwSemanticIntegerLessThanSelf: self
%

category: '*ston-core'
method: Integer
stonOn: stonWriter
	stonWriter writeInteger: self
%

! Class extensions for 'Number'

!		Instance methods for 'Number'

category: '*ston-tests'
method: Number
closeTo: num
  "are these two numbers close?"

  num _isFloat
    ifTrue: [ ^ num closeTo: self asFloat ].
  ^ [ self = num ] on: Error do: [:ignored |  false ]
%

category: '*ston-core'
method: Number
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: Number
stonOn: stonWriter
	stonWriter writeFloat: self asFloat
%

! Class extensions for 'Object'

!		Class methods for 'Object'

category: '*ston-core'
classmethod: Object
fromSton: stonReader
	"Create a new instance and delegate decoding to instance side.
	Override only when new instance should be created directly (see implementors). "
	
	^ self new
		fromSton: stonReader;
		yourself
%

!		Instance methods for 'Object'

category: '*filesystem-gemstone-kernel'
method: Object
flag: aSymbol

	"Send this message, with a relevant symbol as argument, to flag a message for subsequent retrieval.  For example, you might put the following line in a number of messages:
	self flag: #returnHereUrgently
	Then, to retrieve all such messages, browse all senders of #returnHereUrgently."
%

category: '*ston-core'
method: Object
fromSton: stonReader
  "Decode non-variable classes from a map of their instance variables and values.
	Override to customize and add a matching #toSton: (see implementors)."

  self class isVariable
    ifTrue: [ self subclassResponsibility ]
    ifFalse: [ | instanceVariableNames |
      instanceVariableNames := self class allInstVarNames.
      stonReader
        parseMapDo: [ :instVarName :value | self instVarAt: (instanceVariableNames indexOf: instVarName asSymbol) put: value ] ]
%

category: '*filesystem-gemstone-kernel'
method: Object
isCharacter

	^ false
%

category: '*ston-gemstonebase'
method: Object
isNumber
  ^ self _isNumber
%

category: '*ston-core'
method: Object
isStonReference
	^ false
%

category: '*ston-gemstonebase'
method: Object
isString
  ^ false
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher
  ^ self
    error: 'Expected a String or a RwGemStoneVersion'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwSemanticIntegerLessThanSelf: anInteger
  ^ self
    error: 'Invalid semantic verson component - should be an Integer.'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwSemanticStringLessThanSelf: aString
  ^ self
    error: 'Invalid semantic verson component - should be String.'
%

category: '*rowan-gemstone-components-kernel'
method: Object
rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent
  ^ self
    error: 'Invalid semantic verson component - should be String or Integer.'
%

category: '*filesystem-gemstone-kernel'
method: Object
split: aSequenceableCollection
	"Split the argument using the receiver as a separator."
	"optimized version for single delimiters"
	"($/ split: '/foo/bar')>>>#('' 'foo' 'bar') asOrderedCollection"
	"([:c| c isSeparator] split: 'aa bb cc dd')>>> #('aa' 'bb' 'cc' 'dd') asOrderedCollection"
		
	| result |
	result := OrderedCollection new: (aSequenceableCollection size / 2) asInteger.
	self split: aSequenceableCollection do: [ :item |
		result add: item ].
	^ result
%

category: '*filesystem-gemstone-kernel'
method: Object
split: aSequenceableCollection do: aBlock
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar' indicesDo: [ :item | ]"
	self split: aSequenceableCollection indicesDo: [ :start :end | 
		aBlock value: (aSequenceableCollection copyFrom: start to: end) ]
%

category: '*filesystem-gemstone-kernel'
method: Object
split: aSequenceableCollection indicesDo: aBlock
	"Perform an action specified as aBlock (with a start and end argument) to each of the indices of the receiver element that have been identified by splitting the receiver using the splitter argument. optimized version for single delimiters."
	
	"(String streamContents: [:s | Character space split: 'Pharo is cool'  indicesDo: [ :start :end | s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) >>> 's:1 e:5 s:7 e:8 s:10 e:13 '"
		
		
		
	|  position oldPosition |
	
	position := 1.
	oldPosition := position.
	
	position := aSequenceableCollection indexOf: self startingAt: position.
	[ position > 0 ] whileTrue: [
		aBlock value: oldPosition value: position - 1.
		position := position + 1.
		oldPosition := position.
		position := aSequenceableCollection indexOf: self startingAt: position.
	].

	aBlock value: oldPosition value: aSequenceableCollection size.
%

category: '*ston-core'
method: Object
stonContainSubObjects
	"Return true if I contain subObjects that should be processed, false otherwise.
	Overwrite when necessary. See also #stonProcessSubObjects:"
	
	^ true
%

category: '*ston-core'
method: Object
stonOn: stonWriter
	"Encode non-variable classes with a map of their instance variable and values.
	Override to customize and add a matching #fromSton: (see implementors)."

	self class isVariable 
		ifTrue: [
			self subclassResponsibility ]
		ifFalse: [
			stonWriter writeObject: self ]
%

category: '*ston-gemstonecommon'
method: Object
stonProcessSubObjects: block
  "Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."

  1 to: self class instSize do: [ :each | self instVarAt: each put: (block value: (self instVarAt: each)) ].
  (self class isVariable and: [ self class isBytes not ])
    ifTrue: [ 1 to: self _basicSize do: [ :each | self basicAt: each put: (block value: (self basicAt: each)) ] ]
%

category: '*ston-core'
method: Object
stonShouldWriteNilInstVars
	"Return true if my instance variables that are nil should be written out, 
	false otherwise. Overwrite when necessary. By default, return false."
	
	^ false
%

! Class extensions for 'Path'

!		Instance methods for 'Path'

category: '*FileSystem-Core'
method: Path
asFileReference
	^ FileSystem disk referenceTo: self
%

category: '*FileSystem-Core'
method: Path
relativeToReference: aReference 
	^ self relativeToPath: aReference path
%

! Class extensions for 'PositionableStreamPortable'

!		Instance methods for 'PositionableStreamPortable'

category: '*filesystem-gemstone-kernel'
method: PositionableStreamPortable
isBinary
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray
%

category: '*filesystem-gemstone-kernel'
method: PositionableStreamPortable
nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
%

! Class extensions for 'ReadStreamPortable'

!		Instance methods for 'ReadStreamPortable'

category: '*filesystem-gemstone-kernel'
method: ReadStreamPortable
readInto: aCollection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + max - 1
		with: collection
		startingAt: position + 1.
	position := position + max.
	^ max
%

! Class extensions for 'ReadWriteStreamPortable'

!		Instance methods for 'ReadWriteStreamPortable'

category: '*filesystem-gemstone-kernel'
method: ReadWriteStreamPortable
readInto: aCollection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex + max - 1
		with: collection
		startingAt: position + 1.
	position := position + max.
	^ max
%

! Class extensions for 'RwAbstractProjectComponentVisitorV2'

!		Instance methods for 'RwAbstractProjectComponentVisitorV2'

category: '*rowan-gsbase-componentsv2'
method: RwAbstractProjectComponentVisitorV2
_components: componentDirPath forProject: aProjectName
	| urlBase |
	self componentNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,componentDirPath asFileReference pathString, '/'.
	^ self componentNames
		collect: [ :componentName | 
			| url |
			url := urlBase , componentName , '.ston'.
			(RwAbstractProjectLoadComponentV2 fromUrl: url)
				projectName: aProjectName;
				yourself ]
%

category: '*rowan-gsbase-componentsv2'
method: RwAbstractProjectComponentVisitorV2
_projects: projectDirPath forProject: ignored

	| urlBase |
	self projectNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,projectDirPath asFileReference pathString, '/'.
	^ self projectNames
		collect: [ :prjName | 
			| url |
			url := urlBase , prjName , '.ston'.
			RwSpecification fromUrl: url ]
%

! Class extensions for 'RwAbstractProjectLoadComponentV2'

!		Class methods for 'RwAbstractProjectLoadComponentV2'

category: '*rowan-gsbase-componentsv2'
classmethod: RwAbstractProjectLoadComponentV2
fromFile: filePath
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ self _readStonFrom: stream ]
%

category: '*rowan-gsbase-componentsv2'
classmethod: RwAbstractProjectLoadComponentV2
orderedDictionaryClass

	^ GsTonelOrderedDictionary
%

!		Instance methods for 'RwAbstractProjectLoadComponentV2'

category: '*rowan-gsbase-componentsv2'
method: RwAbstractProjectLoadComponentV2
_exportToUrl: fileUrl
	self error: 'Not yet implemented'
%

! Class extensions for 'RwProjectComponentVisitorV2Test'

!		Instance methods for 'RwProjectComponentVisitorV2Test'

category: '*rowan-gsbase-componentsv2'
method: RwProjectComponentVisitorV2Test
_cloneRowanSample9: projectAlias
	"RowanSample9 is expected to be on branch spec_0008"
	^'$GS_HOME/shared/repos/RowanSample9'
%

category: '*rowan-gsbase-componentsv2'
method: RwProjectComponentVisitorV2Test
_cloneVastTonelDemo_555: projectAlias deleteClone: deleteClone
	"tonel-demos is expected to bo on branch rowan_553"
	^'$GS_HOME/shared/repos/tonel-demos'
%

! Class extensions for 'RwSpecification'

!		Class methods for 'RwSpecification'

category: '*rowan-gemstone-specifications'
classmethod: RwSpecification
fromFile: filePath
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ (STON fromStream: stream)
				initializeForImport;
				yourself ]
%

! Class extensions for 'SequenceableCollection'

!		Class methods for 'SequenceableCollection'

category: '*ston-core'
classmethod: SequenceableCollection
fromSton: stonReader
	^ self streamContents: [ :stream |
		stonReader parseListDo: [ :each |
			stream nextPut: each ] ]
%

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
new: newSize streamContents: blockWithArg
  | stream |
  stream := WriteStreamPortable on: (self new: newSize).
  blockWithArg value: stream.
  ^ stream contents
%

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
streamContents: blockWithArg
  ^ self new: 100 streamContents: blockWithArg
%

!		Instance methods for 'SequenceableCollection'

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
allButFirst
  "Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

  ^ self allButFirst: 1
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
allButFirst: n
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyAfterLast: anElement
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyLast: n
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyUpToLast: anElement
  "Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	| n |
	n :=  (self lastIndexOf: anElement ifAbsent: [ ^ self copy ]) - 1.
  ^ self copyFrom: 1 to: n
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
copyWithFirst: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver with newElement as the first element."

	| newIC |
	newIC := self species new: self size + 1.
	newIC 
		replaceFrom: 2
		to: self size + 1
		with: self
		startingAt: 1.
	newIC at: 1 put: newElement.
	^ newIC
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
grownBy: length
	"Answer a copy of receiver collection with size grown by length"

	| newCollection size |
	size := self size.
	newCollection := self species new: size + length.
	newCollection replaceFrom: 1 to: size with: self startingAt: 1.
	^ newCollection
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
lastIndexOf: anElement ifAbsent: exceptionBlock
  "Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

  ^ self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock
  "Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

  lastIndex to: 1 by: -1 do: [ :index | 
    (self at: index) = anElement
      ifTrue: [ ^ index ] ].
  ^ exceptionBlock ~~ nil
    ifTrue: [ exceptionBlock value ]
    ifFalse: [ 0 ]
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
readStreamPortable

	^ ReadStreamPortable on: self
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
replaceAll: oldObject with: newObject
  "Replace all occurences of oldObject with newObject"

  | index |
  index := self indexOf: oldObject startingAt: 1 ifAbsent: [ 0 ].
  [ index = 0 ]
    whileFalse: [ 
      self at: index put: newObject.
      index := self indexOf: oldObject startingAt: index + 1 ifAbsent: [ 0 ] ]
%

category: '*filesystem-gemstone-kernel'
method: SequenceableCollection
reversed
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n := self size.
	result := self species new: n.
	src := n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src := src - 1))].
	^ result
%

category: '*ston-core'
method: SequenceableCollection
stonOn: stonWriter
	self class == STON listClass
		ifTrue: [ stonWriter writeList: self ]
		ifFalse: [ super stonOn: stonWriter ]
%

category: '*rowan-gemstone-components-kernel'
method: SequenceableCollection
writeStreamPortable

	^ WriteStreamPortable on: self
%

! Class extensions for 'STONReader'

!		Class methods for 'STONReader'

category: '*ston-gemstonebase'
classmethod: STONReader
new
  ^ self basicNew
    initialize;
    yourself
%

!		Instance methods for 'STONReader'

category: '*ston-gemstonebase'
method: STONReader
lookupClass: name
  ^ (System myUserProfile objectNamed: name asSymbol)
    ifNil: [ 
		(((AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools')
			ifNotNil: [:rowanSymbolDictionary |
				(rowanSymbolDictionary at: name asSymbol ifAbsent: [])
					ifNotNil: [:cls | ^cls ] ])
						ifNil: [ classes at: name ifAbsentPut: [ (ClassOrganizer new allSubclassesOf: Object)
								detect: [ :cls | cls stonName == name ]
								ifNone: [
									(((AllUsers userWithId: 'SystemUser') objectNamed: 'Rowan') 
										platform serviceClassFor: name)
											ifNil: [ self error: 'Cannot resolve class named ' , name printString ] ] ] ] ]
%

category: '*ston-gemstonecommon'
method: STONReader
optimizeForLargeStructures
  "nothing special for GemStone"

%

! Class extensions for 'STONStreamWriter'

!		Class methods for 'STONStreamWriter'

category: '*ston-gemstonebase'
classmethod: STONStreamWriter
new
  ^ self basicNew
    initialize;
    yourself
%

! Class extensions for 'STONWriter'

!		Class methods for 'STONWriter'

category: '*ston-gemstonecommon'
classmethod: STONWriter
findFirstInString: aString inSet: inclusionMap startingAt: start
  "Trivial, non-primitive version"

  | i stringSize ascii |
  inclusionMap size ~= 256
    ifTrue: [ ^ 0 ].
  i := start.
  stringSize := aString size.
  [ i <= stringSize and: [ ascii := (aString at: i) asciiValue.
      ascii < 256
        ifTrue: [ (inclusionMap at: ascii + 1) = 0 ]
        ifFalse: [ true ] ] ] whileTrue: [ i := i + 1 ].
  i > stringSize
    ifTrue: [ ^ 0 ].
  ^ i
%

category: '*ston-gemstonebase'
classmethod: STONWriter
new
  ^ self basicNew
    initialize;
    yourself
%

!		Instance methods for 'STONWriter'

category: '*ston-gemstonecommon'
method: STONWriter
encodeCharacter: char
  | code encoding |
  ((code := char codePoint) < 127
    and: [ (encoding := STONCharacters at: code + 1) notNil ])
    ifTrue: [ encoding = #'pass'
        ifTrue: [ writeStream nextPut: char ]
        ifFalse: [ writeStream nextPutAll: encoding ] ]
    ifFalse: [ | paddedStream padding digits |
      paddedStream := WriteStream on: String new.
      code printOn: paddedStream base: 16 showRadix: false.
      digits := paddedStream contents.
      padding := 4 - digits size.
      writeStream nextPutAll: '\u'.
      encoding := padding > 0
        ifTrue: [ ((String new: padding)
            atAllPut: $0;
            yourself) , digits ]
        ifFalse: [ digits ].
      writeStream nextPutAll: encoding ]
%

category: '*ston-gemstonecommon'
method: STONWriter
isSimpleSymbol: symbol
  symbol isEmpty
    ifTrue: [ ^ false ].
  ^ (self class
    findFirstInString: symbol
    inSet: STONSimpleSymbolCharacters
    startingAt: 1) = 0
%

category: '*ston-gemstonecommon'
method: STONWriter
optimizeForLargeStructures
  "nothing special for GemStone"

%

category: '*ston-gemstonebase'
method: STONWriter
writeFloat: float
  writeStream nextPutAll: float asString
%

! Class extensions for 'STONWriteReadTests'

!		Instance methods for 'STONWriteReadTests'

category: '*ston-gemstone-tests'
method: STONWriteReadTests
testGemStoneCollections
  | collections |
  collections := STON listClass
    withAll:
      {(KeyValueDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself)}.
  self serializeAndMaterialize: collections
%

! Class extensions for 'Stream'

!		Instance methods for 'Stream'

category: '*filesystem-gemstone-kernel'
method: Stream
isBinary
	^false
%

category: '*filesystem-gemstone-kernel'
method: Stream
wrappedStreamName

	^''
%

! Class extensions for 'String'

!		Instance methods for 'String'

category: '*ston-core'
method: String
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: String
stonOn: stonWriter
	stonWriter writeString: self
%

! Class extensions for 'Symbol'

!		Instance methods for 'Symbol'

category: '*ston-core'
method: Symbol
stonOn: stonWriter
	stonWriter writeSymbol: self
%

! Class extensions for 'TestAsserter'

!		Instance methods for 'TestAsserter'

category: '*filesystem-gemstone-kernel'
method: TestAsserter
assertCollection: actual equals: expected
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: expected = actual
		description: [ self comparingCollectionBetween: actual and: expected ]
%

category: '*filesystem-gemstone-kernel'
method: TestAsserter
comparingCollectionBetween: left and: right
	| additionalLeft additionalRight sortBlock|
	
	"use a very slow sort block"
	sortBlock := [ :a :b | a asString <= b asString ].
	additionalLeft := (left difference: right) sort: sortBlock.
	additionalRight := (right difference: left) sort: sortBlock. 
	
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Given Collections do not match. Got '; lf;
			tab; nextPutAll: 'left := '; print: left; nextPut: $.; lf;
			nextPutAll: ' instead of ';
			tab; nextPutAll: ' right :='; print: right; nextPut: $.; lf.
		left size = right size
			ifFalse: [ 
				stream 
					nextPutAll: 'Collection size does not match: left='; 
					print: left size;
					nextPutAll: ' vs. right=';
					print: right size; lf ].
		additionalLeft isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalLeft size;
					nextPutAll: ' additional element(s) in the left collection: ';
					tab; print: additionalLeft  ].
		additionalRight isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalRight size;
					nextPutAll: ' additional element(s) in the right collection: ';
					tab; print: additionalRight  ]]
%

! Class extensions for 'Time'

!		Class methods for 'Time'

category: '*ston-gemstonecommon'
classmethod: Time
fromSton: stonReader
  ^ self fromString: stonReader parseListSingleton usingFormat: #($: true false)
%

!		Instance methods for 'Time'

category: '*ston-core'
method: Time
stonContainSubObjects 
	^ false
%

category: '*ston-gemstonecommon'
method: Time
stonOn: stonWriter
  "Use an ISO style HH:MM:SS representation"

  stonWriter
    writeObject: self
    listSingleton: (self asStringUsingFormat: #($: true false))
%

! Class extensions for 'UndefinedObject'

!		Instance methods for 'UndefinedObject'

category: '*filesystem-gemstone-kernel'
method: UndefinedObject
isEmptyOrNil
  "Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

  ^ true
%

category: '*ston-core'
method: UndefinedObject
stonContainSubObjects 
	^ false
%

category: '*ston-core'
method: UndefinedObject
stonOn: stonWriter
	stonWriter writeNull
%

! Class extensions for 'UnorderedCollection'

!		Instance methods for 'UnorderedCollection'

category: '*ston-gemstonecommon'
method: UnorderedCollection
stonProcessSubObjects: block
	"Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."
"increase the starting index by 4 because of the private inst vars in UnorderedCollection"

	5 to: self class instSize do: [ :each |
		self instVarAt: each  put: (block value: (self instVarAt: each)) ].
	(self class isVariable and: [ self class isBytes not ])
		ifTrue: [
			1 to: self _basicSize do: [ :each |
				self basicAt: each put: (block value: (self basicAt: each)) ] ]
%

! Class extensions for 'Utf8'

!		Instance methods for 'Utf8'

category: '*filesystem-gemstone-kernel'
method: Utf8
asByteArray
	^ ByteArray streamContents: [ :stream |
		self do: [ :each |
			stream nextPut: each ] ]
%

category: '*filesystem-gemstone-kernel'
method: Utf8
asString
  "override the *filesystem  ByteArray >> asString"
  ^ self decodeToString   "or maybe  decodeToUnicode ??"
%

! Class extensions for 'ZnBufferedReadStream'

!		Instance methods for 'ZnBufferedReadStream'

category: '*rowan-components-kernel'
method: ZnBufferedReadStream
buffer

	^ buffer
%

! Class Initialization

run
FastUUIDGenerator initialize.
STONWriter initialize.
true
%
