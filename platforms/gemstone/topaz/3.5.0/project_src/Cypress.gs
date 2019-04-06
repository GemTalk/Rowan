! Class Declarations

doit
(Error
	subclass: 'CypressError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'CypressJsonError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Structure';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'CypressLoaderError'
	instVarNames: #( exception patchOperation )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

CypressLoaderError is used to report a failure applying a specific CypressPatchOperation.
The CypressLoader made a first attempt to apply the Patch Operation and reported a 
CypressLoaderErrorNotification, set aside the Patch Operation, and has retried it after applying
all other Patch Operations.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
';
		immediateInvariant.
true.
%

doit
(Error
	subclass: 'CypressLoaderMissingClasses'
	instVarNames: #( requirementsMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Notification
	subclass: 'CypressLoaderErrorNotification'
	instVarNames: #( exception patchOperation )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

CypressLoaderErrorNotification is used to notify a consumer of the CypressLoader that a particular CypressPatchOperation failed.
As a Notification, it resumes by default, logging the error to the Transcript.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressAbstractPackageFiler'
	instVarNames: #( repository packageDirectory packageStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageFiler
	subclass: 'CypressAbstractFileoutWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileoutWriter
	subclass: 'CypressSmalltalkFileoutWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileoutWriter
	subclass: 'CypressTopazFileoutWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageFiler
	subclass: 'CypressAbstractPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageReader
	subclass: 'CypressDoNothingPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageReader
	subclass: 'CypressFileTreeFormatPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageReader
	subclass: 'CypressFlexiblePackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageReader
	subclass: 'CypressPackageReader'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageFiler
	subclass: 'CypressAbstractPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #( specials )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageWriter
	subclass: 'CypressFileTreeFormatPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageWriter
	subclass: 'CypressPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageWriter
	subclass: 'CypressStrictFileTreeFormatDoNothingPackageWriter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressAbstractRepository'
	instVarNames: #( url properties readerClass writerClass )
	classVars: #( DefaultCopyrightNotice )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractRepository
	subclass: 'CypressAbstractFileoutRepository'
	instVarNames: #( directoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileoutRepository
	subclass: 'CypressSmalltalkRepository'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

This is a "write-only" repository.
It could be made readable, to be able to file-in Smalltalk scripts, but it''s not the same thing.
';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileoutRepository
	subclass: 'CypressTopazRepository'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

This is a "write-only" repository.
It could be made readable, to be able to file-in Topaz scripts, but it''s not the same thing.
';
		immediateInvariant.
true.
%

doit
(CypressAbstractRepository
	subclass: 'CypressDictionaryRepository'
	instVarNames: #( dictionary )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractRepository
	subclass: 'CypressFileSystemRepository'
	instVarNames: #( directoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressFileSystemRepository
	subclass: 'CypressFileSystemGitRepository'
	instVarNames: #( remoteUrl )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressFileUtilities'
	instVarNames: #(  )
	classVars: #( Current )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressFileUtilities
	subclass: 'CypressGemStoneDirectoryUtilities'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-GemStoneFileServer';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressGsGeneralDependencySorter'
	instVarNames: #( candidates dependsOnConverter dependentConverter individualDependencyMap dependencyGraphs candidateAliasMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Structure';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressJsonParser'
	instVarNames: #( stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Structure';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressObject'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressAbstractPackageInformation'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressConflictingPackageInformation'
	instVarNames: #( conflictsWith )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressEclipsedPackageInformation'
	instVarNames: #( eclipsedBy )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressKnownPackageInformation'
	instVarNames: #( repositories digests )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressUnknownPackageInformation'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressDefinition'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressDefinition
	subclass: 'CypressClassDefinition'
	instVarNames: #( category classInstVarNames classVarNames comment defaultSymbolDictionaryName instVarNames name poolDictionaryNames subclassType superclassName gs_options gs_constraints )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressDefinition
	subclass: 'CypressMethodDefinition'
	instVarNames: #( category classIsMeta className selector source )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressDefinitionIndex'
	instVarNames: #( definitionMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressDependencySorter'
	instVarNames: #( orderedItems provided required )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressDependencySorter
	subclass: 'CypressEnvironmentDependencySorter'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Environmental-Tools';
		comment: '';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressLoader'
	instVarNames: #( additions defaultSymbolDictionaryName errors exceptionClass methodAdditions obsoletions provisions removals requirements unloadable )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressLoader
	subclass: 'CypressEnvironmentLoader'
	instVarNames: #( defaultEnvironmentId lookupSymbolList compilationSymbolList )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Environmental-Tools';
		comment: '';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressPackageDefinition'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressPackageDefinition
	subclass: 'CypressEnvironmentPackageDefinition'
	instVarNames: #( lookupSymbolList )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Environmental-Tools';
		comment: 'No class-specific documentation for CypressEnvironmentPackageDefinition, hierarchy is: 
Object
  CypressObject
    CypressPackageDefinition( name)
      CypressEnvironmentPackageDefinition( lookupSymbolList)
';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressPackageInformation'
	instVarNames: #( name type advice competingPackageNames imageDefinitions savedDefinitions savedLocation repository repositoryDescription imageCounts changesCount )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

CypressPackageInformation documents potential and actual packages for the Cypress Package Manager. 

Candidate package names come from class categories and from method categories beginning with an asterisk. Category names may contain more information than just the package name, such as logical subdivisions within a package or method categorization in addition to the package name. For example, a Package might be named X or Y-Z or whatever. Classes could be categorized as X, Y-Z, X-A, or Y-Z-A, and methods could be categorized as *X, *Y-Z, *X-A, *Y-Z-A, etc. (The various letters X, Y, Z, and A can represent almost any sequence of characters, in either uppercase, lowercase, or both. Package names are case-insensitive.)

There are four types of CypressPackageInformation objects:
 - Known Package - those which are known to represent real packages (e.g., Y-Z). In general, it is because there is a savedLocation specified.
 - Qualified Name - the name is a Known Package name qualified by further details, and cannot be used to represent a Known Package (e.g., X-accessing).
 - Conflicted Name - the name is a prefix of a Known Package name (e.g. given a Known Package named Y-Z, there can be no package named Y).
 - Unknown - the name could represent a package, but it is not known to do so.

Instance Variables
	advice	<String>	Additional information about the type of the instance, usually used only for Qualified Names and Conflcited Names.
	changesCount	<Integer>	The number of differences between the in-image definitions of the package and the definitions previously saved to disk.
	competingPackageNames	<String>*	0 or more strings naming packages in competition with this one.
	imageCounts	<Integer pair>	The number of classes and the number of methods in the image for the package.
	name	<String>	The name of the package or potential package.
	savedLocation	<String>	The path to the directory in which the package was or should be saved, with a trailing slash (e.g., /usr/src/project/).
	type	<String>	One of ''Known Package'', ''Qualified Name'', ''Conflicted Name'', and ''Unknown''.
	imageDefinitions	<CypressDefinition>*	0 or more definitions from the image.
	savedDefinitions	<CypressDefinition>*	0 or more definitions from the savedLocation storage.

';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressPatch'
	instVarNames: #( operations )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressPatchOperation'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressPatchOperation
	subclass: 'CypressAddition'
	instVarNames: #( definition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressPatchOperation
	subclass: 'CypressModification'
	instVarNames: #( modification obsoletion )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressPatchOperation
	subclass: 'CypressRemoval'
	instVarNames: #( definition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressSnapshot'
	instVarNames: #( definitions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Definitions';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressObject
	subclass: 'CypressStructure'
	instVarNames: #( name properties packageStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Structure';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressStructure
	subclass: 'CypressClassStructure'
	instVarNames: #( instanceMethods classMethods comment isClassExtension )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Structure';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressStructure
	subclass: 'CypressMethodStructure'
	instVarNames: #( source isMetaclass classStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Structure';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressStructure
	subclass: 'CypressPackageStructure'
	instVarNames: #( classes extensions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Structure';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressPackageComparator'
	instVarNames: #( directoryPackageMap diskTimestamps diskSnapshots imageSnapshots snapshotDifferences currentPackageName currentAdditions currentRemovals )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Comparison';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressPackageManager'
	instVarNames: #( knownPackages knownRepositories packageInformationList )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressPackageManager2'
	instVarNames: #( knownRepositories packageInformationList )
	classVars: #( SavedPackageManagers )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressPackageManager3'
	instVarNames: #( knownRepositories defaultSymbolDictionaryName resolvedPackageReferences )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressPackageManager3
	subclass: 'CypressEnvironmentPackageManager'
	instVarNames: #( defaultSymbolList lookupSymbolList compilationSymbolList defaultEnvironmentId )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Environmental-Tools';
		comment: '';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressPackageStringComparator'
	instVarNames: #( directoryPackageMap diskTimestamps diskSnapshots imageSnapshots snapshotDifferences currentPackageName currentOperations )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Comparison';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressReference'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

A CypressReference is an abstract superclass for various kinds of references to Cypress packages. Inspired by GoferReference in Pharo';
		immediateInvariant.
true.
%

doit
(CypressReference
	subclass: 'CypressPackageReference'
	instVarNames: #( package branch )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

A CypressPackageReference refers to a specific Cypress package.';
		immediateInvariant.
true.
%

doit
(CypressPackageReference
	subclass: 'CypressResolvedReference'
	instVarNames: #( repository )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

A CypressResolvedReference refers to a specific Cypress package in a repository. This class is the only one that can actually load a package, because it is the only one knowing where to find it.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressUrl'
	instVarNames: #( fragment )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.';
		immediateInvariant.
true.
%

doit
(CypressUrl
	subclass: 'CypressFileUrl'
	instVarNames: #( host path isAbsolute )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

This class models a file URL according to (somewhat) RFC1738, see http://www.w3.org/Addressing/rfc1738.txt

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
(CypressFileUrl
	subclass: 'CypressAbstractFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressCypressFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressFileTreeFormatFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressFileTreeReadOnlyFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressGitFileUrl'
	instVarNames: #( projectPath projectBranchOrTag repositoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressGitFileUrl
	subclass: 'CypressGitFileTreeUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressLaxFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressSmalltalkUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressTopazUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressUrl
	subclass: 'CypressGenericUrl'
	instVarNames: #( schemeName locator )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

a URL type that can''t be broken down in any systematic way.  For example, mailto: and telnet: URLs.  The part after the scheme name is stored available via the #locator message.';
		immediateInvariant.
true.
%

doit
(CypressGenericUrl
	subclass: 'CypressBrowserUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

URLs that instruct a browser to do something.';
		immediateInvariant.
true.
%

doit
(CypressGenericUrl
	subclass: 'CypressMailtoUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

a URL specifying a mailing address; activating it triggers a mail-sender to start up, if one is present.';
		immediateInvariant.
true.
%

doit
(CypressUrl
	subclass: 'CypressHierarchicalUrl'
	instVarNames: #( schemeName authority path query port username password )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

A URL which has a hierarchical encoding.  For instance, http and ftp URLs are hierarchical.';
		immediateInvariant.
true.
%

doit
(CypressHierarchicalUrl
	subclass: 'CypressFtpUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressHierarchicalUrl
	subclass: 'CypressHttpUrl'
	instVarNames: #( realm )
	classVars: #( Passwords )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

A URL that can be accessed via the Hypertext Transfer Protocol (HTTP), ie, a standard Web URL

realm = the name of the security realm that has been discovered for this URL.   Look it up in Passwords.

Passwords = a Dictionary of (realm -> encoded user&password)


TODO: use the username and password, if specified
';
		immediateInvariant.
true.
%

doit
(CypressHttpUrl
	subclass: 'CypressHttpsUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Network-Url';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'CypressVersionReference'
	instVarNames: #( name package author branch versionNumber )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.

A CypressVersionReference refers to a specific version of a Monticello package.';
		immediateInvariant.
true.
%

doit
(WriteStream
	subclass: 'CypressMessageDigestStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-MesssageDigest';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

! Class implementation for 'CypressLoaderError'

!		Class methods for 'CypressLoaderError'

category: 'instance creation'
classmethod: CypressLoaderError
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself
%

!		Instance methods for 'CypressLoaderError'

category: 'accessing'
method: CypressLoaderError
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception
%

category: 'updating'
method: CypressLoaderError
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException
%

category: 'initializing - private'
method: CypressLoaderError
initialize

	super initialize.
	gsResumable := true
%

category: 'initializing - private'
method: CypressLoaderError
initializeMessageText

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPutAll: self patchOperation printString;
		nextPutAll: ' failed because ';
		nextPutAll: self exception printString.
	messageText := stream contents
%

category: 'initializing - private'
method: CypressLoaderError
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
%

category: 'handling'
method: CypressLoaderError
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.
%

category: 'accessing'
method: CypressLoaderError
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation
%

category: 'updating'
method: CypressLoaderError
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation
%

! Class implementation for 'CypressLoaderMissingClasses'

!		Class methods for 'CypressLoaderMissingClasses'

category: 'instance creation'
classmethod: CypressLoaderMissingClasses
missingRequirementsMap: aDictionary
	"Answer an instance of the receiver initialized on the specified
	 missing requirements. aDictionary maps prerequisite names to
	 a collection of dependent definitions."

	^self new
		initializeRequirementsMap: aDictionary;
		yourself
%

!		Instance methods for 'CypressLoaderMissingClasses'

category: 'initializing - private'
method: CypressLoaderMissingClasses
initialize

	super initialize.
	gsResumable := true
%

category: 'initializing - private'
method: CypressLoaderMissingClasses
initializeMessageText

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream nextPutAll: 'Missing classes:'.
	self requirementsMap keysAndValuesDo: 
			[:className :definitions |
			stream
				space;
				nextPutAll: className printString , '(' , definitions size printString
							, ')'].
	messageText := stream contents
%

category: 'initializing - private'
method: CypressLoaderMissingClasses
initializeRequirementsMap: aDictionary

	self
		requirementsMap: aDictionary;
		initializeMessageText.
%

category: 'accessing'
method: CypressLoaderMissingClasses
requirementsMap
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

   ^requirementsMap
%

category: 'updating'
method: CypressLoaderMissingClasses
requirementsMap: aDictionary
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

	requirementsMap := aDictionary
%

! Class implementation for 'CypressLoaderErrorNotification'

!		Class methods for 'CypressLoaderErrorNotification'

category: 'instance creation'
classmethod: CypressLoaderErrorNotification
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself
%

!		Instance methods for 'CypressLoaderErrorNotification'

category: 'handling'
method: CypressLoaderErrorNotification
defaultAction
	"Log the notification to the GCI log and the Transcript, then resume."

	self logNotification: 'Notice: ' , self asString.
	^super defaultAction
%

category: 'accessing'
method: CypressLoaderErrorNotification
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception
%

category: 'updating'
method: CypressLoaderErrorNotification
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException
%

category: 'initializing - private'
method: CypressLoaderErrorNotification
initializeMessageText

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPutAll: self patchOperation printString;
		nextPutAll: ' failed because ';
		nextPutAll: self exception printString.
	messageText := stream contents
%

category: 'initializing - private'
method: CypressLoaderErrorNotification
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
%

category: 'handling'
method: CypressLoaderErrorNotification
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.
%

category: 'accessing'
method: CypressLoaderErrorNotification
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation
%

category: 'updating'
method: CypressLoaderErrorNotification
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation
%

! Class implementation for 'CypressAbstractPackageFiler'

!		Class methods for 'CypressAbstractPackageFiler'

category: 'instance creation'
classmethod: CypressAbstractPackageFiler
forRepository: aCypressFileSystemRepository

	^self new
		initializeForRepository: aCypressFileSystemRepository;
		yourself.
%

!		Instance methods for 'CypressAbstractPackageFiler'

category: 'private'
method: CypressAbstractPackageFiler
fileUtils

	^CypressFileUtilities current
%

category: 'initializing - private'
method: CypressAbstractPackageFiler
initializeForRepository: aCypressFileSystemRepository

	repository := aCypressFileSystemRepository
%

category: 'accessing'
method: CypressAbstractPackageFiler
packageDirectory

	^packageDirectory
%

category: 'accessing'
method: CypressAbstractPackageFiler
packageDirectory: aDirectory

	packageDirectory := aDirectory
%

category: 'accessing'
method: CypressAbstractPackageFiler
packageStructure

	^packageStructure
%

category: 'accessing'
method: CypressAbstractPackageFiler
packageStructure: aPackageStructure

	packageStructure := aPackageStructure
%

category: 'accessing'
method: CypressAbstractPackageFiler
propertiesFileNameExtension

	^'.ston'
%

category: 'accessing'
method: CypressAbstractPackageFiler
propertiesFileNameExtensions

	^Array with: self propertiesFileNameExtension
%

category: 'accessing'
method: CypressAbstractPackageFiler
repository

	^repository
%

category: 'private'
method: CypressAbstractPackageFiler
string: aString endsWith: subString
	"Answer whether the last characters of aString are the same as subString."

	| expectedStart |
	expectedStart := aString size - subString size + 1 max: 1.
	^expectedStart
		= (aString indexOfSubCollection: subString startingAt: expectedStart)
%

! Class implementation for 'CypressAbstractFileoutWriter'

!		Instance methods for 'CypressAbstractFileoutWriter'

category: 'accessing'
method: CypressAbstractFileoutWriter
classesInDependencyOrder

	^(CypressGsGeneralDependencySorter
		on: self packageStructure classes
		dependsOn: [:candidate | candidate superclassName]
		dependent: [:candidate | candidate className]) inOrder
%

category: 'accessing'
method: CypressAbstractFileoutWriter
classesWithInitializers

	^self classesInDependencyOrder
		select: [:each | each classMethods anySatisfy: [:method | method selector = 'initialize']]
%

category: 'private'
method: CypressAbstractFileoutWriter
determinePackageDirectory

	^self fileUtils ensureDirectoryExists: self repository directoryPath
%

category: 'accessing'
method: CypressAbstractFileoutWriter
extensions

	^self packageStructure extensions
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOut: aString implementationsFrom: someClassStructures on: aStream

	someClassStructures
		do: [:each | self fileOutType: aString implementationOf: each on: aStream]
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOut: aString methods: someMethodStructures on: aStream

	someMethodStructures isEmpty ifTrue: [^self].
	self
		fileOut: aString methodsPreambleFor: someMethodStructures any classStructure on: aStream;
		fileOutMethods: someMethodStructures on: aStream
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutClassDeclarationsOn: aStream

	self classesInDependencyOrder
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutClassesOn: aStream

	self
		fileOutClassesPreambleOn: aStream;
		fileOutClassDeclarationsOn: aStream;
		fileOutClassImplementationsOn: aStream
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutClassImplementationsOn: aStream

	self
		fileOut: 'Class Implementation'
		implementationsFrom: self classesInDependencyOrder
		on: aStream
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutClassInitializersOn: aStream

	self fileOutClassInitializersPreambleOn: aStream.
	self classesWithInitializers do: [:each | self fileOutClassInitializerFor: each on: aStream].
	self fileOutClassInitializersPostambleOn: aStream.
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutExtensionImplementationsOn: aStream
  | sortedExtensions |
  sortedExtensions := self extensions
    asSortedCollection: [ :a :b | a className <= b className ].
  self
    fileOut: 'Class Extension'
    implementationsFrom: sortedExtensions
    on: aStream
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutExtensionsOn: aStream

	self
		fileOutExtensionsPreambleOn: aStream;
		fileOutExtensionImplementationsOn: aStream
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutMethods: someMethodStructures on: aStream

	(someMethodStructures
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:methodStructure | self fileOutMethod: methodStructure on: aStream]
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutPackageOn: aStream

	self
		fileOutPackagePreambleOn: aStream;
		fileOutClassesOn: aStream;
		fileOutExtensionsOn: aStream;
		fileOutClassInitializersOn: aStream;
		fileOutPackagePostambleOn: aStream
%

category: 'writing - private'
method: CypressAbstractFileoutWriter
fileOutType: aString implementationOf: classStructure on: aStream

	self
		fileOutPreambleType: aString
			for: classStructure
			on: aStream;
		fileOut: 'Class'
			methods: classStructure classMethods
			on: aStream;
		fileOut: 'Instance'
			methods: classStructure instanceMethods
			on: aStream
%

category: 'accessing'
method: CypressAbstractFileoutWriter
packageName

	^self packageStructure packageName
%

category: 'accessing'
method: CypressAbstractFileoutWriter
packageNameExtension

	^self subclassResponsibility: #packageNameExtension
%

category: 'writing'
method: CypressAbstractFileoutWriter
writePackageStructure

	CypressFileUtilities current
		writeStreamFor: self packageStructure packageName
				, self packageNameExtension
		in: self packageDirectory
		do: [:fileStream | self fileOutPackageOn: fileStream]
%

category: 'writing'
method: CypressAbstractFileoutWriter
writePackageStructure: aPackageStructure

	self
		packageStructure: aPackageStructure;
		packageDirectory: self determinePackageDirectory;
		writePackageStructure
%

! Class implementation for 'CypressSmalltalkFileoutWriter'

!		Instance methods for 'CypressSmalltalkFileoutWriter'

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOut: aString methodsPreambleFor: classStructure on: aStream

	self
		writeChunk: '" ------------------- ' , aString , ' methods for '
				, classStructure name , '"'
		on: aStream.
	aStream
		lf;
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutClassDeclaration: classStructure on: aStream
	"This is the structure for VW/Pharo  class definitions. It may or may not be portable
	to all dialects that use chunk-format fileins. It does not attempt to handle VW
	namespaces at this time. Probably should use an XML fileout for that."

	aStream
		nextPutAll: classStructure superclassName;
		nextPutAll: ' subclass: #';
		nextPutAll: classStructure className;
		lf;
		nextPutAll: '	instanceVariableNames:  ''';
		nextPutAll: classStructure instanceVariablesString;
		nextPut: $';
		lf;
		nextPutAll: '	classVariableNames: ''';
		nextPutAll: classStructure classVariablesString;
		nextPut: $';
		lf;
		nextPutAll: '	poolDictionaries: ''';
		nextPutAll: classStructure poolDictionariesString;
		nextPut: $';
		lf;
		nextPutAll: '	category: ''';
		nextPutAll: classStructure category.
	self writeChunk: '''' on: aStream.
	aStream
		lf;
		lf;
		nextPutAll: classStructure className;
		nextPutAll: ' comment:';
		lf.
	self writeChunk: classStructure comment printString on: aStream.
	aStream
		lf;
		lf;
		nextPutAll: classStructure className;
		nextPutAll: ' class instanceVariableNames: ''';
		nextPutAll: classStructure classInstanceVariablesString.
	self writeChunk: '''' on: aStream.
	aStream
		lf;
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutClassesPreambleOn: aStream

	self writeChunk: '" Class Declarations "' on: aStream.
	aStream
		lf;
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutClassInitializerFor: classStructure on: aStream

	self writeChunk: classStructure className , ' initialize.' on: aStream.
	aStream lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutClassInitializersPostambleOn: aStream

	aStream
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutClassInitializersPreambleOn: aStream

	self writeChunk: '" Class initializers "' on: aStream.
	aStream
		lf;
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutExtensionsPreambleOn: aStream

	self writeChunk: '" Class Extensions "' on: aStream.
	aStream
		lf;
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutMethod: methodStructure on: aStream

	aStream nextPutAll: '!'.
	self
		writeChunk: methodStructure classStructure className
				, (methodStructure isMetaclass
						ifTrue: [' class methodsFor: ']
						ifFalse: [' methodsFor: '])
					, methodStructure category printString
		on: aStream.
	aStream lf.
	self
		writeChunk: methodStructure source on: aStream;
		writeChunk: ' ' on: aStream.
	aStream lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf.
	self writeChunk: '" End of Package: ' , self packageName , '"' on: aStream.
	aStream
		lf;
		lf;
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutPackagePreambleOn: aStream

	self writeChunk: '" Package: ' , self packageName , '"' on: aStream.
	aStream
		lf;
		lf;
		lf
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
fileOutPreambleType: aString for: classStructure on: aStream

	self writeChunk: '" ' , aString , ' for ' , classStructure name , '"'
		on: aStream.
	aStream
		lf;
		lf
%

category: 'accessing'
method: CypressSmalltalkFileoutWriter
packageNameExtension

	^'.st'
%

category: 'writing - private'
method: CypressSmalltalkFileoutWriter
writeChunk: aString on: aStream

	aString do: 
			[:each |
			aStream nextPut: each.
			each = $! ifTrue: [aStream nextPut: each]].
	aStream nextPut: $!
%

! Class implementation for 'CypressTopazFileoutWriter'

!		Instance methods for 'CypressTopazFileoutWriter'

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOut: aString methodsPreambleFor: classStructure on: aStream

	aStream
		nextPutAll: '! ------------------- ', aString, ' methods for ', classStructure name; lf;
		lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutClassDeclaration: classStructure on: aStream
  aStream
    nextPutAll: 'doit';
    lf;
    nextPutAll: '(' , classStructure superclassName;
    lf.
  self
    writeClassTypeMessage: classStructure
    on: aStream
    hasInstanceVariables: [ aStream
        nextPutAll:
            '	instVarNames: #( ' , classStructure instanceVariablesString , ' )';
        lf ].
  aStream
    nextPutAll: '	classVars: #( ' , classStructure classVariablesString , ' )';
    lf;
    nextPutAll:
        '	classInstVars: #( ' , classStructure classInstanceVariablesString , ' )';
    lf;
    nextPutAll: '	poolDictionaries: #()';
    lf;
    nextPutAll: '	inDictionary: UserGlobals';
    lf;
    nextPutAll: '	options: #())';
    lf;
    nextPutAll: '		category: ' , classStructure category printString , ';';
    lf;
    nextPutAll: '		comment: ' , classStructure comment printString , ';';
    lf;
    nextPutAll: '		immediateInvariant.';
    lf;
    nextPutAll: 'true.';
    lf;
    nextPutAll: '%';
    lf;
    lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutClassesPreambleOn: aStream

	aStream
		nextPutAll: '! Class Declarations'; lf;
		lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutClassInitializerFor: classStructure on: aStream

	aStream
		nextPutAll: classStructure className, ' initialize.'; lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutClassInitializersPostambleOn: aStream

	aStream
                nextPutAll: 'true.'; lf;
		nextPutAll: '%'; lf;
		lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutClassInitializersPreambleOn: aStream

	aStream
		nextPutAll: '! Class initializers '; lf;
		lf;
		nextPutAll: 'doit'; lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutExtensionsPreambleOn: aStream

	aStream
		nextPutAll: '! Class Extensions'; lf;
		lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutMethod: methodStructure on: aStream

	aStream
		nextPutAll: 'category: ', methodStructure category printString; lf;
		nextPutAll: (methodStructure isMetaclass ifTrue: ['classmethod: '] ifFalse: ['method: ']), methodStructure classStructure className; lf;
		nextPutAll: methodStructure source.
	methodStructure source last = Character lf
		ifFalse: [aStream lf].
	aStream nextPutAll: '%'; lf;
		lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutPackagePostambleOn: aStream

	aStream
		lf;
		lf;
		nextPutAll: '! End of Package: ', self packageName; lf;
		lf;
		lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutPackagePreambleOn: aStream

	aStream
		nextPutAll: '! Package: ', self packageName; lf;
		lf;
		lf;
		nextPutAll: '! Remove existing behavior from package ', self packageName; lf;
		nextPutAll: '!!!! This can be cleaned up when some package functionality is moved to the base system.'; lf;
		lf;
		nextPutAll: 'doit'; lf;
		nextPutAll: '| packageName |'; lf;
		nextPutAll: 'packageName := ', self packageName printString, '.'; lf;
		nextPutAll: 'System myUserProfile symbolList do: [:symDict |'; lf;
		nextPutAll: '	symDict do: [:possibleClass |'; lf;
		nextPutAll: '			| toRemove |'; lf;
		nextPutAll: '		possibleClass isBehavior ifTrue: ['; lf;
		nextPutAll: '			{possibleClass. possibleClass class} do: [:aClass |'; lf;
		nextPutAll: '				aClass category = packageName'; lf;
		nextPutAll: '					ifTrue: ['; lf;
		nextPutAll: '							"*anythingbutpackagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										each isEmpty not and: ['; lf;
		nextPutAll: '											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])'; lf;
		nextPutAll: '											or: [each first ~= $*]]]'; lf;
		nextPutAll: '					]'; lf;
		nextPutAll: '					ifFalse: ['; lf;
		nextPutAll: '							"*packagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										each isEmpty not and: ['; lf;
		nextPutAll: '											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]'; lf;
		nextPutAll: '					].'; lf;
		nextPutAll: '				toRemove do: [:each | aClass removeCategory: each].'; lf;
		nextPutAll: '			]'; lf;
		nextPutAll: '		]'; lf;
		nextPutAll: '	]'; lf;
		nextPutAll: '].'; lf;
                nextPutAll: 'true.'; lf;
		nextPutAll: '%'; lf;
		lf;
		lf
%

category: 'writing - private'
method: CypressTopazFileoutWriter
fileOutPreambleType: aString for: classStructure on: aStream

	aStream
		nextPutAll: '! ', aString, ' for ', classStructure name; lf;
		lf
%

category: 'accessing'
method: CypressTopazFileoutWriter
packageNameExtension

	^'.gs'
%

category: 'writing - private'
method: CypressTopazFileoutWriter
writeClassTypeMessage: classStructure on: aStream hasInstanceVariables: instanceVariableBlock
  | classType classTypeMessage hasInstanceVariables |
  hasInstanceVariables := true.
  classType := classStructure subclassType.
  classType = 'indexableSubclass'
    ifTrue: [ classTypeMessage := 'indexableSubclass: ' ]
    ifFalse: [ classType = 'byteSubclass'
        ifTrue: [ classTypeMessage := 'byteSubclass: '.
          hasInstanceVariables := false ]
        ifFalse: [ classType = ''
            ifTrue: [ classTypeMessage := 'subclass: ' ]
            ifFalse: [ self error: 'unknown subclass type: ' , classType ] ] ].
  aStream
    tab;
    nextPutAll: classTypeMessage , classStructure className asString printString;
    lf.
  hasInstanceVariables
    ifTrue: [ instanceVariableBlock value ]
%

! Class implementation for 'CypressAbstractPackageReader'

!		Instance methods for 'CypressAbstractPackageReader'

category: 'private'
method: CypressAbstractPackageReader
classStructureFrom: classPropertiesDict

	^(CypressClassStructure new)
		packageStructure: self packageStructure;
		isClassExtension: true;
		properties: classPropertiesDict;
		yourself
%

category: 'private'
method: CypressAbstractPackageReader
classStructureFrom: classPropertiesDict comment: classComment

	^(self classStructureFrom: classPropertiesDict)
		isClassExtension: false;
		comment: classComment;
		yourself
%

category: 'reading'
method: CypressAbstractPackageReader
isPropertiesFileDirectoryEntry: entry

	^self propertiesFileNameExtensions
		anySatisfy: [:each | self string: entry endsWith: '/properties' , each]
%

category: 'accessing'
method: CypressAbstractPackageReader
packageExtension

	^self packageStructure
		packageExtensionOr: [self repository packageExtension]
%

category: 'parsing'
method: CypressAbstractPackageReader
parseSelectorFrom: methodString

	| meth |
	^
	[meth := self
				_parseMethod: methodString
				category: #xyzzy
				using: GsSession currentSession symbolList
				environmentId: 0.
	meth class ~~ GsNMethod
		ifTrue: 
			["if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [^nil].
			meth := meth at: 1].
	meth selector asString]
			on: CompileError
			do: [:ex | ex return: '_____could_not_parse_selector_from_method_source_____']
%

category: 'reading'
method: CypressAbstractPackageReader
readClassCommentFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | self string: entry endsWith: '/README.md']
				ifNone: [^''])
		do: [:fileStream | ^fileStream contents]
%

category: 'reading'
method: CypressAbstractPackageReader
readClassPropertiesFromDirectoryEntries: entries

	self fileUtils readStreamFor: (entries
				detect: [:entry | self isPropertiesFileDirectoryEntry: entry]
				ifNone: [^Dictionary new])
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
%

category: 'reading'
method: CypressAbstractPackageReader
readClassStructureFromEntry: classEntry

	| classDirectory classPropertiesDict classComment entries classStructure |
	classDirectory := classEntry.
	entries := self fileUtils directoryEntriesFrom: classDirectory.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classComment := self readClassCommentFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict
				comment: classComment.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure
%

category: 'reading'
method: CypressAbstractPackageReader
readCypressFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods

	| notice category source selector |
	(fileStream peekFor: $")
		ifTrue: [fileStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(fileStream match: 'notice: ')
		ifTrue: [notice := fileStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(fileStream match: 'category: ')
		ifTrue: [category := fileStream nextLine trimSeparators]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	(fileStream peekFor: $")
		ifTrue: [fileStream nextLine]
		ifFalse: [self error: 'Method does not have valid Cypress format'].
	source := fileStream upToEnd.
	selector := self parseSelectorFrom: source.
	methods at: selector
		put: ((CypressMethodStructure new)
				packageStructure: self packageStructure;
				classStructure: classStructure;
				name: selector;
				isMetaclass: isMeta;
				selector: selector;
				category: category;
				source: source;
				yourself)
%

category: 'reading'
method: CypressAbstractPackageReader
readExtensionClassStructureFromEntry: classEntry

	| classPropertiesDict entries classStructure |
	entries := self fileUtils directoryEntriesFrom: classEntry.
	classPropertiesDict := self
				readClassPropertiesFromDirectoryEntries: entries.
	classStructure := self classStructureFrom: classPropertiesDict.
	self readMethodStructureFor: classStructure in: entries.
	^classStructure
%

category: 'reading'
method: CypressAbstractPackageReader
readFileTreeFormatMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods

	| category source selector |
	category := fileStream nextLine trimSeparators.
	source := fileStream upToEnd.
	selector := self parseSelectorFrom: source.
	methods at: selector
		put: ((CypressMethodStructure new)
				packageStructure: self packageStructure;
				classStructure: classStructure;
				name: selector;
				isMetaclass: isMeta;
				selector: selector;
				category: category;
				source: source;
				yourself)
%

category: 'reading'
method: CypressAbstractPackageReader
readMethodStructureFor: classStructure in: entries

	entries do: 
			[:entry |
			| methods isMeta |
			methods := (isMeta := self string: entry endsWith: '/class')
						ifTrue: [classStructure classMethods]
						ifFalse: [classStructure instanceMethods].
			((self string: entry endsWith: '/instance')
				or: [self string: entry endsWith: '/class'])
					ifTrue: 
						[((self fileUtils directoryEntriesFrom: entry)
							select: [:each | self string: each endsWith: '.st']) do: 
									[:methodEntry |
									self fileUtils readStreamFor: methodEntry
										do: 
											[:fileStream |
											self
												readMethodStructureFrom: fileStream
												intoClassStructure: classStructure
												meta: isMeta
												methods: methods]]]]
%

category: 'reading'
method: CypressAbstractPackageReader
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods

	self subclassResponsibility: #readMethodStructureFrom:intoClassStructure:meta:methods:
%

category: 'reading'
method: CypressAbstractPackageReader
readPackageStructure

	(self fileUtils directoryEntriesFrom: self packageDirectory) do: 
			[:entry |
			(self isPropertiesFileDirectoryEntry: entry)
				ifTrue: [self packageStructure properties: (self readPropertiesFile: entry)].
			(self string: entry endsWith: '.class')
				ifTrue: 
					[self packageStructure classes
						add: (self readClassStructureFromEntry: entry)].
			(self string: entry endsWith: '.extension')
				ifTrue: 
					[self packageStructure extensions
						add: (self readExtensionClassStructureFromEntry: entry)]]
%

category: 'reading'
method: CypressAbstractPackageReader
readPackageStructureForPackageNamed: packageName

	| structureName |
	structureName := packageName , self repository packageExtension.
	self
		packageStructure: (CypressPackageStructure named: structureName);
		packageDirectory: (self fileUtils directoryFromPath: structureName
					relativeTo: self repository directoryPath);
		readPackageStructure
%

category: 'reading'
method: CypressAbstractPackageReader
readPropertiesFile: entry

	self fileUtils
		readStreamFor: entry
		do: [:fileStream | ^CypressJsonParser parseStream: fileStream]
%

category: 'parsing'
method: CypressAbstractPackageReader
_parseMethod: source category: cat using: aSymbolList environmentId: anEnvironmentId
	"Compiles the method into disposable dictionaries, if possible.
	 Attempts auto-recompile for undefinedSymbols.
	 Returns the compiled method or signals a CompileError."

	| undefinedSymbolList undefinedSymbols |
	undefinedSymbols := SymbolDictionary new name: #UndefinedSymbols.
	undefinedSymbolList := SymbolList with: undefinedSymbols.
	^
	[UndefinedObject
		compileMethod: source
		dictionaries: aSymbolList
		category: cat
		intoMethodDict: GsMethodDictionary new
		intoCategories: GsMethodDictionary new
		intoPragmas: nil
		environmentId: anEnvironmentId]
			onSynchronous: (Array with: CompileError with: CompileWarning)
			do: (Array with: 
						[:ex |
						| undefSymbol symbols |
						undefSymbol := true.
						symbols := Array new.
						ex errorDetails do: 
								[:errArray |
								(errArray atOrNil: 1) == 1031
									ifTrue: [symbols add: (errArray atOrNil: 5) asSymbol]
									ifFalse: [undefSymbol := false]].
						undefSymbol
							ifTrue: 
								["attempt auto-define of undefined symbols"
								symbols do: [:sym | undefinedSymbols at: sym put: nil].
								
								[^UndefinedObject
									compileMethod: source
									dictionaries: aSymbolList , undefinedSymbolList
									category: cat
									intoMethodDict: GsMethodDictionary new
									intoCategories: GsMethodDictionary new
									intoPragmas: nil
									environmentId: anEnvironmentId]
										onException: CompileError
										do: [:exb | undefSymbol := false]].
						undefSymbol ifFalse: [ex outer]]
					with: [:ex | ex resume])
%

! Class implementation for 'CypressDoNothingPackageReader'

!		Instance methods for 'CypressDoNothingPackageReader'

category: 'reading'
method: CypressDoNothingPackageReader
readPackageStructure
%

! Class implementation for 'CypressFileTreeFormatPackageReader'

!		Instance methods for 'CypressFileTreeFormatPackageReader'

category: 'private'
method: CypressFileTreeFormatPackageReader
classStructureFrom: fileteeClassPropertiesDict comment: classComment
  | classPropertiesDict subclassType filetreeSubclassType |
  classPropertiesDict := fileteeClassPropertiesDict copy.
  filetreeSubclassType := classPropertiesDict at: 'type'.
  filetreeSubclassType = 'normal'
    ifTrue: [ subclassType := '' ]
    ifFalse: [ 
      filetreeSubclassType = 'variable'
        ifTrue: [ subclassType := 'indexableSubclass' ]
        ifFalse: [ 
          filetreeSubclassType = 'bytes'
            ifTrue: [ subclassType := 'byteSubclass' ]
            ifFalse: [ self error: 'unknown subclass type: ' , filetreeSubclassType printString ] ] ].
  classPropertiesDict at: '_gs_subclassType' put: subclassType.
  ^ super classStructureFrom: classPropertiesDict comment: classComment
%

category: 'accessing'
method: CypressFileTreeFormatPackageReader
propertiesFileNameExtension

	^'.json'
%

category: 'accessing'
method: CypressFileTreeFormatPackageReader
propertiesFileNameExtensions

	^Array
		with: super propertiesFileNameExtension
		with: self propertiesFileNameExtension
%

category: 'reading'
method: CypressFileTreeFormatPackageReader
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"Strict!"

	self
		readFileTreeFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods
%

! Class implementation for 'CypressFlexiblePackageReader'

!		Instance methods for 'CypressFlexiblePackageReader'

category: 'reading'
method: CypressFlexiblePackageReader
isPropertiesFileDirectoryEntry: entry
	"Expect .ston properties file, but tolerate .json if present."

	^(super isPropertiesFileDirectoryEntry: entry)
		or: [self string: entry endsWith: '/properties.json']
%

category: 'reading'
method: CypressFlexiblePackageReader
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"If the stream begins with a double quote, process it on the assumption it is a Cypress-format method.
	 Otherwise, treat it as a FileTree-format method."

	fileStream peek = $"
		ifTrue: 
			[self
				readCypressFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]
		ifFalse: 
			[self
				readFileTreeFormatMethodStructureFrom: fileStream
				intoClassStructure: classStructure
				meta: isMeta
				methods: methods]
%

! Class implementation for 'CypressPackageReader'

!		Instance methods for 'CypressPackageReader'

category: 'reading'
method: CypressPackageReader
readMethodStructureFrom: fileStream intoClassStructure: classStructure meta: isMeta methods: methods
	"Strict!"

	self
		readCypressFormatMethodStructureFrom: fileStream
		intoClassStructure: classStructure
		meta: isMeta
		methods: methods
%

! Class implementation for 'CypressAbstractPackageWriter'

!		Class methods for 'CypressAbstractPackageWriter'

category: 'initialization'
classmethod: CypressAbstractPackageWriter
initializeSpecials
	"Valid binarySelector characters  '!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'"

	| map |
	map := Dictionary new.
	map
		at: $! put: 'bang';
		at: $% put: 'percent';
		at: $& put: 'and';
		at: $* put: 'star';
		at: $+ put: 'plus';
		at: $, put: 'comma';
		at: $- put: 'minus';
		at: $/ put: 'slash';
		at: $< put: 'less';
		at: $= put: 'equals';
		at: $> put: 'more';
		at: $? put: 'wat';
		at: $@ put: 'at';
		at: $\ put: 'backslash';
		at: $| put: 'pipe';
		at: $~ put: 'tilde'.
	map keys do: [:key | map at: (map at: key) put: key].
	^map
%

category: 'accessing'
classmethod: CypressAbstractPackageWriter
specials

	^specials ifNil: [specials := self initializeSpecials]
%

!		Instance methods for 'CypressAbstractPackageWriter'

category: 'private'
method: CypressAbstractPackageWriter
determinePackageDirectory

	^self fileUtils ensureDirectoryExists: (self fileUtils
				directoryFromPath: self packageStructure name
				relativeTo: self repository directoryPath)
%

category: 'private'
method: CypressAbstractPackageWriter
directoryForDirectoryNamed: directoryNameOrPath

	^directoryNameOrPath = '.'
		ifTrue: [self fileUtils ensureDirectoryExists: self packageDirectory]
		ifFalse: [self subPackageFileDirectoryFor: directoryNameOrPath]
%

category: 'private'
method: CypressAbstractPackageWriter
fileNameForSelector: selector

	^selector last = $:
		ifTrue: [selector copyReplacing: $: with: $.]
		ifFalse: 
			[(selector first isLetter or: [selector first = $_])
				ifTrue: [selector]
				ifFalse: 
					[| specials output |
					specials := self class specials.
					output := WriteStreamPortable on: (String new: 100).
					output nextPut: $^.
					selector do: [:each | output nextPutAll: (specials at: each)]
						separatedBy: [output nextPut: $.].
					output contents]]
%

category: 'writing'
method: CypressAbstractPackageWriter
removeOldReplacingWithNew
  self fileUtils deleteAll: self packageDirectory.
  self writePropertiesFile.
  self writePackageStructure
%

category: 'private'
method: CypressAbstractPackageWriter
subPackageFileDirectoryFor: directoryNameOrPath

	| dir |
	dir := self fileUtils directoryFromPath: directoryNameOrPath
				relativeTo: self packageDirectory.
	self fileUtils ensureDirectoryExists: dir.
	^dir
%

category: 'writing'
method: CypressAbstractPackageWriter
writeClassComment: classStructure on: fileStream

	fileStream
		nextPutAll: (CypressObject normalizeLineEndingsOf: classStructure comment)
%

category: 'writing'
method: CypressAbstractPackageWriter
writeClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: self propertiesFileNameExtension
			visit: [:fileStream | classStructure properties _writeCypressJsonOn: fileStream]
%

category: 'writing'
method: CypressAbstractPackageWriter
writeExtensionClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: 
			[:fileStream |
			(Dictionary with: 'name' -> classStructure className)
				_writeCypressJsonOn: fileStream]
%

category: 'private'
method: CypressAbstractPackageWriter
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock

	| directory |
	directory := self directoryForDirectoryNamed: directoryNameOrPath.
	self fileUtils
		writeStreamFor: fileName , ext
		in: directory
		do: [:fileStream | visitBlock value: fileStream]
%

category: 'writing'
method: CypressAbstractPackageWriter
writeMethodStructure: methodStructure onStream: fileStream

	self subclassResponsibility: #writeMethodStructure:onStream:
%

category: 'writing'
method: CypressAbstractPackageWriter
writeMethodStructure: methodStructure to: methodPath

	| filename |
	filename := self fileNameForSelector: methodStructure selector.
	self
		writeInDirectoryName: methodPath
		fileName: filename
		extension: '.st'
		visit: [:fileStream | self writeMethodStructure: methodStructure onStream: fileStream]
%

category: 'writing'
method: CypressAbstractPackageWriter
writePackageStructure

	self
		writePackageStructureClasses: self packageStructure classes
			isClassExtension: false;
		writePackageStructureClasses: self packageStructure extensions
			isClassExtension: true
%

category: 'writing'
method: CypressAbstractPackageWriter
writePackageStructure: aPackageStructure

	self
		packageStructure: aPackageStructure;
		packageDirectory: self determinePackageDirectory;
		removeOldReplacingWithNew
%

category: 'writing'
method: CypressAbstractPackageWriter
writePackageStructureClasses: classStructures isClassExtension: isClassExtension

	| classDirExtension |
	classDirExtension := isClassExtension
				ifTrue: ['.extension']
				ifFalse: ['.class'].
	classStructures do: 
			[:classStructure |
			| classPath instanceMethodPath classMethodPath |
			classPath := classStructure className , classDirExtension
						, self fileUtils pathNameDelimiter asString.
			isClassExtension
				ifTrue: [self writeExtensionClassStructure: classStructure to: classPath]
				ifFalse: [self writeClassStructure: classStructure to: classPath].
			instanceMethodPath := classPath , 'instance' , self fileUtils pathNameDelimiter asString.
			classStructure instanceMethods
				do: [:methodStructure | self writeMethodStructure: methodStructure to: instanceMethodPath].
			classMethodPath := classPath , 'class' , self fileUtils pathNameDelimiter asString.
			classStructure classMethods
				do: [:methodStructure | self writeMethodStructure: methodStructure to: classMethodPath]]
%

category: 'writing'
method: CypressAbstractPackageWriter
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new _writeCypressJsonOn: fileStream]
%

! Class implementation for 'CypressFileTreeFormatPackageWriter'

!		Instance methods for 'CypressFileTreeFormatPackageWriter'

category: 'private'
method: CypressFileTreeFormatPackageWriter
adjustClassPropertiesForFileTree: classPropertyDict
  | props classType |
  props := classPropertyDict copy.
  classType := (props at: '_gs_subclassType' ifAbsent: [  ])
    ifNil: [ 'normal' ]
    ifNotNil: [ :type | 
      props removeKey: '_gs_subclassType'.
      type = 'indexableSubclass'
        ifTrue: [ 'variable' ]
        ifFalse: [ 
          type = 'byteSubclass'
            ifTrue: [ 'bytes' ]
            ifFalse: [ self error: 'unknown subclass type: ' , type ] ] ].
  props at: 'type' put: classType.
  ^ props
%

category: 'accessing'
method: CypressFileTreeFormatPackageWriter
propertiesFileNameExtension
  ^ '.json'
%

category: 'writing'
method: CypressFileTreeFormatPackageWriter
removeOldReplacingWithNew

	self fileUtils deleteAll: self packageDirectory
		rejecting: 
			[:filename |
			"do not delete the monticello.meta directory to preserve existing Monticello meta data.
       Equivalent behavior to MCFileTreeRepository with Metadata property set to false."
			(self string: filename endsWith: 'monticello.meta')
				or: [ (self string: filename endsWith: '.filetree')
					or: [self string: filename endsWith: 'methodProperties.json']]].
	self writePropertiesFile.
	self writePackageStructure
%

category: 'writing'
method: CypressFileTreeFormatPackageWriter
writeClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
			fileName: 'README'
			extension: '.md'
			visit: [:fileStream | self writeClassComment: classStructure on: fileStream];
		writeInDirectoryName: classPath
			fileName: 'properties'
			extension: self propertiesFileNameExtension
			visit: 
				[:fileStream |
				(self adjustClassPropertiesForFileTree: classStructure properties)
					_writeCypressJsonOn: fileStream]
%

category: 'writing'
method: CypressFileTreeFormatPackageWriter
writeExtensionClassStructure: classStructure to: classPath

	self
		writeInDirectoryName: classPath
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: 
			[:fileStream |
			(Dictionary with: 'name' -> classStructure className)
				_writeCypressJsonOn: fileStream]
%

category: 'writing'
method: CypressFileTreeFormatPackageWriter
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: methodStructure category;
		lf;
		nextPutAll: (CypressObject normalizeLineEndingsOf: methodStructure source)
%

category: 'writing'
method: CypressFileTreeFormatPackageWriter
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | Dictionary new _writeCypressJsonOn: fileStream]
%

! Class implementation for 'CypressPackageWriter'

!		Instance methods for 'CypressPackageWriter'

category: 'accessing - private'
method: CypressPackageWriter
methodNoticeLine

	^self packageStructure properties
		at: 'copyrightLine'
		ifAbsent: [self repository copyrightProperty]
%

category: 'writing'
method: CypressPackageWriter
writeMethodStructure: methodStructure onStream: fileStream

	fileStream
		nextPutAll: '"';
		lf;
		nextPutAll: 'notice: ' , self methodNoticeLine;
		lf;
		nextPutAll: 'category: ' , methodStructure category;
		lf;
		nextPutAll: '"';
		lf;
		nextPutAll: (CypressObject normalizeLineEndingsOf: methodStructure source)
%

category: 'writing'
method: CypressPackageWriter
writePropertiesFile

	self
		writeInDirectoryName: '.'
		fileName: 'properties'
		extension: self propertiesFileNameExtension
		visit: [:fileStream | self repository properties _writeCypressJsonOn: fileStream]
%

! Class implementation for 'CypressStrictFileTreeFormatDoNothingPackageWriter'

!		Instance methods for 'CypressStrictFileTreeFormatDoNothingPackageWriter'

category: 'writing'
method: CypressStrictFileTreeFormatDoNothingPackageWriter
removeOldReplacingWithNew
	"Change nothing, since a Cypress writer has insufficient information
	 for preserving the FileTree details. Strictly read-only."
%

category: 'writing'
method: CypressStrictFileTreeFormatDoNothingPackageWriter
writePropertiesFile
	"Change nothing, since a Cypress writer has insufficient information
	 for preserving the FileTree details. Strictly read-only."
%

! Class implementation for 'CypressAbstractRepository'

!		Class methods for 'CypressAbstractRepository'

category: 'instance creation'
classmethod: CypressAbstractRepository
createOn: aUrl alias: aString
  ^ self onUrl: aUrl alias: aString
%

category: 'accessing'
classmethod: CypressAbstractRepository
defaultCopyrightNotice

	^DefaultCopyrightNotice
%

category: 'accessing'
classmethod: CypressAbstractRepository
defaultCopyrightNotice: aString

	DefaultCopyrightNotice := aString
%

category: 'initializing'
classmethod: CypressAbstractRepository
initialize

	self initializeDefaultCopyrightNotice
%

category: 'initializing'
classmethod: CypressAbstractRepository
initializeDefaultCopyrightNotice

	self defaultCopyrightNotice isNil ifFalse: [^self].
	self defaultCopyrightNotice: 'This work is protected by copyright. All rights reserved.'
%

category: 'instance creation'
classmethod: CypressAbstractRepository
onUrl: aUrl alias: aString

	^(aUrl repositoryClass new)
		initializeUrl: aUrl andAlias: aString;
		yourself
%

!		Instance methods for 'CypressAbstractRepository'

category: 'accessing properties'
method: CypressAbstractRepository
alias

	^properties 
		at: 'alias'
		ifAbsent: ['']
%

category: 'accessing properties'
method: CypressAbstractRepository
alias: aString

	properties 
		at: 'alias'
		put: aString
%

category: 'accessing properties'
method: CypressAbstractRepository
copyrightProperty

	^properties 
		at: '_cypress_copyright'
		ifAbsent: ['']
%

category: 'accessing properties'
method: CypressAbstractRepository
copyrightProperty: aString

	properties 
		at: '_cypress_copyright'
		put: aString
%

category: 'accessing'
method: CypressAbstractRepository
defaultCopyrightNotice

	^self class defaultCopyrightNotice
%

category: 'accessing'
method: CypressAbstractRepository
description

	^self alias
%

category: 'initializing - private'
method: CypressAbstractRepository
initialize

	self initializeDefaultRepositoryProperties.
	readerClass := CypressPackageReader.
	writerClass := CypressPackageWriter.
%

category: 'initializing - private'
method: CypressAbstractRepository
initializeDefaultCopyrightProperty

	self copyrightProperty: self defaultCopyrightNotice
%

category: 'initializing - private'
method: CypressAbstractRepository
initializeDefaultRepositoryProperties

	properties := Dictionary new.
	self initializeDefaultCopyrightProperty
%

category: 'initializing - private'
method: CypressAbstractRepository
initializeUrl: aUrl andAlias: aString

	self
		initialize;
		url: aUrl;
		alias: aString;
		validateUrl
%

category: 'accessing properties'
method: CypressAbstractRepository
packageExtension

	^properties 
		at: 'packageExtension'
		ifAbsent: ['.package']
%

category: 'printing'
method: CypressAbstractRepository
printDetailsOn: aStream

	aStream nextPutAll: self alias
%

category: 'printing'
method: CypressAbstractRepository
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'
%

category: 'reading'
method: CypressAbstractRepository
reader

	^readerClass forRepository: self
%

category: 'reading'
method: CypressAbstractRepository
readPackageStructureForPackageNamed: packageName

	^(self reader)
		readPackageStructureForPackageNamed: packageName;
		packageStructure
%

category: 'accessing'
method: CypressAbstractRepository
url

	^url
%

category: 'accessing'
method: CypressAbstractRepository
url: aString

	url := aString
%

category: 'validating - private'
method: CypressAbstractRepository
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."
%

category: 'writing'
method: CypressAbstractRepository
writePackageStructure: aPackageStructure

	^self writer writePackageStructure: aPackageStructure
%

category: 'writing'
method: CypressAbstractRepository
writer

	^writerClass forRepository: self
%

! Class implementation for 'CypressAbstractFileoutRepository'

!		Class methods for 'CypressAbstractFileoutRepository'

category: 'instance creation'
classmethod: CypressAbstractFileoutRepository
on: aDirectory

	^self new
		initializeForDirectory: aDirectory;
		yourself.
%

!		Instance methods for 'CypressAbstractFileoutRepository'

category: 'accessing'
method: CypressAbstractFileoutRepository
description

	| desc |
	desc := super description.
	^desc notEmpty
		ifTrue: [desc]
		ifFalse: [self directoryPath]
%

category: 'accessing'
method: CypressAbstractFileoutRepository
directoryPath

	^directoryPath
%

category: 'initializing - private'
method: CypressAbstractFileoutRepository
directoryPath:  aString

	directoryPath := aString
%

category: 'initializing - private'
method: CypressAbstractFileoutRepository
ensureDirectoryPathExists

	self fileUtils ensureDirectoryExists: self directoryPath
%

category: 'accessing - private'
method: CypressAbstractFileoutRepository
fileUtils

	^CypressFileUtilities current
%

category: 'initializing - private'
method: CypressAbstractFileoutRepository
initializeCreationOn: aUrl alias: aString

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString
%

category: 'initializing - private'
method: CypressAbstractFileoutRepository
initializeForDirectory: aDirectory

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
%

category: 'initializing - private'
method: CypressAbstractFileoutRepository
initializeReaderAndWriterClasses
  self subclassResponsibility: #'initializeReaderAndWriterClasses'
%

category: 'initializing - private'
method: CypressAbstractFileoutRepository
initializeUrl: aUrl andAlias: aString

	super initializeUrl: aUrl andAlias: aString.
	self directoryPath: self url pathForDirectory.
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
%

category: 'printing'
method: CypressAbstractFileoutRepository
printDetailsOn: aStream

	self alias notEmpty
		ifTrue: 
			[aStream
				nextPutAll: self alias;
				nextPutAll: ': '].
	aStream nextPutAll: self url printString
%

category: 'validating - private'
method: CypressAbstractFileoutRepository
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']
%

! Class implementation for 'CypressSmalltalkRepository'

!		Instance methods for 'CypressSmalltalkRepository'

category: 'initializing - private'
method: CypressSmalltalkRepository
initializeReaderAndWriterClasses

	readerClass := CypressDoNothingPackageReader.
	writerClass := CypressSmalltalkFileoutWriter.
%

! Class implementation for 'CypressTopazRepository'

!		Instance methods for 'CypressTopazRepository'

category: 'initializing - private'
method: CypressTopazRepository
initializeReaderAndWriterClasses

	readerClass := CypressDoNothingPackageReader.
	writerClass := CypressTopazFileoutWriter.
%

! Class implementation for 'CypressDictionaryRepository'

!		Class methods for 'CypressDictionaryRepository'

category: 'instance creation'
classmethod: CypressDictionaryRepository
on: aDictionary
  ^ self new
    initializeForDictionary: aDictionary;
    yourself
%

!		Instance methods for 'CypressDictionaryRepository'

category: 'accessing'
method: CypressDictionaryRepository
dictionary
  ^ dictionary
%

category: 'accessing'
method: CypressDictionaryRepository
dictionary: aDictionary
  dictionary := aDictionary
%

category: 'initializing - private'
method: CypressDictionaryRepository
initialize
  super initialize.
  readerClass := nil.
  writerClass := nil
%

category: 'initializing - private'
method: CypressDictionaryRepository
initializeForDictionary: aDictionary
  self initialize.
  self dictionary: aDictionary
%

category: 'accessing'
method: CypressDictionaryRepository
packageNames
  ^ self dictionary keys
%

category: 'reading'
method: CypressDictionaryRepository
readPackageStructureForPackageNamed: packageName
  ^ (self dictionary at: packageName) packageStructure
%

category: 'writing'
method: CypressDictionaryRepository
writePackageStructure: aPackageStructure
  ^ self dictionary at: aPackageStructure packageName put: aPackageStructure
%

! Class implementation for 'CypressFileSystemRepository'

!		Class methods for 'CypressFileSystemRepository'

category: 'instance creation'
classmethod: CypressFileSystemRepository
on: aDirectory

	^self new
		initializeForDirectory: aDirectory;
		yourself.
%

!		Instance methods for 'CypressFileSystemRepository'

category: 'accessing - properties'
method: CypressFileSystemRepository
codeFormatProperty

	^properties 
		at: '_gs_format'
		ifAbsent: ['Cypress']
%

category: 'updating properties'
method: CypressFileSystemRepository
codeFormatProperty: aString

	self validate: aString isOneOf: #('Cypress' 'FileTree' 'Flexible').
	properties 
		at: '_gs_format'
		put: aString
%

category: 'accessing'
method: CypressFileSystemRepository
description
  | desc |
  desc := super description.
  ^ desc notEmpty
    ifTrue: [ desc ]
    ifFalse: [ self url asString ]
%

category: 'accessing'
method: CypressFileSystemRepository
directoryPath

	^directoryPath
%

category: 'initializing - private'
method: CypressFileSystemRepository
directoryPath: aString
  | delim |
  delim := self fileUtils pathNameDelimiter.
  aString last = delim last
    ifTrue: [ directoryPath := aString ]
    ifFalse: [ directoryPath := aString , delim ]
%

category: 'testing - private'
method: CypressFileSystemRepository
doesRepositoryFileExist: fileName
	"Answer whether the named file exists at the repository level."

	^self fileUtils
		directoryExists: (self fileUtils
				directoryFromPath: fileName
				relativeTo: self directoryPath)
%

category: 'initializing - private'
method: CypressFileSystemRepository
ensureDirectoryPathExists

	self fileUtils ensureDirectoryExists: self directoryPath
%

category: 'accessing - private'
method: CypressFileSystemRepository
fileUtils

	^CypressFileUtilities current
%

category: 'initializing - private'
method: CypressFileSystemRepository
fixupMissingCopyrightProperty

	self copyrightProperty isEmpty ifFalse: [^self].
	self initializeDefaultCopyrightProperty.
%

category: 'initializing - private'
method: CypressFileSystemRepository
initializeCreationOn: aUrl alias: aString

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString;
		writePropertiesFile
%

category: 'initializing - private'
method: CypressFileSystemRepository
initializeDefaultRepositoryProperties

	super initializeDefaultRepositoryProperties.
	self
		codeFormatProperty: 'Cypress';
		strictCodeFormat: false.
%

category: 'initializing - private'
method: CypressFileSystemRepository
initializeForDirectory: aDirectory

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self readPropertiesFile.
	self fixupMissingCopyrightProperty.
	self initializeReaderAndWriterClasses.
%

category: 'initializing - private'
method: CypressFileSystemRepository
initializeForFileTreeRepository
  self initializeDefaultRepositoryProperties.
  self
    codeFormatProperty: 'FileTree';
    strictCodeFormat: true
%

category: 'initializing - private'
method: CypressFileSystemRepository
initializeReaderAndWriterClasses

	self isCodeFormatCypress
		ifTrue: 
			[self isCodeFormatStrict
				ifTrue: 
					[readerClass := CypressPackageReader.
					writerClass := CypressPackageWriter]
				ifFalse: 
					[readerClass := CypressFlexiblePackageReader.
					writerClass := CypressPackageWriter]]
		ifFalse: 
			[self isCodeFormatStrict
				ifTrue: 
					[readerClass := CypressFileTreeFormatPackageReader.
					writerClass := CypressStrictFileTreeFormatDoNothingPackageWriter]
				ifFalse: 
					[readerClass := CypressFlexiblePackageReader.
					writerClass := CypressFileTreeFormatPackageWriter]]
%

category: 'initializing - private'
method: CypressFileSystemRepository
initializeRepositoryDirectory
  self directoryPath: self url pathForDirectory.
  self ensureDirectoryPathExists
%

category: 'initializing - private'
method: CypressFileSystemRepository
initializeUrl: aUrl andAlias: aString
  super initializeUrl: aUrl andAlias: aString.
  self initializeRepositoryDirectory.
  self readPropertiesFile.
  self codeFormatProperty: self url codeFormat.
  self strictCodeFormat: self url isStrict.
  self fixupMissingCopyrightProperty.
  self initializeReaderAndWriterClasses
%

category: 'testing properties'
method: CypressFileSystemRepository
isCodeFormatCypress

	^self isCodeFormatProperty: 'Cypress'
%

category: 'testing properties'
method: CypressFileSystemRepository
isCodeFormatFileTree

	^self isCodeFormatProperty: 'FileTree'
%

category: 'testing properties'
method: CypressFileSystemRepository
isCodeFormatFlexiblyCypress

	^self isCodeFormatStrict not and: [self isCodeFormatCypress]
%

category: 'testing properties'
method: CypressFileSystemRepository
isCodeFormatFlexiblyFileTree

	^self isCodeFormatStrict not and: [self isCodeFormatFileTree]
%

category: 'testing properties - private'
method: CypressFileSystemRepository
isCodeFormatProperty: aString

	^(properties at: '_gs_format') equalsNoCase: aString
%

category: 'testing properties'
method: CypressFileSystemRepository
isCodeFormatStrict

	^(properties 
		at: '_gs_strict'
		ifAbsent: ['']) equalsNoCase: 'true'
%

category: 'testing properties'
method: CypressFileSystemRepository
isCodeFormatStrictlyCypress

	^self isCodeFormatStrict and: [self isCodeFormatCypress]
%

category: 'testing properties'
method: CypressFileSystemRepository
isCodeFormatStrictlyFileTree

	^self isCodeFormatStrict and: [self isCodeFormatFileTree]
%

category: 'accessing'
method: CypressFileSystemRepository
packageNames

	| extension extensionSize |
	extension := self packageExtension.
	extensionSize := extension size.
	^(self fileUtils
		directoryEntriesFrom: self directoryPath , '*' , extension) collect: 
				[:each |
				| localName |
				localName := self fileUtils localNameFrom: each.
				localName copyFrom: 1 to: localName size - extensionSize]
%

category: 'printing'
method: CypressFileSystemRepository
printDetailsOn: aStream
  self alias notEmpty
    ifTrue: [ aStream
        nextPutAll: self alias;
        nextPutAll: ': ' ].
  aStream nextPutAll: self url printString
%

category: 'accessing'
method: CypressFileSystemRepository
properties
  ^ properties
%

category: 'reading'
method: CypressFileSystemRepository
readPropertiesFile

	self readPropertiesFile: (#('properties.ston' 'properties.json' '.filetree')
				detect: [:each | self doesRepositoryFileExist: each]
				ifNone: [^self]).
%

category: 'reading'
method: CypressFileSystemRepository
readPropertiesFile: fileName
	"Expect 'properties.ston' for Cypress, but permit 'properties.json' in which case we assume
	 the format should be FileTree.

	 Supported properties are:
		_cypress_copyright	- the (optional, default) copyright notice for the whole repository
		_gs_format			- optional, determines which format will be used for writing packages (and reading, but ...)
							- either Cypress or FileTree (case-insensitive)
		_gs_strict			- optional, determines whether the reader strictly enforces the format
							- either true or false (case-insensitive)
		_gs_fileout			- optional, determines whether to also produce a *.gs fileout when writing a package
							- either true or false (case-insensitive)
	"

	fileName = '.filetree' ifTrue: [^self initializeForFileTreeRepository].
	self fileUtils
		readStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties := CypressJsonParser parseStream: fileStream]
%

category: 'updating properties'
method: CypressFileSystemRepository
strictCodeFormat: aBoolean

	self strictCodeFormatProperty: aBoolean printString

%

category: 'updating properties - private'
method: CypressFileSystemRepository
strictCodeFormatProperty: aString

	self validate: aString isOneOf: #('true' 'false').
	properties 
		at: '_gs_strict'
		put: aString
%

category: 'updating properties - private'
method: CypressFileSystemRepository
validate: aString isOneOf: someStrings

	someStrings
		detect: [:each | aString equalsNoCase: each]
		ifNone: [self error: aString printString, ' must be one of ', someStrings printString].
%

category: 'validating - private'
method: CypressFileSystemRepository
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']
%

category: 'writing'
method: CypressFileSystemRepository
writePropertiesFile

	self writePropertiesFile: (self isCodeFormatFileTree
				ifTrue: ['properties.json']
				ifFalse: ['properties.ston'])
%

category: 'writing'
method: CypressFileSystemRepository
writePropertiesFile: fileName

	self fileUtils
		writeStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties _writeCypressJsonOn: fileStream]
%

! Class implementation for 'CypressFileSystemGitRepository'

!		Class methods for 'CypressFileSystemGitRepository'

category: 'accessing'
classmethod: CypressFileSystemGitRepository
gitRepositoryDir
  | path |
  path := (SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' otherwise: nil)
    ifNil: [
      path := (System gemEnvironmentVariable: 'GEMSTONE_GITDIR')
        ifNil: [ 
          CypressFileUtilities current workingDirectory
          , CypressFileUtilities current pathNameDelimiter , 'cypress-git-repos' ].
      SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' put: path ].
  ^path
%

category: 'accessing'
classmethod: CypressFileSystemGitRepository
gitRepositoryDir: directoryPath
  ^ SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' put: directoryPath
%

category: 'git commands'
classmethod: CypressFileSystemGitRepository
performOnServer: commandLine
  | result |
  result := self
    performOnServer: commandLine
    status: [ :performOnServerStatusArray | "Array of 5 elements: 
       raw status Integer, 
       child process status Integer (after WEXITSTATUS macro applied), 
       result String (or nil if operation failed) ,
       error string from script file write, fork, or result file read ,
       errno value, a SmallInteger from file write, fork, or file read"
      (performOnServerStatusArray at: 1) ~~ 0
        ifTrue: [ | message |
          message := 'performOnServer: ' , commandLine printString , ' stdout: '
            , (performOnServerStatusArray at: 3) printString
            , ' failed with status: '
            , (performOnServerStatusArray at: 1) printString , ' errno: '
            , (performOnServerStatusArray at: 5) printString , ' errStr: '
            , (performOnServerStatusArray at: 4) asString.
          self error: message ].
      performOnServerStatusArray at: 3 ].
  Transcript
    cr;
    show: commandLine printString;
    cr;
    show: result.
  ^ result
%

category: 'git commands'
classmethod: CypressFileSystemGitRepository
performOnServer: commandLine status: statusBlock
  | performOnServerStatusArray |
  performOnServerStatusArray := System _performOnServer: commandLine.
  ^ statusBlock value: performOnServerStatusArray
%

category: 'git commands'
classmethod: CypressFileSystemGitRepository
runGitCommand: argsArray in: gitRootPath

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream nextPutAll: 'cd ' , gitRootPath , '; git '.
	argsArray do: 
			[:arg |
			stream
				space;
				nextPutAll: arg].
	^self performOnServer: stream contents
%

!		Instance methods for 'CypressFileSystemGitRepository'

category: 'accessing'
method: CypressFileSystemGitRepository
currentBranch
  | result |
  result := self
    gitCommand: #('rev-parse' '--abbrev-ref' 'HEAD')
    in: self directoryPath.
  ^ result trimWhiteSpace
%

category: 'git querying'
method: CypressFileSystemGitRepository
gitCloneRepositoryAndCheckoutIn: aDirectoryName workingDirectory: workingDirectory
  "Do a clone on the remote repository and do a checkout on it to get the right branch. Complement the directory as well with the working directory."

  | gitDir branch |
  gitDir := workingDirectory , self fileUtils pathNameDelimiter
    , (aDirectoryName subStrings: '/') last.
  branch := self projectBranchOrTag.
  (self fileUtils directoryExists: gitDir)
    ifTrue: [ | targetDirBranch |
      self directoryPath: (self repositoryPath isEmpty
            ifTrue: [ gitDir ]
            ifFalse: [ gitDir , self fileUtils pathNameDelimiter , self repositoryPath ]).
      targetDirBranch := self currentBranch.
      targetDirBranch = branch
        ifFalse: [ self error: self class name asString
                ,
                  ' target directory already exists and is on another branch, cancelling clone and repository instanciation : '
                , gitDir ] ]
    ifFalse: [ self gitCommand: #('clone') , {'-b'.
              branch} , {remoteUrl.
              gitDir} in: workingDirectory.
      self directoryPath: (self repositoryPath isEmpty
            ifTrue: [ gitDir ]
            ifFalse: [ gitDir , self fileUtils pathNameDelimiter , self repositoryPath ]) ]
%

category: 'git querying'
method: CypressFileSystemGitRepository
gitCommand: aCommandString in: aDirectory
  ^ self class runGitCommand: aCommandString in: aDirectory
%

category: 'initializing - private'
method: CypressFileSystemGitRepository
initializeRepositoryDirectory
  self remoteUrl: self url httpsAccessString.
  self
    gitCloneRepositoryAndCheckoutIn: self projectPath
    workingDirectory: self class gitRepositoryDir.
  (self isGitRepository: self directoryPath)
    ifFalse: [ self error: 'This url is not a git repository' , self url printString ]
%

category: 'git querying'
method: CypressFileSystemGitRepository
isGitRepository: aDirectory
  "Check that we have a git repository"

  | gitPath |
  gitPath := self gitCommand: #('rev-parse' '--show-toplevel') in: aDirectory.
  (gitPath indexOfSubCollection: 'fatal:' startingAt: 1 ifAbsent: [ 0 ]) = 1
    ifTrue: [ ^ false ].
  ^ true
%

category: 'accessing'
method: CypressFileSystemGitRepository
projectBranchOrTag
  "right now only expect to work with branches"

  ^ self url projectBranchOrTag
%

category: 'accessing'
method: CypressFileSystemGitRepository
projectPath
  ^ self url projectPath
%

category: 'accessing'
method: CypressFileSystemGitRepository
remoteUrl

   ^remoteUrl
%

category: 'accessing'
method: CypressFileSystemGitRepository
remoteUrl: anObject

   remoteUrl := anObject
%

category: 'accessing'
method: CypressFileSystemGitRepository
repositoryPath
  ^ self url repositoryPath
%

category: 'validating - private'
method: CypressFileSystemGitRepository
validateUrl
  "At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

  (self url projectPath isEmpty or: [ self url projectBranchOrTag isEmpty ])
    ifTrue: [ self error: self printString , ' should not be used with non-git URLs.' ]
%

! Class implementation for 'CypressFileUtilities'

!		Class methods for 'CypressFileUtilities'

category: 'accessing'
classmethod: CypressFileUtilities
current

	^Current
%

category: 'utilities'
classmethod: CypressFileUtilities
deleteAll: aDirectory

	self subclassResponsibility: #deleteAll:
%

category: 'utilities'
classmethod: CypressFileUtilities
deleteAll: aDirectory rejecting: rejectBlock
  self subclassResponsibility: #'deleteAll:rejecting:'
%

category: 'utilities'
classmethod: CypressFileUtilities
directoryEntriesFrom: aDirectory

	self subclassResponsibility: #directoryEntriesFrom:
%

category: 'utilities'
classmethod: CypressFileUtilities
directoryExists: aDirectory

	self subclassResponsibility: #directoryExists:
%

category: 'unknown'
classmethod: CypressFileUtilities
directoryFileNamesAndContents: aDirectory

	self subclassResponsibility: #directoryFileNamesAndContents:
%

category: 'utilities'
classmethod: CypressFileUtilities
directoryFromPath: directoryPath relativeTo: aDirectory

	self subclassResponsibility: #directoryFromPath:relativeTo:
%

category: 'utilities'
classmethod: CypressFileUtilities
ensureDirectoryExists: aDirectory

	self subclassResponsibility: #ensureDirectoryExists:
%

category: 'initializating'
classmethod: CypressFileUtilities
install

	Current := self
%

category: 'utilities'
classmethod: CypressFileUtilities
localNameFrom: aDirectory

	self subclassResponsibility: #localNameFrom:
%

category: 'utilities'
classmethod: CypressFileUtilities
pathNameDelimiter

	self subclassResponsibility: #pathNameDelimiter
%

category: 'utilities'
classmethod: CypressFileUtilities
readStreamFor: filePath do: aOneArgBlock

	self subclassResponsibility: #readStreamFor:do:
%

category: 'utilities'
classmethod: CypressFileUtilities
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	self subclassResponsibility: #readStreamFor:in:do:
%

category: 'utilities'
classmethod: CypressFileUtilities
workingDirectory

	self subclassResponsibility: #workingDirectory
%

category: 'utilities'
classmethod: CypressFileUtilities
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	self subclassResponsibility: #writeStreamFor:in:do:
%

! Class implementation for 'CypressGemStoneDirectoryUtilities'

!		Class methods for 'CypressGemStoneDirectoryUtilities'

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
deleteAll: aDirectory
  "Delete all the files and directories under the named directory.
	 Ensure we don't try to recursively delete . or .."

  self deleteAll: aDirectory rejecting: [ :filename | false ]
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
deleteAll: aDirectory rejecting: rejectBlock
	"Delete all the files and directories under the named directory.
       Reject file and directores in aDirectory that are rejected by rejectBlock.
       The rejectBlock is not used recursively.
       Ensure we don't try to recursively delete . or .."

	| filename isFile |
	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
		doWithIndex: 
			[:each :index |
			index odd
				ifTrue: [filename := each]
				ifFalse: 
					[isFile := each.
					isFile
						ifTrue: 
							[(rejectBlock value: filename)
								ifFalse: 
									[(rejectBlock value: filename) ifFalse: [GsFile removeServerFile: filename]]]
						ifFalse: 
							[(self endsWithSpecial: filename)
								ifFalse: 
									[(rejectBlock value: filename)
										ifFalse: 
											[self deleteAll: filename rejecting: rejectBlock.
											GsFile removeServerDirectory: filename]]]]]
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
directoryEntriesFrom: aDirectory
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
directoryExists: aDirectory

	"handle the case where GsFile class>>existsOnServer: returns nil"
	^ (GsFile existsOnServer: aDirectory) == true
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
directoryFileNamesAndContents: aDirectory
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

	| map |
	map := Dictionary new.
	self directoryFileNamesAndContents: aDirectory into: map.
	^map.
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
directoryFileNamesAndContents: aDirectory into: aDictionary
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

	| filename isFile |
	(GsFile contentsAndTypesOfDirectory: aDirectory onClient: false)
		doWithIndex: 
			[:each :index |
			index odd
				ifTrue: [filename := each]
				ifFalse: 
					[isFile := each.
					isFile
						ifTrue: 
							[| file |
							file := GsFile openReadOnServer: filename.
							file isNil
								ifFalse: 
									[aDictionary at: filename put: file contents.
									file close]]
						ifFalse: 
							[(self endsWithSpecial: filename)
								ifFalse: [self directoryFileNamesAndContents: filename into: aDictionary]]]]
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
directoryFromPath: directoryPath relativeTo: aDirectory

	| delimiter |
	delimiter := self pathNameDelimiter.
	^(aDirectory last = delimiter last
		or: [(directoryPath indexOfSubCollection: delimiter startingAt: 1 ifAbsent: [ 0 ]) = 1])
			ifTrue: [aDirectory , directoryPath]
			ifFalse: [aDirectory , delimiter , directoryPath]
%

category: 'private'
classmethod: CypressGemStoneDirectoryUtilities
endsWithSpecial: filename
	"Answer true if the given filename ends with any of the special sequences
	'/..' '/.' '\..' '\.', false otherwise."

	| filenameSize finalChars |
	filenameSize := filename size.
	finalChars := filename copyFrom: filenameSize - 1 to: filenameSize.
	finalChars = '/.' ifTrue: [^true].
	finalChars = '\.' ifTrue: [^true].
	finalChars := filename copyFrom: filenameSize - 2 to: filenameSize.
	finalChars = '/..' ifTrue: [^true].
	finalChars = '\..' ifTrue: [^true].
	^false
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
ensureDirectoryExists: aDirectory

	| lastSeparator |
	(GsFile existsOnServer: aDirectory) == true ifTrue: [^aDirectory].
	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].
	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].
	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).
	self ensureDirectoryExists: aDirectory.
%

category: 'initializating'
classmethod: CypressGemStoneDirectoryUtilities
initialize
	"self initialize"

	self install
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
localNameFrom: aDirectory

	| endOfPath |
	endOfPath := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	^aDirectory copyFrom: endOfPath + 1 to: aDirectory size
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
pathNameDelimiter

	^'/'
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
readStreamFor: filePath do: aOneArgBlock

	| file stream blockResult |
	GsFile serverErrorString.
	file := GsFile openReadOnServer: filePath.
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	[stream := ReadStreamPortable on: (String withAll: file contents asByteArray decodeFromUTF8).
	blockResult := aOneArgBlock value: stream] ensure: [file close].
	^ blockResult
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
readStreamFor: filePath in: aDirectory do: aOneArgBlock

	^ self
		readStreamFor: (self directoryFromPath: filePath relativeTo: aDirectory)
		do: aOneArgBlock
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
workingDirectory

	^System gemEnvironmentVariable: 'PWD'
%

category: 'utilities'
classmethod: CypressGemStoneDirectoryUtilities
writeStreamFor: filePath in: aDirectory do: aOneArgBlock

	| file stream |
	GsFile serverErrorString.
	file := GsFile openWriteOnServer: (self directoryFromPath: filePath relativeTo: aDirectory).
	GsFile serverErrorString ifNotNil: [:errorMessage | self error: errorMessage].
	stream := WriteStreamPortable on: String new.
	[aOneArgBlock value: stream] ensure: [file nextPutAll: stream contents encodeAsUTF8; close]
%

! Class implementation for 'CypressGsGeneralDependencySorter'

!		Class methods for 'CypressGsGeneralDependencySorter'

category: 'instance creation'
classmethod: CypressGsGeneralDependencySorter
on: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock
	"Create an instance of the receiver capable for sorting the dependencies of someCandidates.
	 aOneArgBlock is used to evaluate the key of the object depended on for a candidate.
	 anotherOneArgBlock is used to evaluate the key of the candidate itself."

	^self new
		initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock;
		yourself.
%

!		Instance methods for 'CypressGsGeneralDependencySorter'

category: 'sorting - private'
method: CypressGsGeneralDependencySorter
determineGraphRoots
  ^ dependencyGraphs
    selectAssociations: [ :each | (candidateAliasMap includesKey: each key) not ]
%

category: 'initializing - private'
method: CypressGsGeneralDependencySorter
initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock

	candidates := someCandidates.
	dependsOnConverter := aOneArgBlock.
	dependentConverter := anotherOneArgBlock.
	individualDependencyMap := Dictionary new.
	dependencyGraphs := Dictionary new.
	candidateAliasMap := Dictionary new
%

category: 'sorting'
method: CypressGsGeneralDependencySorter
inOrder
  | sorted sortedRoots |
  sorted := OrderedCollection new.
  self mapCandidatesIntoGraphs.
  sortedRoots := SortedCollection sortBlock: [ :a :b | a key <= b key ].
  self determineGraphRoots associationsDo: [ :assoc | sortedRoots add: assoc ].
  sortedRoots do: [ :assoc | self transcribeGraph: assoc value into: sorted ].
  ^ sorted
%

category: 'sorting - private'
method: CypressGsGeneralDependencySorter
mapCandidatesIntoGraphs

	| dependsOnKey dependentKey |
	candidates do: 
			[:each |
			| individualDependency |
			dependsOnKey := dependsOnConverter value: each.
			dependentKey := dependentConverter value: each.
			candidateAliasMap at: dependentKey put: each.
			individualDependencyMap at: dependsOnKey ifAbsentPut: [Dictionary new].
			individualDependencyMap at: dependentKey ifAbsentPut: [Dictionary new].
			individualDependency := individualDependencyMap
						associationAt: dependsOnKey.
			(dependencyGraphs includesKey: dependsOnKey)
				ifFalse: [dependencyGraphs add: individualDependency].
			individualDependency value
				add: (individualDependencyMap associationAt: dependentKey)]
%

category: 'sorting - private'
method: CypressGsGeneralDependencySorter
transcribeGraph: subtree into: sorted
  (subtree keys asSortedCollection: [ :a :b | a <= b ])
    do: [ :name | | subsubtree |
      subsubtree := subtree at: name.
      sorted add: (candidateAliasMap at: name).
      self transcribeGraph: subsubtree into: sorted ]
%

! Class implementation for 'CypressJsonParser'

!		Class methods for 'CypressJsonParser'

category: 'instance creation'
classmethod: CypressJsonParser
new

	CypressJsonError signal: 'Instantiate the parser with a stream.'
%

category: 'instance creation'
classmethod: CypressJsonParser
on: aStream
	^ self basicNew initializeOn: aStream
%

category: 'accessing'
classmethod: CypressJsonParser
parse: aString
	^ self parseStream: aString readStream
%

category: 'accessing'
classmethod: CypressJsonParser
parseStream: aStream
	^ (self on: aStream) parse
%

!		Instance methods for 'CypressJsonParser'

category: 'adding'
method: CypressJsonParser
addProperty: anAssociation to: anObject
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself
%

category: 'adding'
method: CypressJsonParser
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject
%

category: 'creating'
method: CypressJsonParser
createArray
	"Create an empty collection. Subclasses might want to refine this implementation."

	^ Array new
%

category: 'creating'
method: CypressJsonParser
createFalse
	"Create the false literal. Subclasses might want to refine this implementation."
	
	^ false
%

category: 'creating'
method: CypressJsonParser
createNull
	"Create the null literal. Subclasses might want to refine this implementation."

	^ nil
%

category: 'creating'
method: CypressJsonParser
createObject
	"Create an empty object. Subclasses might want to refine this implementation."
	
	^ Dictionary new
%

category: 'creating'
method: CypressJsonParser
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue
%

category: 'creating'
method: CypressJsonParser
createString: aString
	"Create a string literal. Subclasses might want to refine this implementation."

	^ aString
%

category: 'creating'
method: CypressJsonParser
createTrue
	"Create the true literal. Subclasses might want to refine this implementation."

	^ true
%

category: 'private'
method: CypressJsonParser
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^(self match: aString)
		ifFalse: [CypressJsonError signal: aString , ' expected']
%

category: 'initialization'
method: CypressJsonParser
initializeOn: aStream
	stream := aStream
%

category: 'private'
method: CypressJsonParser
match: aString
	"Tries to match aString, consume input and answer true if successful."
	
	| position |
	position := stream position.
	aString do: [ :each |
		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 
			stream position: position.
			^ false ] ].
	self whitespace.
	^ true
%

category: 'parsing'
method: CypressJsonParser
parse

	| result |
	result := self
				whitespace;
				parseValue.
	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].
	^result
%

category: 'parsing'
method: CypressJsonParser
parseArray

	| result |
	self expect: '['.
	result := self createArray.
	(self match: ']') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addValue: self parseValue to: result.
			(self match: ']') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of array expected'
%

category: 'parsing-internal'
method: CypressJsonParser
parseCharacter
	| char |
	(char := stream next) = $\ 
		ifFalse: [ ^ char ].
	(char := stream next) = $" 
		ifTrue: [ ^ char ].
	char = $\
		ifTrue: [ ^ char ].
	char = $/
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
	CypressJsonError signal: 'invalid escape character \' , (String with: char)
%

category: 'parsing-internal'
method: CypressJsonParser
parseCharacterHex
  | value |
  value := self parseCharacterHexDigit.
  3 timesRepeat: [ value := (value bitShift: 4) + self parseCharacterHexDigit ].
  ^ Character codePoint: value
%

category: 'parsing-internal'
method: CypressJsonParser
parseCharacterHexDigit
    | digit |
    stream atEnd
        ifFalse: [ 
            digit := stream next codePoint.
            (digit between: 48 and: 57)
                ifTrue: [ ^ digit - 48 ].	"$0"	"$9"
            (digit between: 65 and: 70)
                ifTrue: [ ^ digit - 55 ].	"$A"	"$F"
            (digit between: 97 and: 102)
                ifTrue: [ ^ digit - 87 ]	"$a"	"$f" ].
    CypressJsonError signal: 'hex-digit expected'
%

category: 'parsing-internal'
method: CypressJsonParser
parseNumber
	| negated number |
	negated := stream peek = $-.
	negated ifTrue: [ stream next ].
	number := self parseNumberInteger.
	(stream peek = $.) ifTrue: [
		stream next. 
		number := number + self parseNumberFraction ].
	(stream peek = $e or: [ stream peek = $E ]) ifTrue: [
		stream next.
		number := number * self parseNumberExponent ].
	negated ifTrue: [ number := number negated ].
	self whitespace.
	^ number
%

category: 'parsing-internal'
method: CypressJsonParser
parseNumberExponent
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number
%

category: 'parsing-internal'
method: CypressJsonParser
parseNumberFraction
    | number power |
    number := 0.
    power := 1.0.
    [ stream atEnd not and: [ stream peek isDigit ] ]
        whileTrue: [ 
            number := 10 * number + (stream next codePoint - 48).
            power := power * 10.0 ].
    ^ number / power
%

category: 'parsing-internal'
method: CypressJsonParser
parseNumberInteger
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    ^ number
%

category: 'parsing'
method: CypressJsonParser
parseObject

	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addProperty: self parseProperty to: result.
			(self match: '}') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of object expected'
%

category: 'parsing-internal'
method: CypressJsonParser
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.
%

category: 'parsing-internal'
method: CypressJsonParser
parseString
	| result |
	self expect: '"'.
	result := WriteStreamPortable on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents
%

category: 'parsing'
method: CypressJsonParser
parseValue
	| char |
	stream atEnd ifFalse: [ 
		char := stream peek.
		char = ${
			ifTrue: [ ^ self parseObject ].
		char = $[
			ifTrue: [ ^ self parseArray ].
		char = $"
			ifTrue: [ ^ self parseString ].
		(char = $- or: [ char between: $0 and: $9 ])
			ifTrue: [ ^ self parseNumber ].
		(self match: 'true')
			ifTrue: [ ^ self createTrue ].
		(self match: 'false')
			ifTrue: [ ^ self createFalse ].
		(self match: 'null')
			ifTrue: [ ^ self createNull ] ].
	CypressJsonError signal: 'invalid input'
%

category: 'private'
method: CypressJsonParser
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]
%

! Class implementation for 'CypressObject'

!		Class methods for 'CypressObject'

category: 'miscellany'
classmethod: CypressObject
collection: aCollection gather: aOneArgBlock
	"Evaluate the block once for each element of aCollection. The block should answer a collection. 
	Answer an Array containing all elements of all the answered collections."

	| result |
	result := Array new.
	aCollection do: [:each | result addAll: (aOneArgBlock value: each)].
	^result
%

category: 'miscellany'
classmethod: CypressObject
elementsIn: sourceCollection butNotIn: exclusionCollection

	| exclusionSet |
	exclusionSet := exclusionCollection asSet.
	^sourceCollection reject: [:each | exclusionSet includes: each]
%

category: 'miscellany'
classmethod: CypressObject
elementsInBoth: collection1 and: collection2
	"Set intersection generalized to any collections."

	| temporarySet |
	temporarySet := collection2 asSet.
	^collection1 select: [:each | temporarySet includes: each]
%

category: 'converting'
classmethod: CypressObject
normalizeLineEndingsOf: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example.

	 GemStone Smalltalk uses the Unix line ending of LF."

	| cr lf inPos outPos outString newOutPos indexLF indexCR |
	cr := Character cr.
	indexCR := aString indexOf: cr startingAt: 1.
	indexCR = 0 ifTrue: [^aString].
	lf := Character lf.
	indexLF := aString indexOf: lf startingAt: 1.
	indexLF = 0 ifTrue: [^aString copyReplacing: cr with: lf].
	inPos := outPos := 1.
	outString := String new: aString size.
	
	["check if next CR is before next LF or if there are no more LF"
	(indexLF = 0 or: [indexCR < indexLF])
		ifTrue: 
			[newOutPos := outPos + 1 + indexCR - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: aString
				startingAt: inPos.
			outString at: newOutPos - 1 put: lf.
			outPos := newOutPos.
			1 + indexCR = indexLF
				ifTrue: 
					["Caught a CR-LF pair"
					inPos := 1 + indexLF.
					indexLF := aString indexOf: lf startingAt: inPos]
				ifFalse: [inPos := 1 + indexCR].
			indexCR := aString indexOf: cr startingAt: inPos]
		ifFalse: 
			[newOutPos := outPos + 1 + indexLF - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 1
				with: aString
				startingAt: inPos.
			outPos := newOutPos.
			inPos := 1 + indexLF.
			indexLF := aString indexOf: lf startingAt: inPos].
	indexCR = 0]
			whileFalse.

	"no more CR line endings. copy the rest"
	newOutPos := outPos + (aString size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: aString
		startingAt: inPos.
	^outString copyFrom: 1 to: newOutPos - 1
%

!		Instance methods for 'CypressObject'

category: 'accessing'
method: CypressObject
allClasses

	| classes |
	classes := Array new.
	self symbolList
		do: [:dict | classes addAll: (dict select: [:each | each isBehavior])].
	^classes
%

category: 'private'
method: CypressObject
anyElementOf: aCollection ifEmpty: aBlock

	aCollection do: [:each | ^each].
	^aBlock value
%

category: 'accessing'
method: CypressObject
classesInPackageNamed: aString

	| packageName classes |
	packageName := aString asLowercase.
	classes := Array new.
	self symbolList do: 
			[:dict |
			classes
				addAll: (dict select: 
							[:each |
							each isBehavior and: 
									[| candidateName |
									candidateName := each category asLowercase.
									candidateName = packageName
										or: [(candidateName indexOfSubCollection: packageName , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]]])].
	^classes sortAscending: #('name')
%

category: 'miscellany'
method: CypressObject
collection: aCollection gather: aOneArgBlock
	"Evaluate the block once for each element of aCollection. The block should answer a collection. 
	Answer an Array containing all elements of all the answered collections."

	^self class collection: aCollection gather: aOneArgBlock
%

category: 'initializing'
method: CypressObject
defaultSymbolDictionaryName
  "Name of the SymbolDictionary where new classes should be installed"

  ^ #'UserGlobals'
%

category: 'sorting'
method: CypressObject
determineClassHierarchicalOrder: someClasses
	"Returns an ordered collection of the specified classes such that
	 hierarchical dependencies come first."
	"Not sure whether we ever get non-behaviors. 
	The previous, more complex, version of this method contained this filter."

	| order toBeOrdered processed aClass |
	toBeOrdered := (someClasses select: [:each | each isBehavior])
				asIdentitySet.
	order := OrderedCollection new.
	processed := IdentitySet new.
	[(aClass := self anyElementOf: toBeOrdered ifEmpty: [nil]) isNil]
		whileFalse: 
			[self
				orderBySuperclass: aClass
				from: toBeOrdered
				into: order
				ignoring: processed].
	^order
%

category: 'miscellany'
method: CypressObject
elementsIn: sourceCollection butNotIn: exclusionCollection

	^self class elementsIn: sourceCollection butNotIn: exclusionCollection
%

category: 'miscellany'
method: CypressObject
elementsInBoth: collection1 and: collection2
	"Set intersection generalized to any collections."

	^self class elementsInBoth: collection1 and: collection2
%

category: 'initializing'
method: CypressObject
initialize
	"Placeholder: #initialize is not defined by Object in GemStone Smalltalk."
%

category: 'converting'
method: CypressObject
normalizeLineEndingsOf: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example."

	^self class normalizeLineEndingsOf: aString.
%

category: 'private'
method: CypressObject
orderBySuperclass: aClass from: toBeOrdered into: order ignoring: processed
	"Private. Add to 'order', superclasses first, aClass and any of its superclasses 
	that appear in 'toBeOrdered' but do not appear in 'processed'.
	Remove from 'toBeOrdered' any class added to 'ordered'.
	Any class seen, add to 'processed' whether or not added to 'order'."

	| superclass |
	superclass := aClass superclass.
	superclass isNil | (processed includes: superclass)
		ifFalse: 
			[self
				orderBySuperclass: superclass
				from: toBeOrdered
				into: order
				ignoring: processed].
	processed add: aClass.
	(toBeOrdered includes: aClass)
		ifTrue: 
			[toBeOrdered remove: aClass.
			order add: aClass]
%

category: 'printing'
method: CypressObject
printDetailsOn: aStream
%

category: 'printing'
method: CypressObject
printOn: aStream

	| className |
	className := self class name.
	aStream
		nextPutAll: (className first isVowel ifTrue:[ 'an ' ] ifFalse:[ 'a ' ]);
		nextPutAll: className;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'.
%

category: 'accessing'
method: CypressObject
resolveGlobalNamed: aString

	^self resolveGlobalNamed: aString
		or: [CypressError signal: 'Could not resolve global named ' , aString printString]
%

category: 'accessing'
method: CypressObject
resolveGlobalNamed: aString or: aBlock

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value
%

category: 'converting'
method: CypressObject
stringForVariables: variableList

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each]
		separatedBy: [stream space].
	^stream contents
%

category: 'accessing'
method: CypressObject
symbolDictionaryForClassNamed: aString
  "Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), put it in UserGlobals."

  ^ self
    symbolDictionaryForClassNamed: aString
    or: [ System myUserProfile objectNamed: self defaultSymbolDictionaryName ]
%

category: 'accessing'
method: CypressObject
symbolDictionaryForClassNamed: aString or: aBlock
	"Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), answer
	 the result of evaluating aBlock."

	^self symbolList asArray
		detect: [:each | each anySatisfy: [:every | every isBehavior and: [every name asString = aString asString]]]
		ifNone: aBlock
%

category: 'accessing'
method: CypressObject
symbolList
  ^ System myUserProfile symbolList
%

! Class implementation for 'CypressAbstractPackageInformation'

!		Class methods for 'CypressAbstractPackageInformation'

category: 'Instance Creation'
classmethod: CypressAbstractPackageInformation
named: aString

	^self new
		initializeWithName: aString;
		yourself
%

!		Instance methods for 'CypressAbstractPackageInformation'

category: 'Initializing - private'
method: CypressAbstractPackageInformation
initialize
%

category: 'Initializing - private'
method: CypressAbstractPackageInformation
initializeWithName: aString

	self initialize.
	self name: aString
%

category: 'Testing'
method: CypressAbstractPackageInformation
isKnown

	^false
%

category: 'Accessing'
method: CypressAbstractPackageInformation
name

	^name
%

category: 'Updating'
method: CypressAbstractPackageInformation
name: aString

	name := aString
%

category: 'Printing'
method: CypressAbstractPackageInformation
printDetailsOn: aStream

	aStream nextPutAll: self name
%

category: 'Accessing'
method: CypressAbstractPackageInformation
repositories

	^#()
%

! Class implementation for 'CypressConflictingPackageInformation'

!		Class methods for 'CypressConflictingPackageInformation'

category: 'Instance Creation'
classmethod: CypressConflictingPackageInformation
fromUnknown: unknownPackageInformation conflictingWith: knownPackageInformation

	^(self named: unknownPackageInformation name)
		conflictsWith: knownPackageInformation;
		yourself.
%

!		Instance methods for 'CypressConflictingPackageInformation'

category: 'Accessing'
method: CypressConflictingPackageInformation
conflictsWith

	^conflictsWith
%

category: 'Updating'
method: CypressConflictingPackageInformation
conflictsWith: someCypressKnownPackageInformations

	conflictsWith := someCypressKnownPackageInformations
%

! Class implementation for 'CypressEclipsedPackageInformation'

!		Class methods for 'CypressEclipsedPackageInformation'

category: 'Instance Creation'
classmethod: CypressEclipsedPackageInformation
fromUnknown: unknownPackageInformation eclipsedBy: knownPackageInformation

	^(self named: unknownPackageInformation name)
		eclipsedBy: knownPackageInformation;
		yourself.
%

!		Instance methods for 'CypressEclipsedPackageInformation'

category: 'Accessing'
method: CypressEclipsedPackageInformation
eclipsedBy

	^eclipsedBy
%

category: 'Updating'
method: CypressEclipsedPackageInformation
eclipsedBy: aPackageInformation

	eclipsedBy := aPackageInformation
%

! Class implementation for 'CypressKnownPackageInformation'

!		Class methods for 'CypressKnownPackageInformation'

category: 'Instance Creation'
classmethod: CypressKnownPackageInformation
fromUnknown: aPackageInformation

	^self named: aPackageInformation name
%

!		Instance methods for 'CypressKnownPackageInformation'

category: 'Updating'
method: CypressKnownPackageInformation
addRepository: aRepository

	self repositories at: aRepository url put: aRepository.
	self updateDigestsFromImageAndRepository: aRepository.
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
determineDigestFromImage

	^self packageStructure digest
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
determineDigestFromRepository: aRepository

	^ (aRepository readPackageStructureForPackageNamed: self name) digest.
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
digestFor: source

	^self digestFor: source or: [nil]
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
digestFor: source or: aBlock

	^self digests at: source ifAbsent: aBlock
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
digests

	^digests
%

category: 'Initializing - private'
method: CypressKnownPackageInformation
digests: anIdentityDictionary
	"Key: Source of package definitions (#IMAGE or a Repository url)
	 Value: (e.g. MD5) Digest of the package's defintions or an empty string.
	 A nil digest means there are no definitions for the given source."

   digests := anIdentityDictionary
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
imageDigest

	^self digestFor: #IMAGE.
%

category: 'Initializing - private'
method: CypressKnownPackageInformation
initialize

	super initialize.
	self
		repositories: IdentityDictionary new;
		digests: IdentityDictionary new.
%

category: 'Testing'
method: CypressKnownPackageInformation
isKnown

	^true
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
packageStructure

	^CypressPackageStructure
		fromPackage: (CypressPackageDefinition named: self name)
%

category: 'Updating'
method: CypressKnownPackageInformation
removeRepository: aRepository

	self repositories removeKey: aRepository url ifAbsent: [].
	self digests removeKey: aRepository url ifAbsent: [].
	self updateDigestsFromImage.
%

category: 'Accessing'
method: CypressKnownPackageInformation
repositories

	^repositories
%

category: 'Initializing - private'
method: CypressKnownPackageInformation
repositories: anIdentityDictionary
	"Key: Repository url
	 Value: Repository"

	repositories := anIdentityDictionary
%

category: 'Accessing - digests'
method: CypressKnownPackageInformation
repositoryDigests

	^self repositories collect: [:each | self digestFor: each url]
%

category: 'Updating - digests'
method: CypressKnownPackageInformation
updateDigestsFromAllRepositories

	self repositories do: [:each | self updateDigestsFromRepository: each]
%

category: 'Updating - digests'
method: CypressKnownPackageInformation
updateDigestsFromImage

	self digests at: #IMAGE put: self determineDigestFromImage
%

category: 'Updating - digests'
method: CypressKnownPackageInformation
updateDigestsFromImageAndAllRepositories

	self
		updateDigestsFromImage;
		updateDigestsFromAllRepositories
%

category: 'Updating - digests'
method: CypressKnownPackageInformation
updateDigestsFromImageAndRepository: aRepository

	self
		updateDigestsFromImage;
		updateDigestsFromRepository: aRepository
%

category: 'Updating - digests'
method: CypressKnownPackageInformation
updateDigestsFromRepository: aRepository

	self digests at: aRepository url put: (self determineDigestFromRepository: aRepository).
%

category: 'Writing'
method: CypressKnownPackageInformation
writeChangesToAllRepositories

	| imageDigest changedDigests changedRepositories |
	self updateDigestsFromImageAndAllRepositories.
	imageDigest := self imageDigest.
	changedDigests := self repositoryDigests reject: [:each | each = imageDigest].
	changedRepositories := changedDigests keys collect: [:each | self repositories at: each].
	self writePackageToRepositories: changedRepositories.
%

category: 'Writing'
method: CypressKnownPackageInformation
writePackageToRepositories: someRepositories

	| packageStructure |
	packageStructure := self packageStructure.
	^someRepositories
		do: [:each | each writePackageStructure: packageStructure]
%

! Class implementation for 'CypressDefinition'

!		Instance methods for 'CypressDefinition'

category: 'comparing'
method: CypressDefinition
= aDefinition

	^(aDefinition isKindOf: CypressDefinition)
		and: [aDefinition description = self description]
%

category: 'loading'
method: CypressDefinition
actualClass

	self subclassResponsibility: #actualClass
%

category: 'visiting'
method: CypressDefinition
classDefinition: classBlock methodDefinition: methodBlock
	"default is noop"
%

category: 'accessing'
method: CypressDefinition
description
	self subclassResponsibility: #description
%

category: 'accessing'
method: CypressDefinition
details

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self printDetailsOn: stream.
	^stream contents
%

category: 'comparing'
method: CypressDefinition
hash
    ^ self description hash
%

category: 'testing'
method: CypressDefinition
isSameRevisionAs: aDefinition
	^ self = aDefinition
%

category: 'loading'
method: CypressDefinition
loadClassDefinition
  self loadClassDefinition: self defaultSymbolDictionaryName
%

category: 'loading'
method: CypressDefinition
loadClassDefinition: aDefaultSymbolDictionaryName
  "default is to do nothing"
%

category: 'loading'
method: CypressDefinition
loadMethodDefinition
	"default is to do nothing"
%

category: 'loading'
method: CypressDefinition
postLoad
	"noop"
%

category: 'loading'
method: CypressDefinition
postLoadOver: aDefinition

	self postLoad
%

category: 'dependency'
method: CypressDefinition
provisions
	"Answer list of global names defined by this definition"

	^#()
%

category: 'dependency'
method: CypressDefinition
requirements
	"Answer list of global names required by this definition"

	^#()
%

category: 'loading'
method: CypressDefinition
unloadDefinition

	self subclassResponsibility: #unloadDefinition
%

! Class implementation for 'CypressClassDefinition'

!		Class methods for 'CypressClassDefinition'

category: 'instance creation'
classmethod: CypressClassDefinition
forClass: aClass

	| superclassname |
	superclassname := aClass superclass
				ifNil: ['nil']
				ifNotNil: [:sClass | sClass name].
	^self
		name: aClass name
		superclassName: superclassname
		category: aClass category
		instVarNames: aClass instVarNames
		classInstVarNames: aClass class instVarNames
		classVarNames: aClass classVarNames
		poolDictionaryNames: aClass sharedPools
		comment: aClass comment
		subclassType: (self subclassTypeOf: aClass)
%

category: 'private'
classmethod: CypressClassDefinition
subclassTypeOf: aClass
	"Answer a description of the argument to identify whether it is a regular class,
	 a byte subclass, or an indexable subclass."

	^(aClass isBytes and: [aClass superClass isBytes not])
		ifTrue: ['byteSubclass']
		ifFalse: 
			[(aClass isIndexable and: [aClass superClass isIndexable not])
				ifTrue: ['indexableSubclass']
				ifFalse: ['']]
%

!		Instance methods for 'CypressClassDefinition'

category: 'comparing'
method: CypressClassDefinition
= aDefinition
  ^ super = aDefinition
    and: [ 
      superclassName = aDefinition superclassName
        and: [ 
          category = aDefinition category
            and: [ 
              instVarNames = aDefinition instVarNames
                and: [ 
                  classInstVarNames = aDefinition classInstVarNames
                    and: [ 
                      classVarNames asSortedCollection = aDefinition classVarNames asSortedCollection
                        and: [ 
                          poolDictionaryNames = aDefinition poolDictionaryNames
                            and: [ comment = aDefinition comment ] ] ] ] ] ] ]
%

category: 'loading'
method: CypressClassDefinition
actualClass

	^self resolveGlobalNamed: self name
%

category: 'loading'
method: CypressClassDefinition
actualClassOrNil

	^self resolveGlobalNamed: self name or: [nil]
%

category: 'converting'
method: CypressClassDefinition
asCypressClassDefinition

	^self
%

category: 'accessing'
method: CypressClassDefinition
category

	^category
%

category: 'visiting'
method: CypressClassDefinition
classDefinition: classBlock methodDefinition: methodBlock

	classBlock value: self
%

category: 'accessing'
method: CypressClassDefinition
classDefinitionCreationString

	| stream symbolDict |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPutAll: '(CypressClassDefinition';
		lf;
		tab;
		nextPutAll: 'name: ' , self className printString;
		lf;
		tab;
		nextPutAll: 'superclassName: ' , self superclassName printString;
		lf;
		tab;
		nextPutAll: 'category: ' , self category printString;
		lf;
		tab;
		nextPutAll: 'instVarNames: #(' , self instanceVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'classInstVarNames: #(' , self classInstanceVariablesString
					, ')';
		lf;
		tab;
		nextPutAll: 'classVarNames: #(' , self classVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'poolDictionaryNames: #(' , self poolDictionariesString , ')';
		lf;
		tab;
		nextPutAll: 'comment: ' , self comment printString;
		lf;
		tab;
		nextPutAll: 'subclassType: ' , self subclassType printString , ')';
		lf;
		tab;
		tab;
		yourself.
	symbolDict := self symbolDictionaryForClassNamed: self className.
	self actualClassOrNil isNil
		ifTrue: 
			[stream
				nextPutAll: 'loadClassDefinition.';
				yourself]
		ifFalse: 
			[stream
				nextPutAll: 'loadClassDefinition: ' , symbolDict name asString printString
							, '.';
				yourself].
	^stream contents
%

category: 'private'
method: CypressClassDefinition
classInstanceVariablesString
  ^ self stringForVariables: self classInstVarNames
%

category: 'accessing'
method: CypressClassDefinition
classInstVarNames
  ^ classInstVarNames
%

category: 'accessing'
method: CypressClassDefinition
className

	^self name
%

category: 'loading'
method: CypressClassDefinition
classNeedingMigration: aClass
  "right now we will create classes without doing a migration ..."

  
%

category: 'private'
method: CypressClassDefinition
classVariablesString
  ^ self stringForVariables: self classVarNames asSortedCollection
%

category: 'accessing'
method: CypressClassDefinition
classVarNames
  ^ classVarNames sort
%

category: 'accessing'
method: CypressClassDefinition
comment

	^comment
%

category: 'loading'
method: CypressClassDefinition
createOrReviseByteClass
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		byteSubclass: self name
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: self poolDictionaryList
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment
%

category: 'loading'
method: CypressClassDefinition
createOrReviseIndexableClass
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		indexableSubclass: self name
		instVarNames: (self instVarNames collect: [:each | each asSymbol])
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: self poolDictionaryList
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment
%

category: 'loading'
method: CypressClassDefinition
createOrReviseRegularClass
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		subclass: self name
		instVarNames: (self instVarNames collect: [:each | each asSymbol])
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: self poolDictionaryList
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment
%

category: 'accessing'
method: CypressClassDefinition
defaultSymbolDictionaryName
  ^ defaultSymbolDictionaryName ifNil: [ super defaultSymbolDictionaryName ]
%

category: 'accessing'
method: CypressClassDefinition
defaultSymbolDictionaryName: aSymbol
  defaultSymbolDictionaryName := aSymbol
%

category: 'accessing'
method: CypressClassDefinition
description

	^ Array with: name
%

category: 'loading'
method: CypressClassDefinition
failedCompiledMethods: someCompiledMethods

	someCompiledMethods isEmpty ifTrue: [^self].
	self halt: 'not implemented yet'
%

category: 'accessing'
method: CypressClassDefinition
gs_constraints

	^gs_constraints ifNil: [ gs_constraints := #() ]
%

category: 'accessing'
method: CypressClassDefinition
gs_constraints: aCollection

	gs_constraints := aCollection
%

category: 'accessing'
method: CypressClassDefinition
gs_options

	^gs_options ifNil: [ gs_options := #() ]
%

category: 'accessing'
method: CypressClassDefinition
gs_options: aCollection

	gs_options := aCollection
%

category: 'comparing'
method: CypressClassDefinition
hash

	| hash |
	hash := name hash.
	hash := superclassName hash bitOr: hash.
	hash := (category ifNil: ['']) hash bitOr: hash.
	instVarNames , classInstVarNames, classVarNames, poolDictionaryNames
		do: [:vName | hash := vName hash bitOr: hash].
	^hash
%

category: 'private'
method: CypressClassDefinition
instanceVariablesString
    ^ self stringForVariables: self instVarNames
%

category: 'accessing'
method: CypressClassDefinition
instVarNames
  ^ instVarNames
%

category: 'loading'
method: CypressClassDefinition
loadClassDefinition
  "Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

  ^ self loadClassDefinition: self defaultSymbolDictionaryName
%

category: 'loading'
method: CypressClassDefinition
loadClassDefinition: aDefaultSymbolDictionaryName
	"Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

	| newClass oldClass |
        self defaultSymbolDictionaryName: aDefaultSymbolDictionaryName.
	oldClass := self actualClassOrNil.
	newClass := self createOrReviseClass.
	(oldClass isNil or: [newClass == oldClass]) ifTrue: [^self].
	self classNeedingMigration: newClass.
	self
		recompileWithSubclassesFrom: oldClass
		to: newClass
		symbolList: System myUserProfile symbolList.
%

category: 'accessing'
method: CypressClassDefinition
name

	^name
%

category: 'initialization'
method: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: aSubclassType

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := someInstanceVariableNames.
	classInstVarNames := someClassInstanceVariableNames.
	classVarNames := someClassVariableNames.
	poolDictionaryNames := somePoolDictionaryNames.
	comment := aComment.
	subclassType := aSubclassType asString
%

category: 'initialization'
method: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames 
	classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames 
	poolDictionaryNames: somePoolDictionaryNames gs_options: someGs_options gs_constraints: someGs_constraints
	comment: aComment subclassType: aSubclassType

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := someInstanceVariableNames.
	classInstVarNames := someClassInstanceVariableNames.
	classVarNames := someClassVariableNames.
	poolDictionaryNames := somePoolDictionaryNames.
	gs_options := someGs_options.
	gs_constraints := someGs_constraints.
	comment := aComment.
	subclassType := aSubclassType asString
%

category: 'loading'
method: CypressClassDefinition
poolDictionariesForNames: pdNames
  | ar existingDict symList sharedPool |
  ar := Array new.
  symList := System myUserProfile symbolList.
  pdNames
    do: [ :poolName | 
      existingDict := symList objectNamed: poolName.
      existingDict
        ifNil: [ 
          | pool |
          pool := SymbolDictionary new.
          pool name: poolName asSymbol.
          ar add: pool ]
        ifNotNil: [ 
          (existingDict isKindOf: SymbolDictionary)
            ifTrue: [ ar add: existingDict ]
            ifFalse: [ 
              sharedPool ifNil: [ sharedPool := symList objectNamed: #'SharedPool' ].
              ((existingDict isKindOf: Class)
                and: [ existingDict isSubclassOf: sharedPool ])
                ifTrue: [ 
                  | cvars pName |
                  ar add: (cvars := existingDict _createClassVarsDict).
                  pName := poolName asSymbol.	"only change dictionary name if needed , to avoid SecurityError"
                  cvars name ~~ pName
                    ifTrue: [ cvars name: pName ] ] ] ] ].
  ^ ar
%

category: 'private'
method: CypressClassDefinition
poolDictionariesString
  ^ self stringForVariables: self poolDictionaryNames
%

category: 'loading'
method: CypressClassDefinition
poolDictionaryList

  ^ self poolDictionariesForNames: self poolDictionaryNames
%

category: 'accessing'
method: CypressClassDefinition
poolDictionaryNames

	^poolDictionaryNames
%

category: 'printString'
method: CypressClassDefinition
printDetailsOn: aStream

	aStream nextPutAll: self name
%

category: 'dependency'
method: CypressClassDefinition
provisions
	"Answer list of global names defined by this definition"

	^{ self name }
%

category: 'loading'
method: CypressClassDefinition
recompileWithSubclassesFrom: oldClass to: newClass symbolList: aSymbolList

	| olds news removedClassVariables removedSharedPools organizer subclasses newSubclass |
	olds := oldClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	news := newClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	removedClassVariables := self elementsIn: olds butNotIn: news.
	removedSharedPools := self elementsIn: oldClass sharedPools
				butNotIn: newClass sharedPools.
	self failedCompiledMethods: (newClass
				_copyMethodsAndVariablesFrom: oldClass
				except: 
					{$V.
					removedClassVariables.
					$P.
					removedSharedPools}
				dictionaries: aSymbolList).
	organizer := ClassOrganizer new.
	subclasses := organizer subclassesOf: oldClass.


	"Do this -after- #subclassesOf:, which has the side effect of replacing the new
	  class with the old class in the organizer"
	organizer addClass: newClass.

	"Iterate over all the first-level subclasses of the old class to create new subclasses"
	subclasses do: 
			[:oldSubclass |
			newSubclass := 
					[oldSubclass definition evaluateInContext: nil symbolList: aSymbolList]
							on: Error
							do: [:ex | ex return: nil].
			(newSubclass notNil and: [newSubclass ~~ oldSubclass])
				ifTrue: 
					[self
						classNeedingMigration: newSubclass;
						recompileWithSubclassesFrom: oldSubclass
							to: newSubclass
							symbolList: aSymbolList]]
%

category: 'dependency'
method: CypressClassDefinition
requirements
	"Answer list of global names required by this definition"

  self superclassName = 'nil'
    ifTrue: [ ^ #() ].
  ^{self superclassName}
%

category: 'accessing'
method: CypressClassDefinition
subclassType

	^subclassType
%

category: 'accessing'
method: CypressClassDefinition
superclassName

	^superclassName
%

category: 'loading'
method: CypressClassDefinition
unloadDefinition
	"GemStone could hold multiple definitions of the same class name.
	 Ignore aliased references.
	 Unload only the first one resolved.
	 It is an error if there is not at least one SymbolDictionary holding a
	 class with that name."

	| dictionarySymbolPair |
	dictionarySymbolPair := ((System myUserProfile symbolList
				dictionariesAndSymbolsOf: self actualClass)
					select: [:each | each last = self name asSymbol]) first.
	dictionarySymbolPair first removeKey: dictionarySymbolPair last
%

! Class implementation for 'CypressMethodDefinition'

!		Class methods for 'CypressMethodDefinition'

category: 'instance creation'
classmethod: CypressMethodDefinition
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	^self new
		className: aName asString
		classIsMeta: isMetaclass
		selector: aSelector asString
		category: aCategory asString
		source: (self normalizeLineEndingsOf: aSource)
%

category: 'instance creation'
classmethod: CypressMethodDefinition
forMethod: aGsNMethod

	| behavior selector |
	behavior := aGsNMethod inClass.
	selector := aGsNMethod selector.
	^self new
		className: behavior theNonMetaClass name asString
		classIsMeta: behavior isMeta
		selector: selector asString
		category: (behavior categoryOfSelector: selector) asString
		source: (self normalizeLineEndingsOf: aGsNMethod sourceString asString)
%

!		Instance methods for 'CypressMethodDefinition'

category: 'comparing'
method: CypressMethodDefinition
= aDefinition
    ^ super = aDefinition
        and: [ aDefinition source = self source
                and: [ aDefinition category = self category ] ]
%

category: 'loading'
method: CypressMethodDefinition
actualClass

  ^ self theNonMetaClass
    ifNotNil: [:cls |
      self classIsMeta
        ifTrue: [ cls class ]
        ifFalse: [ cls  ] ].
%

category: 'converting'
method: CypressMethodDefinition
asCypressMethodDefinition

	^self
%

category: 'accessing'
method: CypressMethodDefinition
category

	^category
%

category: 'visiting'
method: CypressMethodDefinition
classDefinition: classBlock methodDefinition: methodBlock

	methodBlock value: self
%

category: 'accessing'
method: CypressMethodDefinition
classIsMeta

	^classIsMeta
%

category: 'accessing'
method: CypressMethodDefinition
className

	^className
%

category: 'initialization'
method: CypressMethodDefinition
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := self normalizeLineEndingsOf: aSource
%

category: 'accessing'
method: CypressMethodDefinition
description
	^ Array	
		with: className
		with: selector
		with: classIsMeta
%

category: 'comparing'
method: CypressMethodDefinition
hash

	| hash |
	hash := classIsMeta asString hash.
	hash := source hash bitOr: hash.
	hash := category hash bitOr: hash.
	hash := className hash bitOr: hash.
	^hash
%

category: 'visiting'
method: CypressMethodDefinition
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
%

category: 'testing'
method: CypressMethodDefinition
isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
%

category: 'loading'
method: CypressMethodDefinition
loadMethodDefinition

	self actualClass
		compileMethod: self source
		dictionaries: System myUserProfile symbolList
		category: self category
		environmentId: 0
%

category: 'loading'
method: CypressMethodDefinition
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
%

category: 'printing'
method: CypressMethodDefinition
printDetailsOn: aStream

	aStream
		nextPutAll: self className;
		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);
		nextPutAll: '>>';
		nextPutAll: self selector.
%

category: 'dependency'
method: CypressMethodDefinition
requirements
	"Answer list of global names required by this definition"

	^{self className}
%

category: 'accessing'
method: CypressMethodDefinition
selector

	^selector
%

category: 'accessing'
method: CypressMethodDefinition
source

	^source
%

category: 'loading'
method: CypressMethodDefinition
theNonMetaClass

	^self resolveGlobalNamed: self className or: []
%

category: 'loading'
method: CypressMethodDefinition
unloadDefinition

  self actualClass ifNotNil: [:cl | cl removeSelector: self selector asSymbol ].
%

! Class implementation for 'CypressDefinitionIndex'

!		Class methods for 'CypressDefinitionIndex'

category: 'instance creation'
classmethod: CypressDefinitionIndex
definitions: aCollection
	^ self new addAll: aCollection
%

!		Instance methods for 'CypressDefinitionIndex'

category: 'adding'
method: CypressDefinitionIndex
add: aDefinition
	^ self definitionMap at: aDefinition description put: aDefinition
%

category: 'adding'
method: CypressDefinitionIndex
addAll: aCollection
	aCollection do: [:ea | self add: ea]
%

category: 'querying'
method: CypressDefinitionIndex
definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock
	| definition |
	definition := self definitionMap at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]
%

category: 'accessing'
method: CypressDefinitionIndex
definitionMap
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap
%

category: 'accessing'
method: CypressDefinitionIndex
definitions
	^self definitionMap values
%

category: 'removing'
method: CypressDefinitionIndex
remove: aDefinition
	self definitionMap removeKey: aDefinition description ifAbsent: []
%

! Class implementation for 'CypressDependencySorter'

!		Instance methods for 'CypressDependencySorter'

category: 'building'
method: CypressDependencySorter
add: aPatchOperation
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation
%

category: 'building'
method: CypressDependencySorter
addAll: aCollection
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]
%

category: 'private'
method: CypressDependencySorter
addExternalProvisions: aCollection

	(self elementsInBoth: aCollection and: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
%

category: 'private'
method: CypressDependencySorter
addProvision: aGlobalName
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.
%

category: 'private'
method: CypressDependencySorter
addRequirement: globalName for: aPatchOperation
	(self itemsRequiring: globalName) add: aPatchOperation
%

category: 'private'
method: CypressDependencySorter
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
%

category: 'private'
method: CypressDependencySorter
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
%

category: 'accessing'
method: CypressDependencySorter
externalRequirements

	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (self collection: unloaded
				gather: [:e | e provisions]) asSet.
	^self required keys
		reject: [:globalName | providedByUnloaded includes: globalName]
%

category: 'private'
method: CypressDependencySorter
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
%

category: 'accessing'
method: CypressDependencySorter
itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
%

category: 'accessing'
method: CypressDependencySorter
orderedItems
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems
%

category: 'accessing'
method: CypressDependencySorter
provided
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided
%

category: 'accessing'
method: CypressDependencySorter
required
	"dictionary of required global name mapped to list of definitions that require the global"

	required ifNil: [ required := Dictionary new ].
	^required
%

category: 'private'
method: CypressDependencySorter
unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^self elementsIn: aPatchOperation requirements butNotIn: self provided
%

! Class implementation for 'CypressEnvironmentDependencySorter'

!		Instance methods for 'CypressEnvironmentDependencySorter'

category: 'building'
method: CypressEnvironmentDependencySorter
add: aPatchOperation
  | requirements |
  requirements := self unresolvedRequirementsFor: aPatchOperation.
  requirements removeIfPresent: 'nil'.
  requirements removeIfPresent: nil.
  requirements isEmpty
    ifTrue: [ self addToOrder: aPatchOperation ]
    ifFalse: [ self addRequirements: requirements for: aPatchOperation ].
  ^ aPatchOperation
%

! Class implementation for 'CypressLoader'

!		Class methods for 'CypressLoader'

category: 'accessing'
classmethod: CypressLoader
defaultSymbolDictionaryName
  "Name of the SymbolDictionary where new classes should be installed"

  ^ (SessionTemps current 
      at: #'Cypress_Loader_Default_Symbol_Dictionary_Name' 
      ifAbsent: [] ) 
        ifNil: [
          System myUserProfile userId = 'SystemUser'
          ifTrue: [ #Globals ]
          ifFalse: [ #'UserGlobals' ] ]
%

category: 'accessing'
classmethod: CypressLoader
defaultSymbolDictionaryName: aSymbol
  SessionTemps current 
      at: #'Cypress_Loader_Default_Symbol_Dictionary_Name'
      put: aSymbol
%

category: 'unloading'
classmethod: CypressLoader
unloadSnapshot: aSnapshot
  ^ self new
    unloadSnapshot: aSnapshot;
    load
%

category: 'loading'
classmethod: CypressLoader
updatePackage: aPackage defaultSymbolDictionaryName: defaultSymbolDictionaryName withSnapshot: aSnapshot
  "Answer the loader used to apply the update."

  ^ self new
    defaultSymbolDictionaryName: defaultSymbolDictionaryName;
    updatePackage: aPackage withSnapshot: aSnapshot;
    load
%

category: 'loading'
classmethod: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot
  "Answer the loader used to apply the update."

  ^ self
    updatePackage: aPackage
    defaultSymbolDictionaryName: nil
    withSnapshot: aSnapshot
%

!		Instance methods for 'CypressLoader'

category: 'updating'
method: CypressLoader
addFailedPatchOperation: aPatchOperation

	self errors add: aPatchOperation
%

category: 'accessing'
method: CypressLoader
additions

	additions ifNil: [ additions := OrderedCollection new ].
	^additions
%

category: 'loading'
method: CypressLoader
analyze
  self
    analyzeRemovalOfAdditions;
    analyzeAdditions;
    analyzeRemovals
%

category: 'loading'
method: CypressLoader
analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter required.
%

category: 'loading'
method: CypressLoader
analyzeRemovalOfAdditions
  "if there is an addition and a removal for the same definition, the addition wins ... needed when loading multiple packages and a defintion has been moved from one package to another --- see atomic loads for Metacello"

  | index |
  index := CypressDefinitionIndex
    definitions: (self additions collect: [ :each | each definition ]).
  self removals
    removeAllSuchThat: [ :removal | 
      (index
        definitionLike: removal definition
        ifPresent: [ :additionDefinition | self obsoletions at: additionDefinition description put: removal definition ]
        ifAbsent: [  ]) notNil ]
%

category: 'loading'
method: CypressLoader
analyzeRemovals

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reverse.
%

category: 'applying'
method: CypressLoader
applyAddition: aCypressPatchOperation

	self additions add: aCypressPatchOperation
%

category: 'applying'
method: CypressLoader
applyModification: aCypressPatchOperation
  self additions add: aCypressPatchOperation.
  self obsoletions
    at: aCypressPatchOperation modification description
    put: aCypressPatchOperation obsoletion
%

category: 'applying'
method: CypressLoader
applyRemoval: aCypressPatchOperation

	self removals add: aCypressPatchOperation
%

category: 'loading'
method: CypressLoader
attemptInitialLoad
  ^ self attemptInitialLoad: true
%

category: 'loading'
method: CypressLoader
attemptInitialLoad: doUnloads

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions .
  doUnloads ifTrue:[ self unloadRemovals: self removals].
%

category: 'accessing'
method: CypressLoader
defaultSymbolDictionaryName
  ^ defaultSymbolDictionaryName ifNil: [ self class defaultSymbolDictionaryName ]
%

category: 'accessing'
method: CypressLoader
defaultSymbolDictionaryName: aSymbol
  defaultSymbolDictionaryName := aSymbol
%

category: 'loading'
method: CypressLoader
errorOnFailedPatchOperations

	exceptionClass := CypressLoaderError.
%

category: 'accessing'
method: CypressLoader
errors
	errors ifNil: [self resetErrors].
	^errors
%

category: 'loading'
method: CypressLoader
handleCompileError: aCompileError from: aPatchOperation

	| undefinedSymbolErrors otherErrors |
	undefinedSymbolErrors := aCompileError errorDetails
				select: [:each | each first = 1031].
	otherErrors := aCompileError errorDetails
				reject: [:each | each first = 1031].
	undefinedSymbolErrors do: [:each | self requirements add: each last].
	aCompileError pass
%

category: 'loading'
method: CypressLoader
handlePatchOperation: aPatchOperation failure: anException
	"Signal the loader exception appropriate to the current phase.
	 Note that a handler may suppress the #addFailedPatchOperation: by
	 sending #return or #return: to the resignaled exception. Otherwise,
	 resumption from a resumable resignalled exception will continue through
	 this method."

	(exceptionClass patchOperation: aPatchOperation exception: anException) signal.
	self addFailedPatchOperation: aPatchOperation.
%

category: 'loading'
method: CypressLoader
load
  ^self load: true
%

category: 'loading'
method: CypressLoader
load: doUnloads
	self analyze .
  doUnloads ifTrue:[ self reportUnloadableDefinitions ].
	self attemptInitialLoad: doUnloads  ;
		retryFailedLoads;
		postLoad.
%

category: 'loading'
method: CypressLoader
loadAdditions: somePatchOperations
	"Load class definitions first, then method definitions."

	somePatchOperations
		do: [:each | self loadClassDefinition: each];
		do: [:each | self loadMethodDefinition: each].
%

category: 'operations'
method: CypressLoader
loadClassDefinition: aPatchOperation

	[ aPatchOperation loadClassDefinition: self defaultSymbolDictionaryName ]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%

category: 'operations'
method: CypressLoader
loadMethodDefinition: aPatchOperation

	
	[[aPatchOperation loadMethodDefinition]
		on: CompileError
		do: [:ex | self handleCompileError: ex from: aPatchOperation]]
			on: Error
			do: [:ex | self handlePatchOperation: aPatchOperation failure: ex]
%

category: 'accessing'
method: CypressLoader
methodAdditions

	^#()
%

category: 'loading'
method: CypressLoader
notifyOnFailedPatchOperations

	exceptionClass := CypressLoaderErrorNotification.
%

category: 'accessing'
method: CypressLoader
obsoletions
  obsoletions ifNil: [ obsoletions := Dictionary new ].
  ^ obsoletions
%

category: 'loading'
method: CypressLoader
postLoad
	"This is where the obsoletion is taken into account ..."

	self additions do: [:each | self postLoad: each].
%

category: 'operations'
method: CypressLoader
postLoad: aPatchOperation

	[aPatchOperation postLoadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%

category: 'accessing'
method: CypressLoader
provisions
	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]
%

category: 'accessing'
method: CypressLoader
removals

	removals ifNil: [ removals := OrderedCollection new ].
	^removals
%

category: 'loading'
method: CypressLoader
reportUnloadableDefinitions

	self unloadable isEmpty ifTrue: [^self].
	(CypressLoaderMissingClasses missingRequirementsMap: unloadable) signal.
%

category: 'accessing'
method: CypressLoader
requirements

	^requirements
%

category: 'loading'
method: CypressLoader
resetErrors

	errors := OrderedCollection new.
%

category: 'loading'
method: CypressLoader
retryFailedLoads
	"In case any of the failed loads were resolved by subsequent
	 patch operations after the initial attempt or by editting of the
	 failed patch operations by exception handling during the notification
	 phase (initial attempt)."

	| failed |
	failed := self errors.
	self
		resetErrors;
		errorOnFailedPatchOperations;
		loadAdditions: (self elementsInBoth: self additions and: failed);
		unloadRemovals: (self elementsInBoth: self removals and: failed)
%

category: 'accessing'
method: CypressLoader
unloadable

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable
%

category: 'operations'
method: CypressLoader
unloadDefinition: aPatchOperation

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
%

category: 'loading'
method: CypressLoader
unloadRemovals: somePatchOperations
	"Removals need to be done after adding classes and methods."

	somePatchOperations
		do: [:each | self unloadDefinition: each].
%

category: 'unloading'
method: CypressLoader
unloadSnapshot: aSnapshot
  |  patch |
  patch := CypressSnapshot empty patchRelativeToBase: aSnapshot.
  patch applyTo: self
%

category: 'loading'
method: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot

	| patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]
%

! Class implementation for 'CypressEnvironmentLoader'

!		Instance methods for 'CypressEnvironmentLoader'

category: 'accessing'
method: CypressEnvironmentLoader
allClasses
  | classes |
  classes := Array new.
  self defaultSymbolList
    do: [ :dict | classes addAll: (dict select: [ :each | each isBehavior ]) ].
  ^ classes
%

category: 'loading'
method: CypressEnvironmentLoader
analyzeAdditions
  | sorter |
  sorter := CypressEnvironmentDependencySorter new
    addAll: self additions;
    addExternalProvisions: self provisions;
    yourself.
  additions := sorter orderedItems.
  requirements := sorter externalRequirements.
  unloadable := sorter required
%

category: 'loading'
method: CypressEnvironmentLoader
analyzeRemovals
  | sorter |
  sorter := CypressEnvironmentDependencySorter new
    addAll: self removals;
    yourself.
  removals := sorter orderedItems reverse
%

category: 'accessing'
method: CypressEnvironmentLoader
compilationSymbolList
  ^ compilationSymbolList
    ifNil: [ compilationSymbolList := self defaultSymbolList ]
%

category: 'accessing'
method: CypressEnvironmentLoader
compilationSymbolList: anObject

   compilationSymbolList := anObject
%

category: 'accessing'
method: CypressEnvironmentLoader
defaultEnvironmentId
  ^ defaultEnvironmentId ifNil: [ defaultEnvironmentId := 0 ]
%

category: 'accessing'
method: CypressEnvironmentLoader
defaultEnvironmentId: anObject

   defaultEnvironmentId := anObject
%

category: 'accessing'
method: CypressEnvironmentLoader
defaultSymbolList
  ^ System myUserProfile symbolList
%

category: 'operations'
method: CypressEnvironmentLoader
loadClassDefinition: aPatchOperation
  [ 
  aPatchOperation
    loadClassDefinition: self defaultSymbolDictionaryName
    environmentLoader: self ]
    on: Error
    do: [ :ex | self handlePatchOperation: aPatchOperation failure: ex ]
%

category: 'operations'
method: CypressEnvironmentLoader
loadMethodDefinition: aPatchOperation
  [ 
  [ 
  aPatchOperation
    loadMethodDefinition: self lookupSymbolList
    environmentLoader: self ]
    on: CompileError
    do: [ :ex | self handleCompileError: ex from: aPatchOperation ] ]
    on: Error
    do: [ :ex | self handlePatchOperation: aPatchOperation failure: ex ]
%

category: 'accessing'
method: CypressEnvironmentLoader
lookupSymbolList
  ^ lookupSymbolList ifNil: [ lookupSymbolList := self defaultSymbolList ]
%

category: 'accessing'
method: CypressEnvironmentLoader
lookupSymbolList: anObject

   lookupSymbolList := anObject
%

category: 'operations'
method: CypressEnvironmentLoader
postLoad: aPatchOperation
  [ 
  aPatchOperation
    postLoadDefinition: lookupSymbolList
    environmentId: self defaultEnvironmentId ]
    on: Error
    do: [ :ex | self handlePatchOperation: aPatchOperation failure: ex ]
%

! Class implementation for 'CypressPackageDefinition'

!		Class methods for 'CypressPackageDefinition'

category: 'instance creation'
classmethod: CypressPackageDefinition
named: aString

	^self new
		name: aString;
		yourself.
%

!		Instance methods for 'CypressPackageDefinition'

category: 'comparing'
method: CypressPackageDefinition
= other
	^ other species = self species and: [other name sameAs: name]
%

category: 'snapshotting'
method: CypressPackageDefinition
addClass: aClass toDefinitions: definitions

	definitions add: (CypressClassDefinition forClass: aClass)
%

category: 'snapshotting'
method: CypressPackageDefinition
addExtensionMethodsFromClass: aClass toMap: classMap

	| defs map |
	defs := classMap at: aClass theNonMetaClass
				ifAbsent: [OrderedCollection new].
	map := Dictionary new.
	aClass categorysDo: 
			[:category :selectors |
			(category asLowercase
				indexOfSubCollection: '*' , self basePackageName asLowercase startingAt: 1 ifAbsent: [ 0 ]) = 1
				ifTrue: [map at: category put: selectors asSortedCollection]].
	map keys asSortedCollection do: 
			[:category |
			(map at: category) do: 
					[:selector |
					defs add: (CypressMethodDefinition
								forMethod: (aClass compiledMethodAt: selector))]].
	defs notEmpty ifTrue: [classMap at: aClass theNonMetaClass put: defs]
%

category: 'snapshotting'
method: CypressPackageDefinition
addMethodsFromClass: aClass toDefinitions: definitions
	"Add only those methods which are not extensions from other packages."

	(((aClass methodDictForEnv: 0)
		reject: [:each | (each inClass categoryOfSelector: each selector) first = $*])
			asSortedCollection: [:a :b | a selector <= b selector])
			do: [:method | definitions add: (CypressMethodDefinition forMethod: method)]
%

category: 'accessing'
method: CypressPackageDefinition
basePackageName
  "package name may have a platform/branch extension, when comparing against category/protocol names, extension is ignored"

  | nm index |
  nm := self name.
  index := nm indexOfSubCollection: '.' startingAt: 1.
  index = 0
    ifTrue: [ ^ nm ].
  ^ nm copyFrom: 1 to: index - 1
%

category: 'accessing'
method: CypressPackageDefinition
classes
  ^ self classesInPackageNamed: self basePackageName
%

category: 'comparing'
method: CypressPackageDefinition
hash
  ^ name hash
%

category: 'accessing'
method: CypressPackageDefinition
name
	^ name
%

category: 'accessing'
method: CypressPackageDefinition
name: aString
	name := aString
%

category: 'printing'
method: CypressPackageDefinition
printDetailsOn: aStream

	aStream nextPutAll: name
%

category: 'snapshotting'
method: CypressPackageDefinition
snapshot

	| classDefinitions methodDefinitions classMap |
	classDefinitions := OrderedCollection new.
	methodDefinitions := OrderedCollection new.
	(self determineClassHierarchicalOrder: self classes) do: 
			[:cls |
			self
				addClass: cls toDefinitions: classDefinitions;
				addMethodsFromClass: cls toDefinitions: methodDefinitions;
				addMethodsFromClass: cls class toDefinitions: methodDefinitions].
	classMap := Dictionary new.
	self allClasses do: 
			[:each |
			self
				addExtensionMethodsFromClass: each toMap: classMap;
				addExtensionMethodsFromClass: each class toMap: classMap].
	(self determineClassHierarchicalOrder: classMap keys)
		do: [:aClass | methodDefinitions addAll: (classMap at: aClass)].
	^CypressSnapshot definitions: classDefinitions, methodDefinitions
%

! Class implementation for 'CypressEnvironmentPackageDefinition'

!		Instance methods for 'CypressEnvironmentPackageDefinition'

category: 'accessing'
method: CypressEnvironmentPackageDefinition
lookupSymbolList

   ^lookupSymbolList
%

category: 'accessing'
method: CypressEnvironmentPackageDefinition
lookupSymbolList: anObject

   lookupSymbolList := anObject
%

category: 'accessing'
method: CypressEnvironmentPackageDefinition
symbolList
  lookupSymbolList ifNil: [ ^ super symbolList ].
  ^ self lookupSymbolList
%

! Class implementation for 'CypressPackageInformation'

!		Class methods for 'CypressPackageInformation'

category: 'instance creation'
classmethod: CypressPackageInformation
named: aString repository: aCypressRepository
	"Answer an instance of the receiver representing the named package.
	 If the package was saved in a Repository, load up the saved details."

	^self new
		initializeFromName: aString andRepository: aCypressRepository;
		yourself
%

category: 'instance creation'
classmethod: CypressPackageInformation
new

	^super new
		initialize;
		yourself
%

!		Instance methods for 'CypressPackageInformation'

category: 'updating - type'
method: CypressPackageInformation
beConflictedWith: somePackageNames
	"Be designated as representing the prefix of one or more Known Package names."

	type := 'Conflicted Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Conflicts with the packages named ', self competingPackageNamesString
%

category: 'updating - type'
method: CypressPackageInformation
beKnown
	"Be known to represent a real package."

	type := 'Known Package'.
	advice := ''.
	competingPackageNames := #()
%

category: 'updating - type'
method: CypressPackageInformation
beQualifiedNameOf: somePackageNames
	"Be designated as qualifying a Known Package name and therefore not eligible as a package name."

	type := 'Qualified Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Qualifies the package named ', self competingPackageNamesString
%

category: 'updating - type'
method: CypressPackageInformation
beUnknown
	"Be designated as possibly representing a package, but not known to do so."

	type := 'Unknown'.
	advice := ''.
	competingPackageNames := #()
%

category: 'accessing'
method: CypressPackageInformation
changesCount

	^changesCount
%

category: 'accessing'
method: CypressPackageInformation
changesCount: anInteger

	changesCount := anInteger
%

category: 'accessing'
method: CypressPackageInformation
changesStatus

	^self hasChanges
		ifTrue: [' (' , self changesCount printString , ')']
		ifFalse: ['']
%

category: 'accessing'
method: CypressPackageInformation
classCount

	^self imageCounts first
%

category: 'accessing'
method: CypressPackageInformation
competingPackageNames

	^competingPackageNames
%

category: 'accessing'
method: CypressPackageInformation
competingPackageNamesString

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self competingPackageNames
		do: [:each | stream nextPutAll: each printString]
		separatedBy: [stream nextPutAll: ', '].
	^stream contents
%

category: 'accessing'
method: CypressPackageInformation
description

	self isKnown ifTrue: [^self savedLocation].
	self isUnknown ifTrue: [^' <unknown>'].
	^' <', advice, '>'
%

category: 'accessing'
method: CypressPackageInformation
determinedChangesCount

	| notInImage notInSaved |
	notInImage := self savedDefinitions
				reject: [:each | self imageDefinitions includes: each].
	notInSaved := self imageDefinitions
		reject: [:each | self savedDefinitions includes: each].
	^notInImage size + notInSaved size
%

category: 'testing'
method: CypressPackageInformation
hasChanges

	^self changesCount > 0
%

category: 'accessing'
method: CypressPackageInformation
imageCounts

	^imageCounts
%

category: 'accessing'
method: CypressPackageInformation
imageCounts: someIntegers
	"A pair: the number of classes and number of methods"

	imageCounts := someIntegers
%

category: 'accessing'
method: CypressPackageInformation
imageDefinitionCounts

	| classCount methodCount |
	classCount := methodCount := 0.
	self imageDefinitions do: 
			[:each |
			each classDefinition: [:classDefinition | classCount := classCount + 1]
				methodDefinition: [:methodDefinition | methodCount := methodCount + 1]].
	^Array with: classCount with: methodCount
%

category: 'accessing'
method: CypressPackageInformation
imageDefinitions

	^imageDefinitions
%

category: 'accessing'
method: CypressPackageInformation
imageDefinitions: someCypressDefinitions

	imageDefinitions := someCypressDefinitions
%

category: 'accessing'
method: CypressPackageInformation
imageDefinitionsStatus

	^self classCount printString , '/' , self methodCount printString
%

category: 'initializing'
method: CypressPackageInformation
initialize

	self
		beUnknown;
		name: '';
		imageDefinitions: #();
		savedDefinitions: #();
		savedLocation: '';
		repositoryDescription: '';
		imageCounts: #(0 0);
		changesCount: 0
%

category: 'initializing'
method: CypressPackageInformation
initializeFromName: aString andRepository: aCypressRepositoryOrNil

	self name: aString.
	aCypressRepositoryOrNil isNil ifTrue: [^self].
	self updateKnownPackageRepository: aCypressRepositoryOrNil
%

category: 'testing - type'
method: CypressPackageInformation
isConflicted

	^type = 'Conflicted Name'
%

category: 'testing - type'
method: CypressPackageInformation
isKnown

	^type = 'Known Package'
%

category: 'testing - type'
method: CypressPackageInformation
isQualifiedName

	^type = 'Qualified Name'
%

category: 'testing - type'
method: CypressPackageInformation
isUnknown

	^type = 'Unknown'
%

category: 'accessing'
method: CypressPackageInformation
methodCount

	^self imageCounts last
%

category: 'accessing'
method: CypressPackageInformation
name

	^name
%

category: 'accessing'
method: CypressPackageInformation
name: aString

	name := aString
%

category: 'printing'
method: CypressPackageInformation
printDetailsOn: aStream

	aStream
		nextPutAll: self name;
		nextPutAll: ' - ';
		nextPutAll: self description
%

category: 'updating'
method: CypressPackageInformation
readDefinitionsFromRepository

	^(self repository reader readPackageStructureForPackageNamed: self name)
		packageStructure snapshot
		definitions
%

category: 'updating'
method: CypressPackageInformation
refresh

	self isKnown ifFalse: [^self].
	self
		updateImageDefinitions;
		updateSavedDefinitions;
		updateChangesCount.
%

category: 'accessing'
method: CypressPackageInformation
repository

	^repository
%

category: 'accessing'
method: CypressPackageInformation
repository: aCypressFileSystemRepository

	repository := aCypressFileSystemRepository
%

category: 'unknown'
method: CypressPackageInformation
repositoryDescription

	^repositoryDescription
%

category: 'unknown'
method: CypressPackageInformation
repositoryDescription: aString

	repositoryDescription := aString
%

category: 'accessing'
method: CypressPackageInformation
savedDefinitions

	^savedDefinitions
%

category: 'accessing'
method: CypressPackageInformation
savedDefinitions: someCypressDefinitions

	savedDefinitions := someCypressDefinitions
%

category: 'accessing'
method: CypressPackageInformation
savedLocation

	^savedLocation
%

category: 'accessing'
method: CypressPackageInformation
savedLocation: aDirectory

	savedLocation := aDirectory
%

category: 'accessing'
method: CypressPackageInformation
status

	| changes |
	(changes := self changesStatus) isEmpty ifTrue: [^self imageDefinitionsStatus].
	^self imageDefinitionsStatus, changes
%

category: 'updating'
method: CypressPackageInformation
updateChangesCount
	"Must be applied after the image definitions and saved definitions are updated."

	self changesCount: self determinedChangesCount
%

category: 'updating'
method: CypressPackageInformation
updateImageDefinitions

	self
		imageDefinitions: (CypressPackageDefinition named: self name) snapshot
					definitions;
		imageCounts: self imageDefinitionCounts
%

category: 'updating'
method: CypressPackageInformation
updateKnownPackageRepository: aCypressRepository
	"Update the receiver to reflect it being a known package."

	self
		beKnown;
		updateRepository: aCypressRepository;
		refresh.
%

category: 'updating'
method: CypressPackageInformation
updateRepository: aCypressRepository

	self
		repository: aCypressRepository;
		repositoryDescription: self repository description;
		savedLocation: self repository directoryPath
%

category: 'updating'
method: CypressPackageInformation
updateSavedDefinitions

	self savedDefinitions: self readDefinitionsFromRepository
%

! Class implementation for 'CypressPatch'

!		Class methods for 'CypressPatch'

category: 'instance creation'
classmethod: CypressPatch
fromBase: baseSnapshot toTarget: targetSnapshot
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot
%

!		Instance methods for 'CypressPatch'

category: 'applying'
method: CypressPatch
applyTo: aCypressLoader
	operations do: [:ea | ea applyTo: aCypressLoader].
%

category: 'initialization'
method: CypressPatch
fromBase: baseSnapshot toTarget: targetSnapshot
	| base target |	
	operations := OrderedCollection new.
	base := CypressDefinitionIndex definitions: baseSnapshot definitions.
	target := CypressDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions do:
		[:t |
		base
			definitionLike: t
			ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (CypressModification of: b to: t)]]
			ifAbsent: [operations add: (CypressAddition of: t)]].
		
	base definitions do:
		[:b |
		target
			definitionLike: b
			ifPresent: [:t | ]
			ifAbsent: [operations add: (CypressRemoval of: b)]]
%

category: 'accessing'
method: CypressPatch
operations

	^operations
%

! Class implementation for 'CypressPatchOperation'

!		Instance methods for 'CypressPatchOperation'

category: 'comparing'
method: CypressPatchOperation
= aPatchOperation
	^aPatchOperation isKindOf: self class
%

category: 'applying'
method: CypressPatchOperation
applyTo: aCypressLoader

	self subclassResponsibility: #applyTo:
%

category: 'accessing'
method: CypressPatchOperation
definition
  "answer the primary definition associated with the operation"

  self subclassResponsibility: #'definition'
%

category: 'accessing'
method: CypressPatchOperation
description

	self subclassResponsibility: #description
%

category: 'comparing'
method: CypressPatchOperation
hash
    ^ self description hash
%

category: 'loading'
method: CypressPatchOperation
loadClassDefinition
  self loadClassDefinition: self defaultSymbolDictionaryName
%

category: 'loading'
method: CypressPatchOperation
loadClassDefinition: aDefaultSymbolDictionaryName
  self subclassResponsibility: #'loadClassDefinition'
%

category: 'loading'
method: CypressPatchOperation
loadMethodDefinition

	self subclassResponsibility: #loadMethodDefinition
%

category: 'loading'
method: CypressPatchOperation
postLoadDefinition

	self subclassResponsibility: #postLoadDefinition
%

category: 'printing'
method: CypressPatchOperation
printDetailsOn: aStream

	aStream nextPutAll: self description.
%

category: 'dependency'
method: CypressPatchOperation
provisions
	"Answer list of global names defined by this definition"

	self subclassResponsibility: #provisions
%

category: 'dependency'
method: CypressPatchOperation
requirements
	"Answer list of global names required by this definition"

	self subclassResponsibility: #requirements
%

category: 'loading'
method: CypressPatchOperation
unloadDefinition

	CypressError signal: 'inappropriate to send #unloadDefinition to an addition or modification operation'
%

! Class implementation for 'CypressAddition'

!		Class methods for 'CypressAddition'

category: 'instance creation'
classmethod: CypressAddition
of: aDefinition
	^ self new definition: aDefinition
%

!		Instance methods for 'CypressAddition'

category: 'comparing'
method: CypressAddition
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%

category: 'applying'
method: CypressAddition
applyTo: aCypressLoader

	aCypressLoader applyAddition: self
%

category: 'accessing'
method: CypressAddition
definition

	^definition
%

category: 'initialization'
method: CypressAddition
definition: aDefinition

	definition := aDefinition
%

category: 'accessing'
method: CypressAddition
description
    ^ 'add: ' , self definition printString
%

category: 'comparing'
method: CypressAddition
hash
  ^ super hash bitXor: definition hash
%

category: 'loading'
method: CypressAddition
loadClassDefinition: aDefaultSymbolDictionaryName
  self definition loadClassDefinition: aDefaultSymbolDictionaryName
%

category: 'loading'
method: CypressAddition
loadMethodDefinition
  self definition loadMethodDefinition
%

category: 'loading'
method: CypressAddition
postLoadDefinition
	self definition postLoadOver: nil
%

category: 'dependency'
method: CypressAddition
provisions
	"Answer list of global names defined by this definition"

	^self definition provisions
%

category: 'dependency'
method: CypressAddition
requirements
	"Answer list of global names required by this definition"

	^self definition requirements
%

! Class implementation for 'CypressModification'

!		Class methods for 'CypressModification'

category: 'instance creation'
classmethod: CypressModification
of: base to: target
	^ self new base: base target: target
%

!		Instance methods for 'CypressModification'

category: 'initialization'
method: CypressModification
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
%

category: 'applying'
method: CypressModification
applyTo: aCypressLoader

	aCypressLoader applyModification: self
%

category: 'initialization'
method: CypressModification
base: base target: target

	obsoletion := base.
	modification := target.
%

category: 'accessing'
method: CypressModification
definition
  "answer the primary definition associated with the operation"

  ^ self modification
%

category: 'accessing'
method: CypressModification
description
    ^ 'modify from: ' , self obsoletion printString , ' to: ' , self modification printString
%

category: 'comparing'
method: CypressModification
hash
  ^ (super hash bitXor: modification hash) bitXor: obsoletion hash
%

category: 'loading'
method: CypressModification
loadClassDefinition: aDefaultSymbolDictionaryName
  self modification loadClassDefinition: aDefaultSymbolDictionaryName
%

category: 'loading'
method: CypressModification
loadMethodDefinition

	self modification loadMethodDefinition.
%

category: 'accessing'
method: CypressModification
modification

	^modification
%

category: 'accessing'
method: CypressModification
obsoletion

	^obsoletion
%

category: 'loading'
method: CypressModification
postLoadDefinition
	self modification postLoadOver: self obsoletion
%

category: 'dependency'
method: CypressModification
provisions
	"Answer list of global names defined by this definition"

	^self modification provisions
%

category: 'dependency'
method: CypressModification
requirements
	"Answer list of global names required by this definition"

	^self modification requirements
%

! Class implementation for 'CypressRemoval'

!		Class methods for 'CypressRemoval'

category: 'instance creation'
classmethod: CypressRemoval
of: aDefinition
	^ self new definition: aDefinition
%

!		Instance methods for 'CypressRemoval'

category: 'comparing'
method: CypressRemoval
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%

category: 'applying'
method: CypressRemoval
applyTo: aCypressLoader

	aCypressLoader applyRemoval: self
%

category: 'accessing'
method: CypressRemoval
definition

	^definition
%

category: 'initialization'
method: CypressRemoval
definition: aDefinition

	definition := aDefinition
%

category: 'accessing'
method: CypressRemoval
description

	^'remove: ', self definition printString
%

category: 'comparing'
method: CypressRemoval
hash
  ^ super hash bitXor: definition hash
%

category: 'loading'
method: CypressRemoval
loadClassDefinition: aDefaultSymbolDictionaryName
  CypressError
    signal: 'inappropriate to send #loadClassDefinition to a removal operation'
%

category: 'loading'
method: CypressRemoval
loadMethodDefinition
	
	CypressError signal: 'inappropriate to send #loadMethodDefinition to a removal operation'
%

category: 'loading'
method: CypressRemoval
postLoadDefinition
	
	CypressError signal: 'inappropriate to send #postLoadDefinition to a removal operation'
%

category: 'dependency'
method: CypressRemoval
provisions
	"Answer list of global names defined by this definition"

	^#()
%

category: 'dependency'
method: CypressRemoval
requirements
	"Answer list of global names required by this definition"

	^#()
%

category: 'loading'
method: CypressRemoval
unloadDefinition

	self definition unloadDefinition.
%

! Class implementation for 'CypressSnapshot'

!		Class methods for 'CypressSnapshot'

category: 'instance creation'
classmethod: CypressSnapshot
definitions: aDefinitions

	^(self new) definitions: aDefinitions
%

category: 'instance creation'
classmethod: CypressSnapshot
empty

  ^self definitions: #()
%

!		Instance methods for 'CypressSnapshot'

category: 'comparing'
method: CypressSnapshot
= other
	^ definitions asArray = other definitions asArray
%

category: 'enumerating'
method: CypressSnapshot
classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
%

category: 'accessing'
method: CypressSnapshot
definitions

	^definitions
%

category: 'accessing'
method: CypressSnapshot
definitions: aDefinitions

	definitions := aDefinitions
%

category: 'comparing'
method: CypressSnapshot
hash
  ^ definitions asArray hash
%

category: 'patching'
method: CypressSnapshot
patchRelativeToBase: aSnapshot
	^ CypressPatch fromBase: aSnapshot toTarget: self
%

category: 'unloading'
method: CypressSnapshot
unload

  ^CypressLoader unloadSnapshot: self
%

category: 'loading'
method: CypressSnapshot
updatePackage: aPackage
  "Answer the loader used to apply the update."

  ^ self updatePackage: aPackage defaultSymbolDictionaryName: nil
%

category: 'loading'
method: CypressSnapshot
updatePackage: aPackage defaultSymbolDictionaryName: defaultSymbolDictionaryName
  "Answer the loader used to apply the update."

  ^ CypressLoader
    updatePackage: aPackage
    defaultSymbolDictionaryName: defaultSymbolDictionaryName
    withSnapshot: self
%

! Class implementation for 'CypressStructure'

!		Class methods for 'CypressStructure'

category: 'instance creation'
classmethod: CypressStructure
named: aString

	^(self new)
		name: aString;
		yourself
%

!		Instance methods for 'CypressStructure'

category: 'accessing'
method: CypressStructure
name

	^name
%

category: 'accessing'
method: CypressStructure
name: aString 

	name := aString
%

category: 'accessing'
method: CypressStructure
packageStructure
	^packageStructure
%

category: 'accessing'
method: CypressStructure
packageStructure: aCypressPackageStructure
	packageStructure := aCypressPackageStructure
%

category: 'printing'
method: CypressStructure
printDetailsOn: aStream

	aStream nextPutAll: self name.
%

category: 'accessing'
method: CypressStructure
properties

	properties ifNil: [ properties := Dictionary new ].
	^properties
%

category: 'accessing'
method: CypressStructure
properties: aDictionary

	properties := aDictionary
%

! Class implementation for 'CypressClassStructure'

!		Class methods for 'CypressClassStructure'

category: 'instance creation'
classmethod: CypressClassStructure
fromClassDefinition: classDefinition

	^self new
		fromClassDefinition: classDefinition;
		yourself
%

!		Instance methods for 'CypressClassStructure'

category: 'converting'
method: CypressClassStructure
asCypressClassDefinition

	self isClassExtension ifTrue: [^CypressError signal: 'Extensions cannot have class definitions'].
	^CypressClassDefinition
		name: self className
		superclassName: self superclassName
		category: self category
		instVarNames: self instanceVariableNames
		classInstVarNames: self classInstanceVariableNames
		classVarNames: self classVariableNames
		poolDictionaryNames: self poolDictionaryNames
		gs_options: self gs_options
		gs_constraints: self gs_constraints
		comment: self comment
		subclassType: self subclassType
%

category: 'accessing'
method: CypressClassStructure
category

	^self properties
		at: 'category'
		ifAbsent: [self packageStructure packageName]
%

category: 'accessing'
method: CypressClassStructure
category: aString

	^self properties at: 'category' put: aString
%

category: 'accessing'
method: CypressClassStructure
classInstanceVariableNames

	^self properties at: 'classinstvars' ifAbsent: [#()]
%

category: 'accessing'
method: CypressClassStructure
classInstanceVariableNames: someStrings

	^self properties at: 'classinstvars' put: someStrings
%

category: 'converting'
method: CypressClassStructure
classInstanceVariablesString
  ^ self stringForVariables: self classInstanceVariableNames
%

category: 'querying'
method: CypressClassStructure
classMethodNamed: methodName

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure named: methodName]
%

category: 'accessing'
method: CypressClassStructure
classMethods

	classMethods ifNil: [ classMethods := Dictionary new ].
	^classMethods
%

category: 'accessing'
method: CypressClassStructure
className

	^self name
%

category: 'accessing'
method: CypressClassStructure
classVariableNames

	^self properties at: 'classvars' ifAbsent: [#()]
%

category: 'accessing'
method: CypressClassStructure
classVariableNames: someStrings

	^self properties at: 'classvars' put: someStrings
%

category: 'converting'
method: CypressClassStructure
classVariablesString
  ^ self stringForVariables: self classVariableNames asSortedCollection
%

category: 'accessing'
method: CypressClassStructure
comment

	comment ifNil: [ comment := '' ].
	^comment
%

category: 'accessing'
method: CypressClassStructure
comment: aString

	comment := aString
%

category: 'initialization'
method: CypressClassStructure
fromClassDefinition: classDefinition

	self
		isClassExtension: false;
		name: classDefinition name;
		category: classDefinition category;
		comment: classDefinition comment;
		superclassName: classDefinition superclassName;
		instanceVariableNames: classDefinition instVarNames;
		classInstanceVariableNames: classDefinition classInstVarNames;
		classVariableNames: classDefinition classVarNames;
		poolDictionaryNames: classDefinition poolDictionaryNames;
		subclassType: classDefinition subclassType;
		gs_options: classDefinition gs_options;
		gs_constraints: classDefinition gs_constraints
%

category: 'accessing'
method: CypressClassStructure
gs_constraints

	^self properties at: 'gs_constraints' ifAbsent: [#()]
%

category: 'accessing'
method: CypressClassStructure
gs_constraints: anConstraintsArray

	^self properties at: 'gs_constraints' put: anConstraintsArray
%

category: 'accessing'
method: CypressClassStructure
gs_options

	^self properties at: 'gs_options' ifAbsent: [#()]
%

category: 'accessing'
method: CypressClassStructure
gs_options: anOptionsArray

	^self properties at: 'gs_options' put: anOptionsArray
%

category: 'querying'
method: CypressClassStructure
instanceMethodNamed: methodName

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure named: methodName]
%

category: 'accessing'
method: CypressClassStructure
instanceMethods

	instanceMethods ifNil: [ instanceMethods := Dictionary new ].
	^instanceMethods
%

category: 'accessing'
method: CypressClassStructure
instanceVariableNames

	^self properties at: 'instvars' ifAbsent: [#()]
%

category: 'accessing'
method: CypressClassStructure
instanceVariableNames: someStrings

	^self properties at: 'instvars' put: someStrings
%

category: 'converting'
method: CypressClassStructure
instanceVariablesString

	^self stringForVariables: self instanceVariableNames
%

category: 'accessing'
method: CypressClassStructure
isClassExtension

        isClassExtension ifNil: [ isClassExtension := true ].
        ^isClassExtension
%

category: 'accessing'
method: CypressClassStructure
isClassExtension: aBoolean

	isClassExtension := aBoolean
%

category: 'accessing'
method: CypressClassStructure
name

	^self properties at: 'name'
%

category: 'accessing'
method: CypressClassStructure
name: aString

	self properties at: 'name' put: aString
%

category: 'converting'
method: CypressClassStructure
poolDictionariesString
  ^ self stringForVariables: self poolDictionaryNames
%

category: 'accessing'
method: CypressClassStructure
poolDictionaryNames

	^self properties at: 'pools' ifAbsent: [#()]
%

category: 'accessing'
method: CypressClassStructure
poolDictionaryNames: someStrings

	^self properties at: 'pools' put: someStrings
%

category: 'accessing'
method: CypressClassStructure
subclassType

	^self properties at: '_gs_subclassType' ifAbsent: ['']
%

category: 'accessing'
method: CypressClassStructure
subclassType: aString

	aString isEmpty
		ifTrue: [self properties removeKey: '_gs_subclassType' ifAbsent: []]
		ifFalse: [self properties at: '_gs_subclassType' put: aString]
%

category: 'accessing'
method: CypressClassStructure
superclassName

	^self properties at: 'super'
%

category: 'accessing'
method: CypressClassStructure
superclassName: aString

	^self properties at: 'super' put: aString
%

! Class implementation for 'CypressMethodStructure'

!		Class methods for 'CypressMethodStructure'

category: 'instance creation'
classmethod: CypressMethodStructure
fromMethodDefinition: methodDefinition

	^self new
		fromMethodDefinition: methodDefinition;
		yourself
%

!		Instance methods for 'CypressMethodStructure'

category: 'converting'
method: CypressMethodStructure
asCypressMethodDefinition
	"Try to coerce Unicode source to simple Strings when possible."

	^CypressMethodDefinition 
        	className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source asString
%

category: 'accessing'
method: CypressMethodStructure
category

	^self properties at: 'category'
%

category: 'accessing'
method: CypressMethodStructure
category: aString

	self properties at: 'category' put: aString
%

category: 'accessing'
method: CypressMethodStructure
classStructure
	^classStructure
%

category: 'accessing'
method: CypressMethodStructure
classStructure: aCypressClassStructure
	classStructure := aCypressClassStructure
%

category: 'initialization'
method: CypressMethodStructure
fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
%

category: 'accessing'
method: CypressMethodStructure
isMetaclass

	isMetaclass ifNil: [ isMetaclass := false ].
	^isMetaclass
%

category: 'accessing'
method: CypressMethodStructure
isMetaclass: aBoolean
	isMetaclass := aBoolean
%

category: 'accessing'
method: CypressMethodStructure
selector

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self name
		do: [:chara | stream nextPut: (chara = $. ifTrue: [$:] ifFalse: [chara])].
	^stream contents
%

category: 'accessing'
method: CypressMethodStructure
selector: aString

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	aString
		do: [:chara | stream nextPut: (chara = $: ifTrue: [$.] ifFalse: [chara])].
	name := stream contents
%

category: 'accessing'
method: CypressMethodStructure
source

	^source
%

category: 'accessing'
method: CypressMethodStructure
source: aString

	source := aString
%

! Class implementation for 'CypressPackageStructure'

!		Class methods for 'CypressPackageStructure'

category: 'instance creation'
classmethod: CypressPackageStructure
fromPackage: aCypressPackageDefinition

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself
%

category: 'instance creation'
classmethod: CypressPackageStructure
name: aPackageNameString from: classDefinitions classMap: classMap
  ^ self new
    name: aPackageNameString from: classDefinitions classMap: classMap;
    yourself
%

!		Instance methods for 'CypressPackageStructure'

category: 'accessing'
method: CypressPackageStructure
classes

	classes ifNil: [ classes := OrderedCollection new ].
	^classes
%

category: 'accessing'
method: CypressPackageStructure
extensions

	extensions ifNil: [ extensions := OrderedCollection new ].
	^extensions
%

category: 'initialization'
method: CypressPackageStructure
fromPackage: aCypressPackageDefinition
  | snapshot classMap classDefinitions |
  snapshot := aCypressPackageDefinition snapshot.
  classDefinitions := OrderedCollection new.
  classMap := Dictionary new.
  snapshot definitions
    do: [ :definition | 
      definition
        classDefinition: [ :classDefinition | classDefinitions add: classDefinition ]
        methodDefinition: [ :methodDefinition | 
          (classMap at: methodDefinition className ifAbsentPut: [ Set new ])
            add: methodDefinition ] ].
  self
    name: aCypressPackageDefinition name , self packageExtension
    from: classDefinitions
    classMap: classMap
%

category: 'initialization'
method: CypressPackageStructure
name: aString from: classDefinitions classMap: classMap

	| classStructure |
	name := aString.
	properties := Dictionary new.
	classDefinitions do: [:classDefinition |
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure named: className)
			isClassExtension: true;
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].
%

category: 'accessing'
method: CypressPackageStructure
packageExtension

	^self packageExtensionOr: ['.package' ]
%

category: 'accessing'
method: CypressPackageStructure
packageExtensionOr: aBlock

	^self properties at: 'extension' ifAbsent: aBlock
%

category: 'accessing'
method: CypressPackageStructure
packageName

	| extension extensionSize stopIndex |
	extension := self packageExtension.
	extensionSize := extension size.
	stopIndex :=  extensionSize < self name size
		ifTrue: [
			self name
					indexOfSubCollection: extension
					startingAt: self name size - extensionSize + 1
					ifAbsent: [ self name size + 1 ] ]
		ifFalse: [  self name size + 1 ].
	^self name copyFrom: 1 to: stopIndex - 1
%

category: 'accessing'
method: CypressPackageStructure
packageStructure
	^self
%

category: 'snapshotting'
method: CypressPackageStructure
snapshot
	| definitions |
	definitions := OrderedCollection new.
	self classes do: [:classStructure |
        	definitions add: classStructure asCypressClassDefinition.
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	self extensions do: [:classStructure |
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	^ CypressSnapshot definitions: definitions
%

! Class implementation for 'CypressPackageComparator'

!		Class methods for 'CypressPackageComparator'

category: 'instance creation'
classmethod: CypressPackageComparator
comparingPackageNamed: packageName fromDirectory: aDirectory

	^(self new)
		comparingPackageNamed: packageName fromDirectory: aDirectory;
		yourself
%

category: 'instance creation'
classmethod: CypressPackageComparator
new

	^super new
		initialize;
		yourself
%

!		Instance methods for 'CypressPackageComparator'

category: 'comparing - private'
method: CypressPackageComparator
add: aDefinition to: aDictionary

	aDefinition
		classDefinition: [:classDefinition | self addClassDefinition: classDefinition to: aDictionary]
		methodDefinition: [:methodDefinition | self addMethodDefinition: methodDefinition to: aDictionary]
%

category: 'comparing - private'
method: CypressPackageComparator
addClassDefinition: classDefinition to: aDictionary

	(aDictionary at: classDefinition className ifAbsentPut: [Dictionary new])
		at: 'class category' put: classDefinition category;
		at: 'class comment' put: classDefinition comment;
		at: 'class definition' put: classDefinition classDefinitionString.
%

category: 'comparing - private'
method: CypressPackageComparator
addMethodDefinition: methodDefinition to: aDictionary

	((aDictionary at: methodDefinition className ifAbsentPut: [Dictionary new])
		at: (methodDefinition classIsMeta
				ifTrue: ['class methods']
				ifFalse: ['instance methods'])
		ifAbsentPut: [Dictionary new]) at: methodDefinition selector
			put: methodDefinition category -> methodDefinition source
%

category: 'comparing - private'
method: CypressPackageComparator
applyAddition: aCypressAddition

	self add: aCypressAddition definition to: self currentAdditions
%

category: 'comparing - private'
method: CypressPackageComparator
applyModification: aCypressModification

	self
		add: aCypressModification modification to: self currentAdditions;
		add: aCypressModification obsoletion to: self currentRemovals.
%

category: 'comparing - private'
method: CypressPackageComparator
applyRemoval: aCypressRemoval

	self add: aCypressRemoval definition to: self currentRemovals.
%

category: 'comparing'
method: CypressPackageComparator
compare

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.
%

category: 'initializing'
method: CypressPackageComparator
comparingPackages: someNames fromDirectory: aDirectory

	(directoryPackageMap at: aDirectory ifAbsentPut: [OrderedCollection new])
		addAll: someNames.
	someNames do: 
			[:packageName |
			| reader modTime modTimestamp |
			reader := (CypressFileSystemRepository on: aDirectory) reader
						readPackageStructureForPackageNamed: packageName.
			diskSnapshots at: packageName put: reader packageStructure snapshot.
			modTime := System
						performOnServer: 'stat --printf=%y ' , reader packageDirectory.
			modTimestamp := (modTime indexOfSubCollection: 'stat:' startingAt: 1 ifAbsent: [ 0 ]) = 1
						ifTrue: [nil]
						ifFalse: [self dateAndTimeFromUnixFormatString: modTime].
			diskTimestamps at: packageName put: modTimestamp.
			imageSnapshots at: packageName
				put: (CypressPackageDefinition named: packageName) snapshot]
%

category: 'comparing - private'
method: CypressPackageComparator
currentAdditions

	currentAdditions ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentAdditions
%

category: 'comparing - private'
method: CypressPackageComparator
currentDiskSnapshot

	^diskSnapshots at: currentPackageName
%

category: 'comparing - private'
method: CypressPackageComparator
currentImageSnapshot

	^imageSnapshots at: currentPackageName
%

category: 'comparing - private'
method: CypressPackageComparator
currentPatchOperations

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.
%

category: 'comparing - private'
method: CypressPackageComparator
currentRemovals

	currentRemovals ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentRemovals
%

category: 'initializing - private'
method: CypressPackageComparator
dateAndTimeFromUnixFormatString: aString
	"YYYY-MM-DDTHH:MM:SS +HHMM
	 Examples:
		| string |
		string := '2013-06-20 14:47:55.40271592140198 -0700'.
		(DateAndTimeANSI fromUnixFormatString: string) printString = '2013-06-20T14:47:55.40271592140198-07:00'.
	"

	| stream sign positionBias |
	stream := ReadStreamPortable on: aString.
	sign := aString at: aString size - 4.
	positionBias := stream class isLegacyStreamImplementation
				ifTrue: [1]
				ifFalse: [0].
	^DateAndTime
		year: (stream next: 4) asNumber
		month: (stream
				next;
				next: 2) asNumber
		day: (stream
				next;
				next: 2) asNumber
		hour: (stream
				next;
				next: 2) asNumber
		minute: (stream
				next;
				next: 2) asNumber
		second: (stream
				next;
				next: aString size - 6 - stream position + positionBias) asNumber
		offset: (Duration
				days: 0
				hours: (stream
						next;
						next;
						next: 2) asNumber
						* (sign == $- ifTrue: [-1] ifFalse: [1])
				minutes: (stream next: 2) asNumber
				seconds: 0)
%

category: 'comparing'
method: CypressPackageComparator
getDifferences

	self compare.
	^self snapshotDifferences
%

category: 'initializing - private'
method: CypressPackageComparator
initialize

	directoryPackageMap := Dictionary new.
	diskTimestamps := Dictionary new.
	diskSnapshots := Dictionary new.
	imageSnapshots := Dictionary new.
	snapshotDifferences := Dictionary new
		at: 'newer' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		at: 'older' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		yourself.
%

category: 'comparing - private'
method: CypressPackageComparator
resetCurrentForPackage: aStringOrNil

	currentPackageName := aStringOrNil.
	currentAdditions := nil.
	currentRemovals := nil.
%

category: 'accessing'
method: CypressPackageComparator
snapshotDifferences

	^snapshotDifferences
%

category: 'comparing - private'
method: CypressPackageComparator
updateCurrentAdditionsAndRemovals

	| oldTimestamp |
	currentAdditions := (snapshotDifferences at: 'newer')
				at: currentPackageName
				ifAbsentPut: [Dictionary new].
	oldTimestamp := (diskTimestamps at: currentPackageName) ifNil: [^self].
	currentRemovals := (snapshotDifferences at: 'older')
				at: currentPackageName
				ifAbsentPut: [Dictionary with: 'Timestamp' -> oldTimestamp printString].
%

! Class implementation for 'CypressPackageManager'

!		Class methods for 'CypressPackageManager'

category: 'instance creation'
classmethod: CypressPackageManager
new

	^super new
		initialize;
		yourself
%

category: 'accessing'
classmethod: CypressPackageManager
packageNamePermutationsFor: aString
	"Answer the variations on possible package names from the specified string.
	 Each hyphen may possibly separate the package name from a suffix."

	| names |
	names := OrderedCollection new.
	aString doWithIndex: 
			[:each :index |
			(each = $- and: [index > 1])
				ifTrue: [names add: (aString copyFrom: 1 to: index - 1)]].
	aString last ~= $- ifTrue: [names add: aString].
	^names
%

category: 'accessing'
classmethod: CypressPackageManager
potentialPackageNames
	"Answer a list of 'package names' from classes and methods.
	 The class category is the package name, if the class is in a package at all.
	 The method category begins with an asterisk (*) before the package name,
	 but can be continued with other details (e.g., *PackageName-accessing).
	 This version does NOT recognize method category suffixes."

	| classCategories methodCategories |
	classCategories := Set new.
	methodCategories := Set new.
	System myUserProfile symbolList do: 
			[:dict |
			dict do: 
					[:aClass |
					aClass isBehavior and: 
							[classCategories addAll: (self packageNamePermutationsFor: aClass category).
							aClass categorysDo: 
									[:cat :method |
									cat first = $*
										ifTrue: 
											[methodCategories
												addAll: (self packageNamePermutationsFor: (cat copyFrom: 2 to: cat size))]].
							false]]].
	^(Set new)
		addAll: classCategories;
		addAll: methodCategories;
		removeIfPresent: 'User Classes';
		removeIfPresent: 'Kernel';
		sortAscending
%

!		Instance methods for 'CypressPackageManager'

category: 'comparing'
method: CypressPackageManager
compareDefinitionsFromConflictedPackageInformation: aCypressPackageInformation

	| badDefinitions expectedDefinitions |
	badDefinitions := (CypressPackageStructure
				fromPackage: (CypressPackageDefinition
						named: aCypressPackageInformation name))
					snapshot definitions
				asSet.
	expectedDefinitions := OrderedCollection new.
	aCypressPackageInformation competingPackageNames do: 
			[:each |
			expectedDefinitions
				addAll: (CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each)) snapshot
						definitions].
	expectedDefinitions do: [:each | badDefinitions remove: each ifAbsent: []].
	^(badDefinitions collect: [:each | each printString]) sortAscending
%

category: 'comparing'
method: CypressPackageManager
comparePackageFrom: aCypressPackageInformation

	^self comparePackagesFrom: (Array with: aCypressPackageInformation)
%

category: 'updating - private'
method: CypressPackageManager
determineKnownPackages

	^(packageInformationList select: [:each | each repository notNil])
		inject: Dictionary new
		into: 
			[:dict :each |
			dict
				at: each name put: each savedLocation;
				yourself]
%

category: 'initializing - private'
method: CypressPackageManager
initialize

	self refreshPackageInformation.
%

category: 'initializing - private'
method: CypressPackageManager
initializeConflictingPackageNames

	| conflictingPackages |
	conflictingPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			conflictingPackages at: each
				put: (knownPackages keys select: 
							[:knownName |
							knownName ~= each name
								and: [(knownName indexOfSubCollection: each name , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]])].
	conflictingPackages := conflictingPackages reject: [:each | each isEmpty].
	conflictingPackages
		keysAndValuesDo: [:package :conflicts | package beConflictedWith: conflicts]
%

category: 'initializing - private'
method: CypressPackageManager
initializeKnownPackages

	knownPackages := (System myUserProfile objectNamed: #KnownCypressPackages)
				ifNil: [Dictionary new]
%

category: 'initializing - private'
method: CypressPackageManager
initializeKnownRepositories

	knownRepositories := Dictionary new.
	knownPackages asSet
		do: [:each | self repositoryOn: each]
%

category: 'initializing - private'
method: CypressPackageManager
initializePackageInformationList

	| allInterestingNames |
	allInterestingNames := Set new
		addAll: self potentialPackageNames;
		addAll: knownPackages keys;
		sortAscending.
	packageInformationList := allInterestingNames collect: 
					[:each |
					| directory repo |
					directory := knownPackages at: each ifAbsent: [nil].
					repo := directory ifNotNil: [self repositoryOn: directory].
					CypressPackageInformation named: each repository: repo]
%

category: 'initializing - private'
method: CypressPackageManager
initializeQualifiedPackageNames

	| qualifiedPackages |
	qualifiedPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			qualifiedPackages at: each
				put: (knownPackages keys select: 
							[:knownName |
							knownName ~= each name
								and: [(each name indexOfSubCollection: knownName , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]])].
	qualifiedPackages := qualifiedPackages reject: [:each | each isEmpty].
	qualifiedPackages
		keysAndValuesDo: [:package :baseNames | package beQualifiedNameOf: baseNames]
%

category: 'updating'
method: CypressPackageManager
loadPackageFrom: aCypressPackageInformation

	| summary loader |
	loader := (CypressSnapshot definitions: aCypressPackageInformation savedDefinitions)
				updatePackage: (CypressPackageDefinition named: aCypressPackageInformation name).
	summary := Dictionary new.

	loader unloadable notEmpty
		ifTrue: [summary at: 'Unloadable' put: (loader unloadable collect: [:each | each printString])].
	loader errors notEmpty
		ifTrue: [summary at: 'Errors' put: (loader errors collect: [:each | each printString])].
	loader requirements notEmpty
		ifTrue: [summary at: 'Missing Requirements' put: loader requirements asArray].

	^summary
%

category: 'updating'
method: CypressPackageManager
lookForLoadedPackagesIn: aDirectory
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	self lookForLoadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil
%

category: 'updating'
method: CypressPackageManager
lookForLoadedPackagesInRepository: aCypressRepository
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	| packageNames |
	packageNames := aCypressRepository packageNames.
	(self packageInformationList
		select: [:each | packageNames includes: each name])
			do: [:each | each updateKnownPackageRepository: aCypressRepository].
	self saveKnownPackages.
	^nil
%

category: 'updating'
method: CypressPackageManager
lookForUnloadedPackagesIn: aDirectory
	"Load any package names from aDirectory as known packages.
	 This does not load the package contents."

	self lookForUnloadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil
%

category: 'updating'
method: CypressPackageManager
lookForUnloadedPackagesInRepository: aCypressRepository
	"Add known packages for any Cypress file outs in the specified directory."

	| packageNames existingPackageNames |
	packageNames := aCypressRepository packageNames.
	(self packageInformationList
		select: [:each | packageNames includes: each name])
			do: [:each | each updateKnownPackageRepository: aCypressRepository].
	existingPackageNames := self packageInformationList
				collect: [:each | each name].
	(packageNames reject: [:each | existingPackageNames includes: each])
		do: 
			[:each |
			self packageInformationList
				add: (CypressPackageInformation named: each repository: aCypressRepository)].
	self saveKnownPackages.
	^nil
%

category: 'accessing'
method: CypressPackageManager
packageInformationList

	^packageInformationList
%

category: 'accessing'
method: CypressPackageManager
potentialPackageNames

	^self class potentialPackageNames
%

category: 'accessing'
method: CypressPackageManager
refreshedPackageInformationList

	self refreshPackageInformation.
	^self packageInformationList.
%

category: 'updating'
method: CypressPackageManager
refreshPackageInformation

	self
		initializeKnownPackages;
		initializeKnownRepositories;
		initializePackageInformationList;
		initializeConflictingPackageNames;
		initializeQualifiedPackageNames
%

category: 'initializing - private'
method: CypressPackageManager
repositoryOn: aDirectory

	^knownRepositories
		at: aDirectory
		ifAbsentPut: [CypressFileSystemRepository on: aDirectory].
%

category: 'updating - private'
method: CypressPackageManager
saveKnownPackages

	self updateKnownPackages.
	((System myUserProfile resolveSymbol: #KnownCypressPackages)
		ifNil: 
			[(System myUserProfile objectNamed: #UserGlobals)
				addAssociation: #KnownCypressPackages -> Dictionary new])
			value: knownPackages
%

category: 'updating - private'
method: CypressPackageManager
updateKnownPackages

	knownPackages := self determineKnownPackages
%

category: 'updating'
method: CypressPackageManager
updateSavedLocation: aDirectory for: aCypressPackageInformation
	"Update the specified package to reflect the path and repository where the
	 package should be saved."

	aCypressPackageInformation
		updateKnownPackageRepository: (self repositoryOn: aDirectory).
	self saveKnownPackages.
	^nil
%

category: 'writing - private'
method: CypressPackageManager
writeCypressPackageToDiskFrom: aCypressPackageInformation

	| packageStructure |
	packageStructure := CypressPackageStructure
				fromPackage: (CypressPackageDefinition
						named: aCypressPackageInformation name).
	aCypressPackageInformation repository writer
		writePackageStructure: packageStructure
%

category: 'writing'
method: CypressPackageManager
writePackagesToDiskFrom: someCypressPackageInformations

	| packageStructure |
	^someCypressPackageInformations do: 
			[:each |
			packageStructure := CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each name).
			each repository writer writePackageStructure: packageStructure.
			each refresh.
			self saveKnownPackages]
%

category: 'writing - private'
method: CypressPackageManager
writePackageStructure: packageStructure to: aCypressRepository

	aCypressRepository writer writePackageStructure: packageStructure
%

category: 'writing'
method: CypressPackageManager
writePackageToDiskFrom: aCypressPackageInformation

	^self writePackagesToDiskFrom: (Array with: aCypressPackageInformation)
%

! Class implementation for 'CypressPackageManager2'

!		Class methods for 'CypressPackageManager2'

category: 'Instance Creation'
classmethod: CypressPackageManager2
create

	^self new
		initializeFromImage;
		yourself.
%

category: 'Initializing'
classmethod: CypressPackageManager2
initialize

	self savedPackageManagers: IdentityDictionary new
%

category: 'Accessing'
classmethod: CypressPackageManager2
named: aKey
	"Answer the Package Manager previously saved under aKey.
	 It is an error if there was not one saved under that key."

	^self
		named: aKey
		or: [self error: 'No previously saved Package Manager under the key ', aKey printString]
%

category: 'Accessing'
classmethod: CypressPackageManager2
named: aKey or: aBlock
	"Answer the Package Manager previously saved under aKey.
	 Answer the result of evaluating aBlock, if there was not one saved under that key."

	^self savedPackageManagers at: aKey ifAbsent: aBlock
%

category: 'Instance Creation'
classmethod: CypressPackageManager2
new

	^super new
		initialize;
		yourself.
%

category: 'Accessing - private'
classmethod: CypressPackageManager2
packageNamePermutationsFor: aString
	"Answer the variations on possible package names from the specified string.
	 Each hyphen may possibly separate the package name from a suffix."

	| names |
	names := OrderedCollection new.
	aString doWithIndex: 
			[:each :index |
			(each = $- and: [index > 1])
				ifTrue: [names add: (aString copyFrom: 1 to: index - 1)]].
	aString last ~= $- ifTrue: [names add: aString].
	^names
%

category: 'Accessing - private'
classmethod: CypressPackageManager2
potentialPackageNames
	"Answer a list of 'package names' from classes and methods.
	 The class category is the package name, if the class is in a package at all.
	 The method category begins with an asterisk (*) before the package name,
	 but can be continued with other details (e.g., *PackageName-accessing).
	 This version does NOT recognize method category suffixes."

	| classCategories methodCategories |
	classCategories := Set new.
	methodCategories := Set new.
	System myUserProfile symbolList do: 
			[:dict |
			dict do: 
					[:aClass |
					aClass isBehavior and: 
							[classCategories addAll: (self packageNamePermutationsFor: aClass category).
							aClass categorysDo: 
									[:cat :method |
									cat first = $*
										ifTrue: 
											[methodCategories
												addAll: (self packageNamePermutationsFor: (cat copyFrom: 2 to: cat size))]].
							false]]].
	^(Set new)
		addAll: classCategories;
		addAll: methodCategories;
		removeIfPresent: 'User Classes';
		removeIfPresent: 'Kernel';
		sortAscending
%

category: 'Updating'
classmethod: CypressPackageManager2
removePackageManagerSavedAs: aKey
	"Remove the Package Manager previously saved under aKey, if there was one.
	 Answer it or nil if there was not one saved under that key."

	^self savedPackageManagers removeKey: aKey ifAbsent: []
%

category: 'Accessing'
classmethod: CypressPackageManager2
savedPackageManagers

	^SavedPackageManagers
%

category: 'Initializing - private'
classmethod: CypressPackageManager2
savedPackageManagers: anIdentityDictionary

	SavedPackageManagers := anIdentityDictionary
%

!		Instance methods for 'CypressPackageManager2'

category: 'Updating'
method: CypressPackageManager2
addRepository: aRepository to: aKnownPackageInformation

	aKnownPackageInformation addRepository: aRepository.
%

category: 'Updating'
method: CypressPackageManager2
addUnknownPackageNamed: aString

	self packageInformationList
		at: aString
		put: (CypressUnknownPackageInformation named: aString).
%

category: 'Querying'
method: CypressPackageManager2
allResolvedPackageReferences
  | resolved |
  resolved := OrderedCollection new.
  self knownRepositories
    keysAndValuesDo: [ :repoUrl :repo | 
      repo packageNames
        do: [ :packageName | resolved add: (CypressResolvedReference name: packageName repository: repo) ] ].
  ^ resolved asSortedCollection asArray
%

category: 'Updating'
method: CypressPackageManager2
assignRepository: aRepository to: aPackageInformation

	self assignRepository: aRepository toAll: (Array with: aPackageInformation)
%

category: 'Updating'
method: CypressPackageManager2
assignRepository: aRepository toAll: somePackageInformations
	"Assign to those having no repository information and add to those with.
	 Those without a repository need to be converted to Known Package Information instances."

	self knownRepositories at: aRepository url put: aRepository.
	(somePackageInformations reject: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: (self convertToKnown: each)].
	(somePackageInformations select: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: each].
%

category: 'Updating'
method: CypressPackageManager2
convert: anUnknownPackageInformation toConflictingWith: aKnownPackageInformation

	| conflicting |
	conflicting := CypressConflictingPackageInformation
				fromUnknown: anUnknownPackageInformation
				conflictingWith: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: conflicting.
	^conflicting
%

category: 'Updating'
method: CypressPackageManager2
convert: anUnknownPackageInformation toEclipsedBy: aKnownPackageInformation

	| eclipsed |
	eclipsed := CypressEclipsedPackageInformation
				fromUnknown: anUnknownPackageInformation
				eclipsedBy: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: eclipsed.
	^eclipsed
%

category: 'Updating'
method: CypressPackageManager2
convertToKnown: aPackageInformation

	| known |
	known := CypressKnownPackageInformation fromUnknown: aPackageInformation.
	self replace: aPackageInformation with: known.
	(self findPackagesEclipsedBy: known) do: [:each | self convert: each toEclipsedBy: known].
	(self findPackagesConflictingWith: known) do: [:each | self convert: each toConflictingWith: known].
	^known.
%

category: 'Updating'
method: CypressPackageManager2
createRepositoryNamed: aName under: aDirectory alias: aString schema: schemaName

	^CypressFileSystemRepository createOn: (CypressUrl
				absoluteFromText: schemaName
						, (CypressFileUtilities current directoryFromPath: aName
								relativeTo: aDirectory)
							, '/')
		alias: aString
%

category: 'Accessing - private'
method: CypressPackageManager2
findPackagesConflictingWith: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name and: 
					[(aKnownPackageInformation name indexOfSubCollection: each name , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]]
%

category: 'Accessing - private'
method: CypressPackageManager2
findPackagesEclipsedBy: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name and: 
					[(each name indexOfSubCollection: aKnownPackageInformation name , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]]
%

category: 'Initializing - private'
method: CypressPackageManager2
initialize

	self
		knownRepositories: Dictionary new;
		packageInformationList: Dictionary new
%

category: 'Initializing - private'
method: CypressPackageManager2
initializeFromImage

	self initializePackageInformationList.
%

category: 'Initializing - private'
method: CypressPackageManager2
initializePackageInformationList

	self
		packageInformationList: (self potentialPackageNames
				inject: Dictionary new
				into: 
					[:dict :each |
					dict
						at: each put: (CypressUnknownPackageInformation named: each);
						yourself])
%

category: 'Accessing'
method: CypressPackageManager2
knownRepositories

	^knownRepositories
%

category: 'Updating'
method: CypressPackageManager2
knownRepositories: someNamedRepositories

	knownRepositories := someNamedRepositories
%

category: 'Loading'
method: CypressPackageManager2
loadPackageFrom: aKnownPackageInformation defaultSymbolDictionaryName: defaultSymbolDictionaryNameOrNil inRepository: aRepository
  | snapshot summary loader |
  snapshot := (aRepository
    readPackageStructureForPackageNamed: aKnownPackageInformation name) snapshot.
  loader := snapshot
    updatePackage:
      (CypressPackageDefinition named: aKnownPackageInformation name)
    defaultSymbolDictionaryName: defaultSymbolDictionaryNameOrNil.
  summary := Dictionary new.
  loader unloadable notEmpty
    ifTrue: [ 
      summary
        at: 'Unloadable'
        put: (loader unloadable collect: [ :each | each printString ]) ].
  loader errors notEmpty
    ifTrue: [ summary at: 'Errors' put: (loader errors collect: [ :each | each printString ]) ].
  loader requirements notEmpty
    ifTrue: [ summary at: 'Missing Requirements' put: loader requirements asArray ].
  ^ summary
%

category: 'Loading'
method: CypressPackageManager2
loadPackageFrom: aKnownPackageInformation inRepository: aRepository
  ^ self
    loadPackageFrom: aKnownPackageInformation
    defaultSymbolDictionaryName: nil
    inRepository: aRepository
%

category: 'Updating'
method: CypressPackageManager2
lookForPackagesInRepository: aRepository
	"Find the packages in the repository and update the list
	 of package information accordingly."

	| packageNames inImage exImage |
	packageNames := aRepository packageNames.
	inImage := self packageInformationList asArray select: [:each | packageNames includes: each name].
	exImage := packageNames reject: [:each | self packageInformationList anySatisfy: [:info | info name = each]].
	exImage := exImage collect: [:each | CypressUnknownPackageInformation named: each].

	self assignRepository: aRepository toAll: inImage, exImage
%

category: 'Accessing'
method: CypressPackageManager2
packageInformationList

	^packageInformationList
%

category: 'Updating'
method: CypressPackageManager2
packageInformationList: someNamedPackageInformations

	packageInformationList := someNamedPackageInformations
%

category: 'Accessing'
method: CypressPackageManager2
packageInformationNamed: aString

	^self packageInformationNamed: aString
		or: [self error: 'No package information for ' , aString printString]
%

category: 'Accessing'
method: CypressPackageManager2
packageInformationNamed: aString or: aBlock

	^self packageInformationList
		at: aString
		ifAbsent: aBlock.
%

category: 'Accessing - private'
method: CypressPackageManager2
potentialPackageNames

	^self class potentialPackageNames
%

category: 'Updating'
method: CypressPackageManager2
replace: oldPackageInformation with: newPackageInformation

	self packageInformationList
		at: oldPackageInformation name
		put: newPackageInformation.
%

category: 'Initializing - private'
method: CypressPackageManager2
repositoryOn: url alias: aString
	"Answer a repository instance for the specified URL.
	 The characteristics will come from the properties file at the URL location,
	 or will default according to the schema, if there is no properties file."

	^self knownRepositories
		at: url
		ifAbsentPut: [CypressAbstractRepository onUrl: url alias: aString].
%

category: 'Updating'
method: CypressPackageManager2
saveAs: aKey
	"Save the receiver in the class' collection of named managers
	 under the specified key.
	 It will quietly replace anything already under that key."


	self savedPackageManagers at: aKey put: self.
%

category: 'Accessing - private'
method: CypressPackageManager2
savedPackageManagers

	^self class savedPackageManagers
%

category: 'Writing'
method: CypressPackageManager2
writeChangesToAllRepositoriesFor: aPackageInformation

	aPackageInformation writeChangesToAllRepositories.
%

! Class implementation for 'CypressPackageManager3'

!		Class methods for 'CypressPackageManager3'

category: 'testing'
classmethod: CypressPackageManager3
isPackageLoaded: aPackageName

  ^ (CypressPackageDefinition named: aPackageName) snapshot definitions isEmpty not
%

category: 'instance creation'
classmethod: CypressPackageManager3
new
  ^self basicNew initialize
%

!		Instance methods for 'CypressPackageManager3'

category: 'Updating'
method: CypressPackageManager3
addRepository: aRepository
  self knownRepositories at: aRepository url asString put: aRepository
%

category: 'Updating'
method: CypressPackageManager3
addResolvedReference: resolvedReference
  self resolvedPackageReferences add: resolvedReference
%

category: 'Querying'
method: CypressPackageManager3
allResolvedPackageReferences
  | resolved |
  resolved := OrderedCollection new.
  self knownRepositories
    keysAndValuesDo: [ :repoUrl :repo | 
      repo packageNames
        do: [ :packageName | resolved add: (CypressResolvedReference name: packageName repository: repo) ] ].
  ^ resolved asSortedCollection asArray
%

category: 'initialization'
method: CypressPackageManager3
defaultSymbolDictionaryName

  ^defaultSymbolDictionaryName
%

category: 'initialization'
method: CypressPackageManager3
defaultSymbolDictionaryName: aStringOrNil

  defaultSymbolDictionaryName := aStringOrNil
%

category: 'Updating'
method: CypressPackageManager3
initialize
  self
    knownRepositories: Dictionary new;
    resolvedPackageReferences: OrderedCollection new;
    yourself
%

category: 'Accessing'
method: CypressPackageManager3
knownRepositories

   ^knownRepositories
%

category: 'Accessing'
method: CypressPackageManager3
knownRepositories: anObject

   knownRepositories := anObject
%

category: 'loading'
method: CypressPackageManager3
loadResolvedReference: resolvedReference
  | cypressLoader package repository snapshot |
  cypressLoader := CypressLoader new.
  cypressLoader defaultSymbolDictionaryName: self defaultSymbolDictionaryName.
  package := resolvedReference packageDefinition.
  repository := resolvedReference repository.
  snapshot := (repository
    readPackageStructureForPackageNamed: resolvedReference name) snapshot.
  cypressLoader updatePackage: package withSnapshot: snapshot.
  cypressLoader load.
  cypressLoader unloadable notEmpty
    ifTrue: [ self error: 'Unloadable definitions' ].
  cypressLoader errors notEmpty
    ifTrue: [ self error: 'Load errors' ].
  cypressLoader requirements notEmpty
    ifTrue: [ self error: 'Missing Requirements' ]
%

category: 'loading'
method: CypressPackageManager3
loadResolvedReferences
  | cypressLoader |
  cypressLoader := CypressLoader new.
  cypressLoader defaultSymbolDictionaryName: self defaultSymbolDictionaryName.
  self resolvedPackageReferences
    do: [ :resolvedReference | | package repository snapshot |
      package := resolvedReference packageDefinition.
      repository := resolvedReference repository.
      snapshot := (repository
        readPackageStructureForPackageNamed: resolvedReference name) snapshot.
      cypressLoader updatePackage: package withSnapshot: snapshot ].
  cypressLoader load.
  cypressLoader unloadable notEmpty
    ifTrue: [ self error: 'Unloadable definitions' ].
  cypressLoader errors notEmpty
    ifTrue: [ self error: 'Load errors' ].
  cypressLoader requirements notEmpty
    ifTrue: [ self error: 'Missing Requirements' ]
%

category: 'Accessing'
method: CypressPackageManager3
resolvedPackageReferences
  ^ resolvedPackageReferences
%

category: 'Accessing'
method: CypressPackageManager3
resolvedPackageReferences: anObject
  resolvedPackageReferences := anObject
%

category: 'Unloading'
method: CypressPackageManager3
unloadPackage: aPackage

  | loader summary |
  loader := (CypressPackageDefinition named: aPackage name) snapshot
              unload.
  summary := Dictionary new.
  loader unloadable notEmpty
    ifTrue: [ 
      summary
        at: 'Unloadable'
        put: (loader unloadable collect: [ :each | each printString ]) ].
  loader errors notEmpty
    ifTrue: [ summary at: 'Errors' put: (loader errors collect: [ :each | each printString ]) ].
  loader requirements notEmpty
    ifTrue: [ summary at: 'Missing Requirements' put: loader requirements asArray ].
  ^ summary
%

category: 'Unloading'
method: CypressPackageManager3
unloadPackageNamed: aPackageName
  ^ self unloadPackage: (CypressPackageDefinition named: aPackageName)
%

! Class implementation for 'CypressEnvironmentPackageManager'

!		Instance methods for 'CypressEnvironmentPackageManager'

category: 'accessing'
method: CypressEnvironmentPackageManager
compilationSymbolList
  ^ compilationSymbolList
    ifNil: [ compilationSymbolList := self defaultSymbolList ]
%

category: 'accessing'
method: CypressEnvironmentPackageManager
compilationSymbolList: anObject

   compilationSymbolList := anObject
%

category: 'accessing'
method: CypressEnvironmentPackageManager
defaultEnvironmentId

   ^defaultEnvironmentId
%

category: 'accessing'
method: CypressEnvironmentPackageManager
defaultEnvironmentId: anObject

   defaultEnvironmentId := anObject
%

category: 'accessing'
method: CypressEnvironmentPackageManager
defaultSymbolList

   ^defaultSymbolList
%

category: 'accessing'
method: CypressEnvironmentPackageManager
defaultSymbolList: anObject

   defaultSymbolList := anObject
%

category: 'loading'
method: CypressEnvironmentPackageManager
loadResolvedReferences
  | cypressLoader |
  cypressLoader := CypressEnvironmentLoader new.
  cypressLoader
    defaultSymbolDictionaryName: self defaultSymbolDictionaryName;
    compilationSymbolList: self compilationSymbolList;
    lookupSymbolList: self lookupSymbolList;
    defaultEnvironmentId: self defaultEnvironmentId.
  self resolvedPackageReferences
    do: [ :resolvedReference | 
      | package repository snapshot |
      package := resolvedReference packageDefinition.
      repository := resolvedReference repository.
      snapshot := (repository
        readPackageStructureForPackageNamed: resolvedReference name) snapshot.
      cypressLoader updatePackage: package withSnapshot: snapshot ].
  cypressLoader load.
  cypressLoader unloadable notEmpty
    ifTrue: [ self error: 'Unloadable definitions' ].
  cypressLoader errors notEmpty
    ifTrue: [ self error: 'Load errors' ].
  cypressLoader requirements notEmpty
    ifTrue: [ self error: 'Missing Requirements' ]
%

category: 'loading'
method: CypressEnvironmentPackageManager
loadResolvedReferences: fileNames
  | cypressLoader doUnloads |
  cypressLoader := CypressEnvironmentLoader new.
  cypressLoader
    defaultSymbolDictionaryName: self defaultSymbolDictionaryName;
    compilationSymbolList: self compilationSymbolList;
    lookupSymbolList: self lookupSymbolList;
    defaultEnvironmentId: self defaultEnvironmentId.
  self resolvedPackageReferences
    do: [ :resolvedReference | 
      | package repository snapshot |
      package := resolvedReference packageDefinition.
      repository := resolvedReference repository.
      snapshot := (repository
        readPackageStructureForPackageNamed: resolvedReference name
        files: fileNames ) snapshot.
      cypressLoader updatePackage: package withSnapshot: snapshot ].
  doUnloads := false .
  cypressLoader load: doUnloads .
  doUnloads ifTrue:[ cypressLoader unloadable notEmpty
      ifTrue: [ self error: 'Unloadable definitions' ]].
  cypressLoader errors notEmpty
    ifTrue: [ self error: 'Load errors' ].
  doUnloads ifTrue:[ cypressLoader requirements notEmpty
      ifTrue: [ self error: 'Missing Requirements' ]].
%

category: 'accessing'
method: CypressEnvironmentPackageManager
lookupSymbolList
  ^ lookupSymbolList ifNil: [ lookupSymbolList := self defaultSymbolList ]
%

category: 'accessing'
method: CypressEnvironmentPackageManager
lookupSymbolList: anObject

   lookupSymbolList := anObject
%

! Class implementation for 'CypressPackageStringComparator'

!		Class methods for 'CypressPackageStringComparator'

category: 'instance creation'
classmethod: CypressPackageStringComparator
comparingPackageNamed: packageName fromDirectory: aDirectory

	^(self new)
		comparingPackageNamed: packageName fromDirectory: aDirectory;
		yourself
%

category: 'instance creation'
classmethod: CypressPackageStringComparator
forCypress

	^(self new)
		comparingPackages: #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer' 'Cypress-Comparison')
			fromDirectory: '/opt/git/CypressReferenceImplementation/';
		yourself
%

category: 'instance creation'
classmethod: CypressPackageStringComparator
new

	^super new
		initialize;
		yourself
%

!		Instance methods for 'CypressPackageStringComparator'

category: 'comparing - private'
method: CypressPackageStringComparator
addAddition: aCypressModification to: aCollection
  aCypressModification definition
    classDefinition: [ :classDefinition | self addClassAddition: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodAddition: aCypressModification to: aCollection ]
%

category: 'comparing - private'
method: CypressPackageStringComparator
addClassAddition: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification definition details).
      #'addition'.
      #'class'.
      (aCypressModification definition classDefinitionString)}
%

category: 'comparing - private'
method: CypressPackageStringComparator
addClassModification: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification modification details).
      #'modification'.
      #'class'.
      (aCypressModification obsoletion classDefinitionString).
      (aCypressModification modification classDefinitionString)}
%

category: 'comparing - private'
method: CypressPackageStringComparator
addClassRemoval: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification definition details).
      #'removal'.
      #'class'.
      (aCypressModification definition classDefinitionString)}
%

category: 'comparing - private'
method: CypressPackageStringComparator
addMethodAddition: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification definition details).
      #'addition'.
      #'method'.
      (aCypressModification definition source)}
%

category: 'comparing - private'
method: CypressPackageStringComparator
addMethodModification: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification modification details).
      #'modification'.
      #'method'.
      (aCypressModification obsoletion source).
      (aCypressModification modification source)}
%

category: 'comparing - private'
method: CypressPackageStringComparator
addMethodRemoval: aCypressModification to: aCollection
  aCollection
    add:
      {(aCypressModification definition details).
      #'removal'.
      #'method'.
      (aCypressModification definition source)}
%

category: 'comparing - private'
method: CypressPackageStringComparator
addModification: aCypressModification to: aCollection
  aCypressModification modification
    classDefinition: [ :classDefinition | self addClassModification: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodModification: aCypressModification to: aCollection ]
%

category: 'comparing - private'
method: CypressPackageStringComparator
addRemoval: aCypressModification to: aCollection
  aCypressModification definition
    classDefinition: [ :classDefinition | self addClassRemoval: aCypressModification to: aCollection ]
    methodDefinition: [ :methodDefinition | self addMethodRemoval: aCypressModification to: aCollection ]
%

category: 'comparing - private'
method: CypressPackageStringComparator
applyAddition: aCypressAddition
  self addAddition: aCypressAddition to: self currentOperations
%

category: 'comparing - private'
method: CypressPackageStringComparator
applyModification: aCypressModification
  self addModification: aCypressModification to: self currentOperations
%

category: 'comparing - private'
method: CypressPackageStringComparator
applyRemoval: aCypressRemoval
  self addRemoval: aCypressRemoval to: self currentOperations
%

category: 'comparing'
method: CypressPackageStringComparator
compare

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.
%

category: 'initializing'
method: CypressPackageStringComparator
comparingPackages: someNames fromDirectory: aDirectory
  (directoryPackageMap at: aDirectory ifAbsentPut: [ OrderedCollection new ])
    addAll: someNames.
  someNames
    do: [ :packageName | 
      | reader |
      reader := (CypressFileSystemRepository on: aDirectory) reader
        readPackageStructureForPackageNamed: packageName.
      diskSnapshots at: packageName put: reader packageStructure snapshot.
      imageSnapshots
        at: packageName
        put: (CypressPackageDefinition named: packageName) snapshot ]
%

category: 'comparing - private'
method: CypressPackageStringComparator
currentDiskSnapshot

	^diskSnapshots at: currentPackageName
%

category: 'comparing - private'
method: CypressPackageStringComparator
currentImageSnapshot

	^imageSnapshots at: currentPackageName
%

category: 'comparing - private'
method: CypressPackageStringComparator
currentOperations
  (self dynamicInstVarAt: #'currentOperations')
    ifNil: [ self updateCurrentOperations ].
  ^ self dynamicInstVarAt: #'currentOperations'
%

category: 'comparing - private'
method: CypressPackageStringComparator
currentOperations: aDictionary
  self dynamicInstVarAt: #'currentOperations' put: aDictionary
%

category: 'comparing - private'
method: CypressPackageStringComparator
currentPatchOperations

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.
%

category: 'comparing'
method: CypressPackageStringComparator
getDifferences

	self compare.
	^self snapshotDifferences
%

category: 'initializing - private'
method: CypressPackageStringComparator
initialize
  directoryPackageMap := Dictionary new.
  diskTimestamps := Dictionary new.
  diskSnapshots := Dictionary new.
  imageSnapshots := Dictionary new.
  snapshotDifferences := Dictionary new
    at: 'operations' put: Dictionary new;
    yourself
%

category: 'comparing - private'
method: CypressPackageStringComparator
resetCurrentForPackage: aStringOrNil
  currentPackageName := aStringOrNil.
  self currentOperations: nil
%

category: 'accessing'
method: CypressPackageStringComparator
snapshotDifferences

	^snapshotDifferences
%

category: 'comparing - private'
method: CypressPackageStringComparator
updateCurrentOperations
  self
    currentOperations:
      ((snapshotDifferences at: 'operations')
        at: currentPackageName
        ifAbsentPut: [ OrderedCollection new ])
%

! Class implementation for 'CypressReference'

!		Class methods for 'CypressReference'

category: 'instance creation'
classmethod: CypressReference
name: aString
  ^ self basicNew initializeName: aString
%

category: 'instance creation'
classmethod: CypressReference
new
  self error: 'Use #name: to initialize the receiver.'
%

!		Instance methods for 'CypressReference'

category: 'comparing'
method: CypressReference
= aReference
	^ self class = aReference class and: [ self name = aReference name ]
%

category: 'comparing'
method: CypressReference
hash
	^ self name hash
%

category: 'initialization'
method: CypressReference
initializeName: aString
	name := aString
%

category: 'private'
method: CypressReference
matches: aResolvedReference
  "Answer true if the receiver matches aResolvedReference."

  self subclassResponsibility: #'matches:'
%

category: 'accessing'
method: CypressReference
name
	"Answer the name of this reference."
	
	^ name
%

category: 'accessing'
method: CypressReference
packageName
  "Answer the package name."

  self subclassResponsibility: #'packageName'
%

category: 'printing'
method: CypressReference
printOn: aStream
  aStream
    nextPutAll: self class name;
    nextPutAll: ' name: ';
    print: self name
%

category: 'querying'
method: CypressReference
resolveAllWith: aPackageManager
  "Answer a sorted collection of all resolved references within aGofer."

  ^ aPackageManager allResolvedPackageReferences
    select: [ :each | self matches: each ]
%

! Class implementation for 'CypressPackageReference'

!		Instance methods for 'CypressPackageReference'

category: 'accessing'
method: CypressPackageReference
branch
	"Answer the branch of the receiver."
	
	^ branch
%

category: 'initialization'
method: CypressPackageReference
initializeName: aString
	super initializeName: aString.
	self parseName: aString
%

category: 'private'
method: CypressPackageReference
matches: aResolvedReference
  ^ self name = aResolvedReference name
%

category: 'accessing'
method: CypressPackageReference
packageDefinition
  "For in-image packages, only the base package name is used (no branch)"

  ^ CypressPackageDefinition named: self packageName
%

category: 'accessing'
method: CypressPackageReference
packageName
  "Answer the package of the receiver."

  ^ package
%

category: 'initialization'
method: CypressPackageReference
parseName: aString
  | basicName index |
  basicName := aString.
  index := basicName indexOfSubCollection: '.' startingAt: 1.
  index = 0
    ifTrue: [ 
      package := basicName.
      branch := '' ]
    ifFalse: [ 
      package := basicName copyFrom: 1 to: index - 1.
      branch := basicName copyFrom: index to: basicName size ]
%

! Class implementation for 'CypressResolvedReference'

!		Class methods for 'CypressResolvedReference'

category: 'instance creation'
classmethod: CypressResolvedReference
name: aString repository: aRepository
	^ self basicNew initializeName: aString repository: aRepository
%

!		Instance methods for 'CypressResolvedReference'

category: 'comparing'
method: CypressResolvedReference
<= aResolvedReference
  ^ self name <= aResolvedReference name
%

category: 'initialization'
method: CypressResolvedReference
initializeName: aString repository: aRepository
	self initializeName: aString.
	repository := aRepository
%

category: 'accessing'
method: CypressResolvedReference
repository
	"Answer the repository of the receiver."
	
	^ repository
%

! Class implementation for 'CypressUrl'

!		Class methods for 'CypressUrl'

category: 'instance creation'
classmethod: CypressUrl
absoluteFromFileNameOrUrlString: aString
	"Return a URL from and handle Strings without schemes
	as local relative FileUrls instead of defaulting to a HttpUrl
	as absoluteFromText: does."

	^(CypressUrl schemeNameForString: aString)
		ifNil: [CypressFileUrl workingDirectory newFromRelativeText: aString]
		ifNotNil: [CypressUrl absoluteFromText: aString]
%

category: 'instance creation'
classmethod: CypressUrl
absoluteFromText: aString
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	"CypressUrl absoluteFromText: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'" 
	"CypressUrl absoluteFromText: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"
	"CypressUrl absoluteFromText: 'telnet:chaos.resnet.gatech.edu'"
	"CypressUrl absoluteFromText: 'file:/etc/passwd'"

	| remainder index scheme fragment newUrl |
	"trim surrounding whitespace"
	remainder := aString trimSeparators.

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0 ifTrue: [
		fragment := remainder copyFrom: index + 1 to: remainder size.
		remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl
%

category: 'parsing'
classmethod: CypressUrl
combine: baseURL withRelative: relURL 
	"Take two URL as string form, combine them and return the corresponding URL in string form"

	^((self absoluteFromText: baseURL) newFromRelativeText: relURL) asString
%

category: 'instance creation'
classmethod: CypressUrl
for: aString
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	^self absoluteFromText: aString
%

category: 'encoding'
classmethod: CypressUrl
isCharacterSafeForHttp: aChar
	"Answer whether a character is 'safe', or needs to be escaped when used, eg, in a URL."

	^aChar codePoint < 128
		and: [aChar isAlphaNumeric or: ['.-_' includes: aChar]]
%

category: 'constants'
classmethod: CypressUrl
schemeName
	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^ nil.
%

category: 'parsing'
classmethod: CypressUrl
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
classmethod: CypressUrl
urlClassForScheme: scheme

	| allSubclasses |
	allSubclasses := self userId = System myUserProfile userId
		ifTrue: [ ClassOrganizer new allSubclassesOf: self ]
		ifFalse: [ (ClassOrganizer newWithRoot: self forUserId: self userId) allSubclassesOf: self ].
	^allSubclasses detect: [:urlClass | urlClass schemeName = scheme]
		ifNone: [CypressGenericUrl]
%

category: 'encoding'
classmethod: CypressUrl
writeWithHttpEscapes: aCollection on: aStream
	"Write the given string or Utf8 on the stream with 'dangerous' characters 
	escaped to their %XX form, for use in HTTP transactions.
	Note that Utf8s containing code points over 128 will not work properly here."

	aCollection do: 
			[:each |
			| char |
			char := each asCharacter.
			(self isCharacterSafeForHttp: char)
				ifTrue: [aStream nextPut: char]
				ifFalse: 
					[| int |
					aStream nextPut: $%.
					int := each asInteger.
					int // 16 printOn: aStream base: 16 showRadix: false.
					int \\ 16 printOn: aStream base: 16 showRadix: false]]
%

!		Instance methods for 'CypressUrl'

category: 'downloading'
method: CypressUrl
activate
	"spawn an external handler for this URL"
	
%

category: 'converting'
method: CypressUrl
asString

	^self printString
%

category: 'converting'
method: CypressUrl
asURI
	^self asString asURI
%

category: 'converting'
method: CypressUrl
asUrl
	^self
%

category: 'converting'
method: CypressUrl
asUrlRelativeTo: aUrl
	^self
%

category: 'accessing'
method: CypressUrl
authority
	^''
%

category: 'encoding'
method: CypressUrl
decodeHttpEscapesOf: aString
	"decode string including %XX form
	 (adapted from Pharo 2.0)"

	| unescaped pos sourceSize |
	unescaped := ReadWriteStreamPortable on: String new.
	pos := 1.
	sourceSize := aString size.
	[pos > sourceSize] whileFalse: 
			[| char |
			char := aString at: pos.
			(char = $% and: [pos + 2 <= sourceSize])
				ifTrue: 
					[| asciiVal |
					asciiVal := ((aString at: pos + 1) asUppercase digitValueInRadix: 16) * 16
								+ ((aString at: pos + 2) asUppercase digitValueInRadix: 16).
					asciiVal > 255 ifTrue: [^aString].
					unescaped nextPut: (Character withValue: asciiVal).
					pos := pos + 3]
				ifFalse: 
					[char = $+
						ifTrue: [unescaped nextPut: Character space]
						ifFalse: [unescaped nextPut: char].
					pos := pos + 1]].
	^unescaped contents
%

category: 'converting'
method: CypressUrl
downloadUrl
	^self asString
%

category: 'fragment'
method: CypressUrl
fragment
	^fragment
%

category: 'downloading'
method: CypressUrl
hasContents
	"whether this URL can download contents to be displayed; if not, it fundamentally requires an outside application to deal with it.  For example, mailto: and telnet: urls"
	^false
%

category: 'parsing'
method: CypressUrl
newFromRelativeText: aString
	"return a URL relative to the current one, given by aString.  For instance, if self is 'http://host/dir/file', and aString is '/dir2/file2', then the return will be a Url for 'http://host/dir2/file2'"

	"if the scheme is the same, or not specified, then use the same class"

	| newSchemeName remainder fragmentStart newFragment newUrl bare |

	bare := aString trimSeparators.
	newSchemeName := CypressUrl schemeNameForString: bare.
	(newSchemeName notNil and: [ newSchemeName ~= self schemeName ]) ifTrue: [
		"different scheme -- start from scratch"
		^CypressUrl absoluteFromText: aString ].

	remainder := bare.

	"remove the fragment, if any"
	fragmentStart := remainder indexOf: $#.
	fragmentStart > 0 ifTrue: [
		newFragment := remainder copyFrom: fragmentStart+1 to: remainder size. 
		remainder := remainder copyFrom: 1 to: fragmentStart-1].

	"remove the scheme name"
	newSchemeName ifNotNil: [
		remainder := remainder copyFrom: (newSchemeName size + 2) to: remainder size ].

	"create and initialize the new url"
	newUrl := self class new privateInitializeFromText: remainder  relativeTo: self.


	"set the fragment"
	newUrl privateFragment: newFragment.


	^newUrl
%

category: 'printing'
method: CypressUrl
printOn: aStream

	^self subclassResponsibility: #printOn:
%

category: 'fragment'
method: CypressUrl
privateFragment: aString
	fragment := aString
%

category: 'parsing'
method: CypressUrl
privateInitializeFromText: aString

	^self subclassResponsibility: #privateInitializeFromText:
%

category: 'parsing'
method: CypressUrl
privateInitializeFromText: aString relativeTo: aUrl
	"initialize from the given string, as a relative URL.  aString will have had the scheme name removed, if it was present to begin with.  If it was, then the scheme name was the same as the receiver's scheme name"

	"by default, just do regular initialization"
	^self privateInitializeFromText: aString
%

category: 'classification'
method: CypressUrl
scheme
	"return a string with the scheme of this URL.  For instance, HTTP"

	^self subclassResponsibility: #scheme
%

category: 'classification'
method: CypressUrl
schemeName
	"return a lowercase string with the scheme of this URL.  For instance, 'http'"

	^self subclassResponsibility: #schemeName
%

category: 'fragment'
method: CypressUrl
withFragment: newFragment
	"return a URL which is the same except that it has a different fragment"
	^self copy privateFragment: newFragment; yourself
%

category: 'fragment'
method: CypressUrl
withoutFragment
	"return a URL which is identical to the receiver except that it has no fragment associated with it"
	^self withFragment: nil
%

category: 'encoding'
method: CypressUrl
writeWithHttpEscapes: aCollection on: aStream

	self class writeWithHttpEscapes: aCollection on: aStream
%

! Class implementation for 'CypressFileUrl'

!		Class methods for 'CypressFileUrl'

category: 'instance creation'
classmethod: CypressFileUrl
absoluteFromText: aString
	"Method that can be called explicitly to create a FileUrl."

	^self new privateInitializeFromText: aString
%

category: 'instance creation'
classmethod: CypressFileUrl
host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean
	"Create a FileUrl."

	^self new host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean
%

category: 'instance creation'
classmethod: CypressFileUrl
pathParts: aCollectionOfPathParts
	"Create a FileUrl."

	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: true
%

category: 'instance creation'
classmethod: CypressFileUrl
pathParts: aCollectionOfPathParts isAbsolute: aBoolean
	"Create a FileUrl."

	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: aBoolean
%

category: 'constants'
classmethod: CypressFileUrl
schemeName
	^'file'
%

category: 'instance creation'
classmethod: CypressFileUrl
workingDirectory

	^self absoluteFromText: CypressFileUtilities current workingDirectory
%

!		Instance methods for 'CypressFileUrl'

category: 'downloading'
method: CypressFileUrl
default
	"Answer a new URL with the receiver's path relative to the current working directory."
	
	self privateInitializeFromText: self pathString relativeTo: self class workingDirectory.
%

category: 'accessing'
method: CypressFileUrl
fileName
	"Return the last part of the path,
	most often a filename but can also be a directory."

	^self path last
%

category: 'testing'
method: CypressFileUrl
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

category: 'downloading'
method: CypressFileUrl
hasContents
	^true
%

category: 'accessing'
method: CypressFileUrl
host
	"Return the host name, either 'localhost', '', or a fully qualified domain name."
	
	^host ifNil: ['']
%

category: 'accessing'
method: CypressFileUrl
host: hostName
	"Set the host name, either 'localhost', '', or a fully qualified domain name."
	
	host := hostName
%

category: 'private-initialization'
method: CypressFileUrl
host: aHostString pathParts: aCollection isAbsolute: aBoolean

	host := aHostString.
	path := aCollection.
	isAbsolute := aBoolean
%

category: 'private-initialization'
method: CypressFileUrl
initializeFromPathString: aPathString
	"<aPathString> is a file path as a String.
	We construct a path collection using various heuristics."

	| pathString hasDriveLetter |
	pathString := aPathString.
	pathString isEmpty ifTrue: [pathString := '/'].
	"Copy without empty string preceeding first / or between duplicated /s."
	path := ((pathString subStrings: '/') copyWithout: '')
				collect: [:token | self decodeHttpEscapesOf: token].

	"A path like 'C:' refers in practice to 'c:/'"
	(pathString last = $/
		or: [(hasDriveLetter := self firstPartIsDriveLetter) and: [path size = 1]])
			ifTrue: [path add: ''].

	"Decide if we are absolute by checking for leading $/ or
	beginning with drive letter. Smarts for other OSes?"
	self isAbsolute: ((pathString at: 1) = $/
				or: [hasDriveLetter ifNil: [self firstPartIsDriveLetter]])
%

category: 'accessing'
method: CypressFileUrl
isAbsolute
	"Should the path be considered absolute to
	the filesystem instead of relative to the default directory?"
 
	^isAbsolute
%

category: 'accessing'
method: CypressFileUrl
isAbsolute: aBoolean
	"Set if the path should be considered absolute to
	the filesystem instead of relative to the default directory."

	isAbsolute := aBoolean
%

category: 'accessing'
method: CypressFileUrl
path
	"Return an ordered collection of the path elements."
	
	^path
%

category: 'accessing'
method: CypressFileUrl
path: aCollection
	"Set the collection of path elements."

	path := aCollection
%

category: 'paths'
method: CypressFileUrl
pathDirString
	"Path to directory as url, using slash as delimiter.
	Filename is left out."

	| s |
	s := WriteStreamPortable on: (String new: 100).
	isAbsolute ifTrue: [s nextPut: $/].
	1 to: self path size - 1
		do: 
			[:ii |
			s
				nextPutAll: (path at: ii);
				nextPut: $/].
	^s contents
%

category: 'paths'
method: CypressFileUrl
pathForDirectory
	"Path using local file system's pathname delimiter.
	DOS paths with drive letters should not
	be prepended with a delimiter even though
	they are absolute. Filename is left out."

	| delimiter s |
	delimiter := CypressFileUtilities current pathNameDelimiter.
	s := WriteStreamPortable on: (String new: 100).
	(self isAbsolute and: [self firstPartIsDriveLetter not])
		ifTrue: [s nextPutAll: delimiter].
	1 to: self path size - 1
		do: 
			[:ii |
			s
				nextPutAll: (path at: ii);
				nextPutAll: delimiter].
	^s contents
%

category: 'paths'
method: CypressFileUrl
pathForFile
  ^ self pathString
%

category: 'private-initialization'
method: CypressFileUrl
pathParts: aCollection isAbsolute: aBoolean

	^self host: nil pathParts: aCollection isAbsolute: aBoolean
%

category: 'paths'
method: CypressFileUrl
pathString
	"Path as it appears in a URL with $/ as delimiter."

	| s first |
	s := WriteStreamPortable on: (String new: 100).

	"isAbsolute ifTrue:[ s nextPut: $/ ]."
	first := true.
	self path do: 
			[:p |
			first ifFalse: [s nextPut: $/].
			first := false.
			self writeWithHttpEscapes: p on: s].
	^s contents
%

category: 'copying'
method: CypressFileUrl
postCopy
	"Be sure not to share the path with the copy."

	super postCopy.
	path := path copy
%

category: 'printing'
method: CypressFileUrl
printOn: aStream
	"Return the FileUrl according to RFC3986
		'file:'['//'<host>]<path>#<fragment>
	Note that <host> being '' is equivalent to 'localhost' and is not printed."

	aStream
		nextPutAll: self schemeName;
		nextPut: $:.

	"File URLs with hosts (which are fairly useless) cannot be relative."
	host isEmpty
		ifFalse: 
			[isAbsolute
				ifFalse: 
					[aStream nextPutAll: '<ErroneousURL>'.
					^nil].
			aStream
				nextPutAll: '//';
				nextPutAll: host].
	isAbsolute ifTrue: [aStream nextPut: $/].
	aStream nextPutAll: self pathString.
	fragment
		ifNotNil: 
			[aStream nextPut: $#.
			self writeWithHttpEscapes: fragment on: aStream]
%

category: 'private-initialization'
method: CypressFileUrl
privateInitializeFromText: aString
	"Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

	| schemeName pathString bare i |
	bare := aString trimSeparators.
	schemeName := CypressUrl schemeNameForString: bare.
	(schemeName isNil or: [schemeName ~= self schemeName])
		ifTrue: 
			[host := ''.
			pathString := bare]
		ifFalse: 
			["First remove schemeName and colon"
			bare := bare copyFrom: schemeName size + 2 to: bare size.
			"A proper file URL then has two slashes before host,
			A malformed URL is interpreted as using syntax file:<path>."
			(bare indexOfSubCollection: '//' startingAt: 1 ifAbsent: [ 0 ]) = 1
				ifTrue: 
					[i := bare indexOf: $/ startingAt: 3.
					i = 0
						ifTrue: 
							[host := bare copyFrom: 3 to: bare size.
							pathString := '']
						ifFalse: 
							[host := bare copyFrom: 3 to: i - 1.
							pathString := bare copyFrom: host size + 3 to: bare size]]
				ifFalse: 
					[host := ''.
					pathString := bare]].
	self initializeFromPathString: pathString
%

category: 'private-initialization'
method: CypressFileUrl
privateInitializeFromText: pathString relativeTo: aUrl
	"<pathString> should be a filesystem path.
	This url is adjusted to be aUrl + the path."

	| newPath |
	self host: aUrl host.
	self initializeFromPathString: pathString.
	self isAbsolute: aUrl isAbsolute.
	newPath := aUrl path copy.
	newPath removeLast.	"empty string that says its a directory"
	path do: 
			[:token |
			(token ~= '..' and: [token ~= '.'])
				ifTrue: [newPath addLast: (self decodeHttpEscapesOf: token)].
			token = '..'
				ifTrue: 
					[newPath isEmpty
						ifFalse: [newPath last = '..' ifFalse: [newPath removeLast]]]
			"token = '.' do nothing"].
	path := newPath
%

category: 'classification'
method: CypressFileUrl
scheme
	^self class schemeName
%

category: 'classification'
method: CypressFileUrl
schemeName
	^self class schemeName
%

! Class implementation for 'CypressAbstractFileUrl'

!		Class methods for 'CypressAbstractFileUrl'

category: 'instance creation'
classmethod: CypressAbstractFileUrl
absoluteFromText: aString
	"Return a URL from a string and handle
	a String without a scheme as a FileUrl."

	"Url absoluteFromText: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'" 
	"Url absoluteFromText: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"
	"Url absoluteFromText: 'telnet:chaos.resnet.gatech.edu'"
	"Url absoluteFromText: 'file:/etc/passwd'"

	| remainder index scheme fragment newUrl |
	"trim surrounding whitespace"
	remainder := aString trimSeparators.

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0 ifTrue: [
		fragment := remainder copyFrom: index + 1 to: remainder size.
		remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl
%

category: 'parsing'
classmethod: CypressAbstractFileUrl
urlClassForScheme: scheme

	scheme isNil ifTrue: [^CypressFileUrl].
	^super urlClassForScheme: scheme
%

!		Instance methods for 'CypressAbstractFileUrl'

category: 'accessing'
method: CypressAbstractFileUrl
codeFormat

	^self subclassResponsibility: #codeFormat.
%

category: 'private'
method: CypressAbstractFileUrl
fileUtils
  ^ CypressFileUtilities current
%

category: 'testing'
method: CypressAbstractFileUrl
isStrict

	^self subclassResponsibility: #isStrict.
%

category: 'accessing'
method: CypressAbstractFileUrl
repositoryClass

	^CypressFileSystemRepository
%

! Class implementation for 'CypressCypressFileUrl'

!		Class methods for 'CypressCypressFileUrl'

category: 'constants'
classmethod: CypressCypressFileUrl
schemeName

	^'cypress'
%

!		Instance methods for 'CypressCypressFileUrl'

category: 'accessing'
method: CypressCypressFileUrl
codeFormat

	^'Cypress'
%

category: 'testing'
method: CypressCypressFileUrl
isStrict

	^true
%

! Class implementation for 'CypressFileTreeFormatFileUrl'

!		Class methods for 'CypressFileTreeFormatFileUrl'

category: 'constants'
classmethod: CypressFileTreeFormatFileUrl
schemeName

	^'cypressft'
%

!		Instance methods for 'CypressFileTreeFormatFileUrl'

category: 'accessing'
method: CypressFileTreeFormatFileUrl
codeFormat

	^'FileTree'
%

category: 'testing'
method: CypressFileTreeFormatFileUrl
isStrict

	^false
%

! Class implementation for 'CypressFileTreeReadOnlyFileUrl'

!		Class methods for 'CypressFileTreeReadOnlyFileUrl'

category: 'constants'
classmethod: CypressFileTreeReadOnlyFileUrl
schemeName

	^'cypressfiletree'
%

!		Instance methods for 'CypressFileTreeReadOnlyFileUrl'

category: 'accessing'
method: CypressFileTreeReadOnlyFileUrl
codeFormat

	^'FileTree'
%

category: 'testing'
method: CypressFileTreeReadOnlyFileUrl
isStrict

	^true
%

! Class implementation for 'CypressGitFileUrl'

!		Class methods for 'CypressGitFileUrl'

category: 'constants'
classmethod: CypressGitFileUrl
schemeName
  "A gitcypress url with a host is the target for a remote. All other parameters are optional.
	Parameters are:
		dir : the directory inside the repository where the target MC packages are.
		branch : the git branch to fetch.
		protocol: the user name part to add to the ssh Url, default to git, but can also be https (which implies read only access).
		readOnly : is the repository read only? If present, reduce the history to a minimum (and change the GUI).
	Alternative url syntax:
		gitcypress://github.com/dalehenrich/filetree:pharo5.0_dev/repository
	with:
		host : github.com
		project : dalehenrich/filetree
		branch : pharo5.0_dev
		dir : repository
"

  ^ 'gitcypress'
%

!		Instance methods for 'CypressGitFileUrl'

category: 'accessing'
method: CypressGitFileUrl
codeFormat

	^'Cypress'
%

category: 'printing'
method: CypressGitFileUrl
httpsAccessString
  ^ 'https://' , self host , self projectPath , '.git'
%

category: 'private-initialization'
method: CypressGitFileUrl
initializeFromPathString: aPathString
  | projectDelim repoDelimIndex branchOrTagDelimIndex |
  projectBranchOrTag := repositoryPath := nil.
  projectDelim := aPathString indexOf: $/ startingAt: 2.
  repoDelimIndex := aPathString indexOf: $/ startingAt: projectDelim + 1.
  (branchOrTagDelimIndex := aPathString indexOf: $:) == 0
    ifTrue: [ repoDelimIndex == 0
        ifTrue: [ self projectPath: aPathString ]
        ifFalse: [ self projectPath: (aPathString copyFrom: 1 to: repoDelimIndex - 1).
          self
            repositoryPath:
              (aPathString copyFrom: repoDelimIndex + 1 to: aPathString size) ] ]
    ifFalse: [ self projectPath: (aPathString copyFrom: 1 to: branchOrTagDelimIndex - 1).
      repoDelimIndex == 0
        ifTrue: [ projectBranchOrTag := aPathString
            copyFrom: branchOrTagDelimIndex + 1
            to: aPathString size ]
        ifFalse: [ self projectPath: (aPathString copyFrom: 1 to: branchOrTagDelimIndex - 1).
          self parseBranchOrTagField: [ :pv :rp | projectBranchOrTag := pv.
              self repositoryPath: rp ] pathString: aPathString
          branchOrTagDelimIndex: branchOrTagDelimIndex ] ]
%

category: 'testing'
method: CypressGitFileUrl
isStrict

	^true
%

category: 'private-initialization'
method: CypressGitFileUrl
parseBranchOrTagField: parseBlock pathString: aPathString branchOrTagDelimIndex: branchOrTagDelimIndex
  | strm done escaped repoDelimIndex |
  strm := WriteStream on: String new.
  repoDelimIndex := branchOrTagDelimIndex + 1.
  escaped := done := false.
  [ done ] whileFalse: [ | char |
      repoDelimIndex > aPathString size
        ifTrue: [ done := true ]
        ifFalse: [ char := aPathString at: repoDelimIndex.
          char == $\
            ifTrue: [ escaped
                ifTrue: [ "$\ not legal in branch name ... literally ignored"
                  escaped := false ]
                ifFalse: [ escaped := true ] ]
            ifFalse: [ char == $/
                ifTrue: [ escaped
                    ifFalse: [ done := true ] ].
              done
                ifFalse: [ strm nextPut: char ].
              escaped := false ].
          repoDelimIndex := repoDelimIndex + 1 ] ].
  repoDelimIndex := repoDelimIndex - 1.
  parseBlock
    value: strm contents
    value: (aPathString copyFrom: repoDelimIndex + 1 to: aPathString size)
%

category: 'printing'
method: CypressGitFileUrl
printOn: aStream

	aStream nextPutAll: self schemeName , '://' , self host.
	aStream
		nextPutAll: self projectPath;
		nextPut: $:.
	self projectBranchOrTag do: 
			[:char |
			char = $/ ifTrue: [aStream nextPut: $\].
			aStream nextPut: char].
	aStream
		nextPut: $/;
		nextPutAll: self repositoryPath
%

category: 'accessing'
method: CypressGitFileUrl
projectBranchOrTag
  ^ projectBranchOrTag ifNil: [ 'master' ]
%

category: 'accessing'
method: CypressGitFileUrl
projectBranchOrTag: anObject

   projectBranchOrTag := anObject
%

category: 'accessing'
method: CypressGitFileUrl
projectPath

   ^projectPath
%

category: 'accessing'
method: CypressGitFileUrl
projectPath: aString
  aString last = self fileUtils pathNameDelimiter last
    ifTrue: [ projectPath := aString copyFrom: 1 to: aString size - 1 ]
    ifFalse: [ projectPath := aString ]
%

category: 'accessing'
method: CypressGitFileUrl
repositoryClass
  ^ CypressFileSystemGitRepository
%

category: 'accessing'
method: CypressGitFileUrl
repositoryPath
  ^ repositoryPath ifNil: [ '' ]
%

category: 'accessing'
method: CypressGitFileUrl
repositoryPath: aString
  (aString size > 0
    and: [ aString last = self fileUtils pathNameDelimiter last ])
    ifTrue: [ repositoryPath := aString copyFrom: 1 to: aString size - 1 ]
    ifFalse: [ repositoryPath := aString ]
%

! Class implementation for 'CypressGitFileTreeUrl'

!		Class methods for 'CypressGitFileTreeUrl'

category: 'constants'
classmethod: CypressGitFileTreeUrl
schemeName
  "A gitfiletree url with a host is the target for a remote. All other parameters are optional.
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
"

  ^ 'gitfiletree'
%

!		Instance methods for 'CypressGitFileTreeUrl'

category: 'accessing'
method: CypressGitFileTreeUrl
codeFormat

	^'FileTree'
%

category: 'testing'
method: CypressGitFileTreeUrl
isStrict

	^false
%

! Class implementation for 'CypressLaxFileUrl'

!		Class methods for 'CypressLaxFileUrl'

category: 'constants'
classmethod: CypressLaxFileUrl
schemeName

	^'cypresslax'
%

!		Instance methods for 'CypressLaxFileUrl'

category: 'accessing'
method: CypressLaxFileUrl
codeFormat

	^'Cypress'
%

category: 'testing'
method: CypressLaxFileUrl
isStrict

	^false
%

! Class implementation for 'CypressSmalltalkUrl'

!		Class methods for 'CypressSmalltalkUrl'

category: 'constants'
classmethod: CypressSmalltalkUrl
schemeName

	^'chunk'
%

!		Instance methods for 'CypressSmalltalkUrl'

category: 'accessing'
method: CypressSmalltalkUrl
codeFormat

	^'Chunk'
%

category: 'testing'
method: CypressSmalltalkUrl
isStrict

	^true
%

category: 'accessing'
method: CypressSmalltalkUrl
repositoryClass

	^CypressSmalltalkRepository
%

! Class implementation for 'CypressTopazUrl'

!		Class methods for 'CypressTopazUrl'

category: 'constants'
classmethod: CypressTopazUrl
schemeName

	^'topaz'
%

!		Instance methods for 'CypressTopazUrl'

category: 'accessing'
method: CypressTopazUrl
codeFormat

	^'Topaz'
%

category: 'testing'
method: CypressTopazUrl
isStrict

	^true
%

category: 'accessing'
method: CypressTopazUrl
repositoryClass

	^CypressTopazRepository
%

! Class implementation for 'CypressGenericUrl'

!		Class methods for 'CypressGenericUrl'

category: 'parsing'
classmethod: CypressGenericUrl
absoluteFromText: aString

	| schemeName locator |
	schemeName := CypressUrl schemeNameForString: aString.
	schemeName ifNil: [^self schemeName: 'xnoscheme' locator: aString].
	locator := aString copyFrom: schemeName size + 2 to: aString size.
	^self schemeName: schemeName locator: locator
%

category: 'instance creation'
classmethod: CypressGenericUrl
schemeName: schemeName  locator: locator
	^self new schemeName: schemeName  locator: locator
%

!		Instance methods for 'CypressGenericUrl'

category: 'access'
method: CypressGenericUrl
locator
	^locator
%

category: 'printing'
method: CypressGenericUrl
printOn: aStream
	
	self schemeName ifNotNil: [
		aStream nextPutAll: self schemeName; nextPut: $:].
	
	aStream nextPutAll: self locator.

	self fragment ifNotNil: [
		aStream nextPut: $#; nextPutAll: self fragment].
%

category: 'parsing'
method: CypressGenericUrl
privateInitializeFromText: aString

	schemeName := CypressUrl schemeNameForString: aString.
	locator := schemeName
				ifNil: [aString]
				ifNotNil: [aString copyFrom: schemeName size + 2 to: aString size]
%

category: 'parsing'
method: CypressGenericUrl
privateInitializeFromText: aString relativeTo: aUrl
	schemeName := aUrl schemeName.
	locator := aString.
%

category: 'classification'
method: CypressGenericUrl
scheme
	^ self schemeName.
%

category: 'access'
method: CypressGenericUrl
schemeName
	^schemeName
%

category: 'private'
method: CypressGenericUrl
schemeName: schemeName0  locator: locator0
	schemeName := schemeName0.
	locator := locator0.
%

! Class implementation for 'CypressBrowserUrl'

!		Class methods for 'CypressBrowserUrl'

category: 'constants'
classmethod: CypressBrowserUrl
schemeName

	^'browser'
%

!		Instance methods for 'CypressBrowserUrl'

category: 'downloading'
method: CypressBrowserUrl
hasContents

	^true
%

! Class implementation for 'CypressMailtoUrl'

!		Class methods for 'CypressMailtoUrl'

category: 'constants'
classmethod: CypressMailtoUrl
schemeName

	^ 'mailto'
%

! Class implementation for 'CypressHierarchicalUrl'

!		Class methods for 'CypressHierarchicalUrl'

category: 'instance creation'
classmethod: CypressHierarchicalUrl
schemeName: schemeName  authority: authority  path: path  query: query
	^self new schemeName: schemeName  authority: authority  path: path  query: query
%

!		Instance methods for 'CypressHierarchicalUrl'

category: 'access'
method: CypressHierarchicalUrl
authority
	^authority
%

category: 'access'
method: CypressHierarchicalUrl
fileName
	"Return the last part of the path,
	most often a filename but does not need to be."

	^self path last
%

category: 'printing'
method: CypressHierarchicalUrl
fullPath

	| ans |
	ans := WriteStreamPortable on: String new.
	path do: 
			[:pathElem |
			ans nextPut: $/.
			self writeWithHttpEscapes: pathElem on: ans].
	self query isNil
		ifFalse: 
			[ans nextPut: $?.
			ans nextPutAll: self query].
	self fragment isNil
		ifFalse: 
			[ans nextPut: $#.
			self writeWithHttpEscapes: self fragment on: ans].
	^ans contents
%

category: 'downloading'
method: CypressHierarchicalUrl
hasContents
	"most of these do...."
	^true
%

category: 'access'
method: CypressHierarchicalUrl
isAbsolute
	
	path size > 0 ifFalse: [^ false].
	(path at: 1) size > 0 ifFalse: [^ false].
	^ ((path at: 1) at: 1) ~~ $.
%

category: 'access'
method: CypressHierarchicalUrl
password
	"http://user:pword@foo.com' asUrl password"
	^password
%

category: 'access'
method: CypressHierarchicalUrl
path
	"return a collection of the decoded path elements, as strings"
	^path
%

category: 'access'
method: CypressHierarchicalUrl
path: aCollection
	"Set the collection of path elements."

	path := aCollection
%

category: 'access'
method: CypressHierarchicalUrl
port
	^port
%

category: 'copying'
method: CypressHierarchicalUrl
postCopy
	"Be sure not to share the path with the copy"

	super postCopy.
	path := path copy
%

category: 'printing'
method: CypressHierarchicalUrl
printOn: aStream

	aStream nextPutAll: self schemeName.
	aStream nextPutAll: '://'.
	self username
		ifNotNil: 
			[self writeWithHttpEscapes: self username on: aStream.
			self password
				ifNotNil: 
					[aStream nextPutAll: ':'.
					self writeWithHttpEscapes: self password on: aStream].
			aStream nextPutAll: '@'].
	aStream nextPutAll: self authority.
	port
		ifNotNil: 
			[aStream
				nextPut: $:;
				nextPutAll: port printString].
	path do: 
			[:pathElem |
			aStream nextPut: $/.
			self writeWithHttpEscapes: pathElem on: aStream].
	self query isNil
		ifFalse: 
			[aStream nextPut: $?.
			aStream nextPutAll: self query].
	self fragment isNil
		ifFalse: 
			[aStream nextPut: $#.
			self writeWithHttpEscapes: self fragment on: aStream]
%

category: 'parsing'
method: CypressHierarchicalUrl
privateInitializeFromText: aString
	| remainder ind specifiedSchemeName |
	remainder := aString.
	schemeName
		ifNil: 
			[specifiedSchemeName := CypressUrl schemeNameForString: remainder.
			specifiedSchemeName
				ifNotNil: 
					[schemeName := specifiedSchemeName.
					remainder := remainder copyFrom: schemeName size + 2 to: remainder size].
			schemeName
				ifNil: 
					["assume HTTP"
					schemeName := 'http']].

	"remove leading // if it's there"
	(remainder indexOfSubCollection: '//' startingAt: 1 ifAbsent: [ 0 ]) = 1
		ifTrue: [remainder := remainder copyFrom: 3 to: remainder size].

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0
		ifTrue: 
			[query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the authority"
	ind := remainder indexOf: $/.
	ind > 0
		ifTrue: 
			[ind = 1
				ifTrue: [authority := '']
				ifFalse: 
					[authority := remainder copyFrom: 1 to: ind - 1.
					remainder := remainder copyFrom: ind + 1 to: remainder size]]
		ifFalse: 
			[authority := remainder.
			remainder := ''].

	"extract the username+password"
	ind := authority indexOf: $@.
	ind > 0
		ifTrue: 
			[username := authority copyFrom: 1 to: ind - 1.
			authority := authority copyFrom: ind + 1 to: authority size.
			ind := username indexOf: $:.
			ind > 0
				ifTrue: 
					[password := (self
								decodeHttpEscapesOf: (username copyFrom: ind + 1 to: username size))
									asByteArray decodeFromUTF8
								asString.
					username := username copyFrom: 1 to: ind - 1]
				ifFalse: [password := nil].
			username := (self decodeHttpEscapesOf: username) asByteArray
						decodeFromUTF8 asString].

	"Extract the port"
	(authority includes: $:)
		ifTrue: 
			[| lastColonIndex portString |
			lastColonIndex := authority findLast: [:c | c = $:].
			portString := authority copyFrom: lastColonIndex + 1 to: authority size.
			(portString size > 0) 
				ifTrue: [ 
					(portString allSatisfy: [:each | each isDigit])
						ifTrue: 
							[port := Integer fromString: portString.
							port > 65535 ifTrue: [self error: 'Invalid port number']]
						ifFalse: [self error: 'Invalid port number']].
			authority := authority copyFrom: 1 to: lastColonIndex - 1].

	"get the path"
	path := self privateParsePath: remainder relativeTo: #()
%

category: 'parsing'
method: CypressHierarchicalUrl
privateInitializeFromText: aString relativeTo: aUrl

	| remainder ind basePath |
	remainder := aString.
	"set the scheme"
	schemeName := aUrl schemeName.

	"a leading // means the authority is specified, meaning it is absolute"
	(remainder indexOfSubCollection: '//' startingAt: 1 ifAbsent: [ 0 ]) = 1
		ifTrue: [^self privateInitializeFromText: aString].

	"otherwise, use the same authority"
	authority := aUrl authority.
	port := aUrl port.
	username := aUrl username.
	password := aUrl password.

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0
		ifTrue: 
			[query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the path"
	(remainder indexOfSubCollection: '/' startingAt: 1 ifAbsent: [ 0 ]) = 1
		ifTrue: [basePath := #()]
		ifFalse: [basePath := aUrl path].
	path := self privateParsePath: remainder relativeTo: basePath
%

category: 'parsing'
method: CypressHierarchicalUrl
privateParsePath: remainder relativeTo: basePath

	| nextTok s parsedPath |
	s := remainder readStream.
	parsedPath := OrderedCollection new.
	parsedPath addAll: basePath.
	parsedPath isEmpty ifFalse: [parsedPath removeLast].
	
	[s peek = $/ ifTrue: [s next].
	nextTok := WriteStreamPortable on: String new.
	[s atEnd or: [s peek = $/]] whileFalse: [nextTok nextPut: s next].
	nextTok := self decodeHttpEscapesOf: nextTok contents.
	nextTok = '..'
		ifTrue: [parsedPath size > 0 ifTrue: [parsedPath removeLast]]
		ifFalse: [nextTok ~= '.' ifTrue: [parsedPath add: nextTok]].
	s atEnd]
			whileFalse.
	parsedPath isEmpty ifTrue: [parsedPath add: ''].
	^parsedPath
%

category: 'access'
method: CypressHierarchicalUrl
query
	"return the query, the part after any ?.  Any %XY's have already been decoded.  If there wasno query part, nil is returned (it is possible to also have an empty query"
	^query 
%

category: 'classification'
method: CypressHierarchicalUrl
scheme
	^ self schemeName.
%

category: 'access'
method: CypressHierarchicalUrl
schemeName
	^schemeName
%

category: 'private'
method: CypressHierarchicalUrl
schemeName: schemeName0  authority: authority0  path: path0  query: query0
	"initialize a new instance"
	schemeName := schemeName0.
	authority := authority0.
	path := path0.
	query := query0.
%

category: 'access'
method: CypressHierarchicalUrl
username
	"http://user:pword@foo.com' asUrl username"
	^username
%

! Class implementation for 'CypressFtpUrl'

!		Class methods for 'CypressFtpUrl'

category: 'constants'
classmethod: CypressFtpUrl
schemeName

	^ 'ftp'.
%

! Class implementation for 'CypressHttpUrl'

!		Class methods for 'CypressHttpUrl'

category: 'constants'
classmethod: CypressHttpUrl
schemeName

	^ 'http'.
%

! Class implementation for 'CypressHttpsUrl'

!		Class methods for 'CypressHttpsUrl'

category: 'constants'
classmethod: CypressHttpsUrl
schemeName

	^ 'https'.
%

! Class implementation for 'CypressVersionReference'

!		Class methods for 'CypressVersionReference'

category: 'instance creation'
classmethod: CypressVersionReference
name: aString

	^(self basicNew)
		initializeName: aString;
		yourself
%

category: 'instance creation'
classmethod: CypressVersionReference
new

	self error: 'Use #name: to initialize the receiver.'
%

!		Instance methods for 'CypressVersionReference'

category: 'comparing'
method: CypressVersionReference
= aReference

	^self class = aReference class
		and: [self name = aReference name]
%

category: 'accessing'
method: CypressVersionReference
author
	"Answer the author of the receiver."
	
	^ author
%

category: 'accessing'
method: CypressVersionReference
branch
	"Answer the branch of the receiver."
	
	^ branch
%

category: 'comparing'
method: CypressVersionReference
hash

	^self name hash
%

category: 'initialization'
method: CypressVersionReference
initializeName: aString

	name := aString.
	self parseName: aString
%

category: 'private'
method: CypressVersionReference
matches: aResolvedReference
	^ self name = aResolvedReference name
%

category: 'accessing'
method: CypressVersionReference
name
	"Answer the name of this reference."
	
	^ name
%

category: 'accessing'
method: CypressVersionReference
packageName
	"Answer the package of the receiver."

	^ package
%

category: 'initialization'
method: CypressVersionReference
parseName: aString

	| basicName lastDotIndex packageDotIndex lastMinusIndex |
	basicName := (aString isEmpty
				or: [aString last isDigit or: [(aString includes: $() not]])
					ifTrue: [aString]
					ifFalse: 
						["up to last (, but not if there's a . after it"
						| parenIndex dotIndex |
						parenIndex := 0.
						dotIndex := 0.
						aString size to: 1
							by: -1
							do: 
								[:i |
								| c |
								c := aString at: i.
								(c = $. and: [dotIndex = 0])
									ifTrue: 
										[dotIndex := i.
										parenIndex := 0].
								(c = $( and: [parenIndex = 0]) ifTrue: [parenIndex := i]].
						aString copyFrom: 1 to: parenIndex - 1].
	lastMinusIndex := 0.
	lastDotIndex := 0.
	basicName size to: 1
		by: -1
		do: 
			[:i |
			| c |
			c := basicName at: i.
			(c = $- and: [lastMinusIndex = 0]) ifTrue: [lastMinusIndex := i].
			(c = $. and: [lastDotIndex = 0]) ifTrue: [lastDotIndex := i]].
	lastMinusIndex = 0 ifTrue: [lastMinusIndex := basicName size + 1].
	package := basicName copyFrom: 1 to: lastMinusIndex - 1.
	branch := ''.
	packageDotIndex := package indexOf: $..
	packageDotIndex > 0
		ifTrue: 
			[branch := package copyFrom: packageDotIndex + 1 to: package size.
			package := package copyFrom: 1 to: packageDotIndex - 1].
	author := lastMinusIndex = 0
				ifTrue: ['']
				ifFalse: 
					[lastDotIndex < lastMinusIndex ifTrue: [lastDotIndex := basicName size + 1].
					basicName copyFrom: lastMinusIndex + 1 to: lastDotIndex - 1].

	"if basicName is of the form anything-something.number, you'll get number.
If it contains no hyphen, or no period after the last hyphen, you get nothing"
	versionNumber := (basicName indexOf: $-) = 0
				ifTrue: [0]
				ifFalse: 
					[| index lastIndex char |
					index := lastIndex := basicName size.
					char := basicName at: index.
					[char = $. | (char = $-)] whileFalse: 
							[index := index - 1.
							char := basicName at: index].
					char = $-
						ifTrue: [0	"No period after last hyphen."]
						ifFalse: 
							[| numberString |
							numberString := basicName copyFrom: index + 1 to: lastIndex.
							(numberString notEmpty
								and: [numberString allSatisfy: [:each | each isDigit]])
									ifTrue: [numberString asInteger]
									ifFalse: [0]]]
%

category: 'printing'
method: CypressVersionReference
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' name: '.
	self name printOn: aStream
%

category: 'accessing'
method: CypressVersionReference
versionNumber
	"Answer the version of the receiver."

	^ versionNumber
%

! Class implementation for 'CypressMessageDigestStream'

!		Class methods for 'CypressMessageDigestStream'

category: 'instance creation'
classmethod: CypressMessageDigestStream
bytes

	^self on: ByteArray new
%

category: 'instance creation'
classmethod: CypressMessageDigestStream
characters

	^self on: String new
%

!		Instance methods for 'CypressMessageDigestStream'

category: 'digests'
method: CypressMessageDigestStream
md5sum

	^self contents md5sum
%

category: 'digests'
method: CypressMessageDigestStream
sha1Sum

	^self contents sha1Sum
%

category: 'digests'
method: CypressMessageDigestStream
sha256Sum

	^self contents sha256Sum
%

category: 'digests'
method: CypressMessageDigestStream
sha512Sum

	^self contents sha512Sum
%

! Class extensions for 'Array'

!		Instance methods for 'Array'

category: '*Cypress-PackageManagement'
method: Array
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	| indent |
	aStream
		nextPutAll: '[';
		lf.
	indent := startIndent + 1.
	1 to: self size
		do: 
			[:index |
			| item |
			item := self at: index.
			indent timesRepeat: [aStream tab].
			item _writeCypressJsonOn: aStream indent: indent.
			index < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [indent timesRepeat: [aStream tab]].
	aStream nextPutAll: ' ]'
%

! Class extensions for 'Behavior'

!		Instance methods for 'Behavior'

category: '*cypress-environmental-tools'
method: Behavior
persistentSuperclassForEnv: envId
  "result will be nil if no methods exist for specified environmentId."

  | mds |
  (mds := methDicts) _isArray
    ifTrue: [ ^ mds atOrNil: envId * 4 + 3 ].
  envId == 0
    ifTrue: [ ^ mds ].
  ^ nil
%

category: '*cypress-environmental-tools'
method: Behavior
persistentSuperclassForEnv: envId put: aValue
  "aValue should be a GsMethodDictionary, or nil ,
   caller responsible for _refreshClassCache "

  <protected>
  | ofs mds |
  (mds := methDicts) _isArray
    ifFalse: [ envId == 0
        ifTrue: [ methDicts := aValue.
          ^ self ].
      mds := {mds}.
      methDicts := mds ].
  ofs := envId * 4 + 3.
  mds size < ofs
    ifTrue: [ mds size: ofs ].
  mds at: ofs put: aValue
%

! Class extensions for 'Boolean'

!		Instance methods for 'Boolean'

category: '*Cypress-PackageManagement'
method: Boolean
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	aStream nextPutAll: self printString
%

! Class extensions for 'CypressAddition'

!		Instance methods for 'CypressAddition'

category: '*cypress-environmental-tools'
method: CypressAddition
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader
  self definition
    loadClassDefinition: aSymbolDictionaryName
    environmentLoader: environmentLoader
%

category: '*cypress-environmental-tools'
method: CypressAddition
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader
  self definition
    loadMethodDefinition: lookupSymbolList
    environmentLoader: environmentLoader
%

category: '*cypress-environmental-tools'
method: CypressAddition
postLoadDefinition: lookupSymbolList environmentId: environmentId
  self definition
    postLoadOver: nil
    lookupSymbolList: lookupSymbolList
    environmentId: environmentId
%

! Class extensions for 'CypressClassDefinition'

!		Instance methods for 'CypressClassDefinition'

category: '*Cypress-Comparison'
method: CypressClassDefinition
category: aString

	category := aString
%

category: '*Cypress-Comparison'
method: CypressClassDefinition
classCreationSelector
  | type |
  type := self subclassType.
  type = ''
    ifTrue: [ ^ 'subclass:' ]
    ifFalse: [ 
      type = 'indexableSubclass'
        ifTrue: [ ^ 'indexableSubclass:' ]
        ifFalse: [ 
          type = 'byteSubclass'
            ifTrue: [ ^ 'byteSubclass:' ]
            ifFalse: [ self error: 'unknown subclass type: ' , type ] ] ]
%

category: '*Cypress-Comparison'
method: CypressClassDefinition
classDefinitionString

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPut: $(;
		nextPutAll: superclassName;
		space;
		nextPutAll: self classCreationSelector;
		space;
		nextPutAll: self name printString.
	self subclassType = 'byteSubclass'
		ifFalse: 
			[stream
				lf;
				tab;
				nextPutAll: 'instVarNames: #(' , self instanceVariablesString , ')'].
	stream
		lf;
		tab;
		nextPutAll: 'classVars: #(' , self classVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'classInstVars: #(' , self classInstanceVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'poolDictionaries: #(' , self poolDictionariesString , ')';
		lf;
		tab;
		nextPutAll: 'inDictionary: ''<not-defined>''';
		nextPut: $);
		lf;
		tab;
		tab;
		nextPutAll: 'category: ' , self category printString , ';';
		lf;
		tab;
		tab;
		nextPutAll: 'comment: ' , self comment printString.
	^stream contents
%

category: '*cypress-environmental-tools'
method: CypressClassDefinition
createOrReviseClass: aSymbolDictionaryName environmentLoader: environmentLoader
  ^ self subclassType = ''
    ifTrue: [ 
      self
        createOrReviseRegularClass: aSymbolDictionaryName
        environmentLoader: environmentLoader ]
    ifFalse: [ 
      self subclassType = 'byteSubclass'
        ifTrue: [ 
          self
            createOrReviseByteClass: aSymbolDictionaryName
            environmentLoader: environmentLoader ]
        ifFalse: [ 
          self subclassType = 'indexableSubclass'
            ifTrue: [ 
              self
                createOrReviseIndexableClass: aSymbolDictionaryName
                environmentLoader: environmentLoader ]
            ifFalse: [ self error: 'unknown subclass type: ' , self subclassType printString ] ] ]
%

category: '*cypress-environmental-tools'
method: CypressClassDefinition
createOrReviseRegularClass: aSymbolDictionaryName environmentLoader: environmentLoader
  "To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

  | superClass lookupSymbolList |
  lookupSymbolList := environmentLoader lookupSymbolList.
  superClass := (lookupSymbolList resolveSymbol: self superclassName) value.
  ^ (superClass
    subclass: self name
    instVarNames: (self instVarNames collect: [ :each | each asSymbol ])
    classVars: (self classVarNames collect: [ :each | each asSymbol ])
    classInstVars: (self classInstVarNames collect: [ :each | each asSymbol ])
    poolDictionaries: #()
    inDictionary:
      ((self symbolDictionaryForClassNamed: self name symbolList: lookupSymbolList)
        ifNil: [ (lookupSymbolList resolveSymbol: aSymbolDictionaryName) value ])
    options: #())
    category: category;
    comment: self comment
%

category: '*cypress-environmental-tools'
method: CypressClassDefinition
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader
  "Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

  | newClass oldClass lookupSymbolList |
  lookupSymbolList := environmentLoader lookupSymbolList.
  self defaultSymbolDictionaryName: aSymbolDictionaryName.
  (lookupSymbolList resolveSymbol: self name)
    ifNotNil: [ :assoc | oldClass := assoc value ].
  newClass := self
    createOrReviseClass: aSymbolDictionaryName
    environmentLoader: environmentLoader.
  (oldClass isNil or: [ newClass == oldClass ])
    ifTrue: [ ^ self ].
  self classNeedingMigration: newClass.
  self
    recompileWithSubclassesFrom: oldClass
    to: newClass
    symbolList: lookupSymbolList 
%

category: '*cypress-environmental-tools'
method: CypressClassDefinition
symbolDictionaryForClassNamed: aString symbolList: aSymbolList
  "Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), answer
	 the result of evaluating aBlock."

  ^ aSymbolList asArray detect: [ :each | each
        anySatisfy: [ :every | every isBehavior and: [ every name asString = aString asString ] ] ]
  ifNone: [  ]
%

! Class extensions for 'CypressClassStructure'

!		Instance methods for 'CypressClassStructure'

category: '*Cypress-MesssageDigest'
method: CypressClassStructure
addToDigest: aMessageDigestStream

	aMessageDigestStream
		tab;
		tab;
		nextPutAll: self class name;
		cr;
		tab;
		tab;
		tab;
		nextPutAll: 'extension:';
		nextPutAll: self isClassExtension printString;
		cr;
		tab;
		tab;
		tab;
		nextPutAll: 'comment:';
		nextPutAll: self comment;
		cr;
		tab;
		tab;
		tab;
		nextPutAll: 'properties:';
		cr;
		tab;
		tab;
		tab;
		tab.
	self properties _writeCypressJsonOn: aMessageDigestStream indent: 4.
	aMessageDigestStream
		cr;
		tab;
		tab;
		tab;
		nextPutAll: 'class methods:';
		cr.
	(self classMethods asSortedCollection: 
			[:a :b |
			(a isMetaclass printString , a selector)
				< (b isMetaclass printString , b selector)])
		do: [:each | each addToDigest: aMessageDigestStream].
	aMessageDigestStream
		tab;
		tab;
		tab;
		nextPutAll: 'instance methods:';
		cr.
	(self instanceMethods asSortedCollection: 
			[:a :b |
			(a isMetaclass printString , a selector)
				< (b isMetaclass printString , b selector)])
		do: [:each | each addToDigest: aMessageDigestStream]
%

category: '*Cypress-MesssageDigest'
method: CypressClassStructure
isSkeleton

	^instanceMethods isNil
		and: [classMethods isNil
		and: [comment isNil
		and: [isClassExtension isNil]]]
%

! Class extensions for 'CypressDefinition'

!		Instance methods for 'CypressDefinition'

category: '*cypress-environmental-tools'
method: CypressDefinition
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader
  "default is to do nothing"

%

category: '*cypress-environmental-tools'
method: CypressDefinition
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader
  "default is to do nothing"

%

category: '*cypress-environmental-tools'
method: CypressDefinition
postLoad: lookupSymbolList environmentId: environmentId
  "noop"

%

category: '*cypress-environmental-tools'
method: CypressDefinition
postLoadOver: aDefinition lookupSymbolList: lookupSymbolList environmentId: environmentId
  self postLoad: lookupSymbolList environmentId: environmentId
%

! Class extensions for 'CypressMethodDefinition'

!		Instance methods for 'CypressMethodDefinition'

category: '*cypress-environmental-tools'
method: CypressMethodDefinition
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader
  | cls actualCls |
  cls := (lookupSymbolList resolveSymbol: self className) value.
  actualCls := self classIsMeta
    ifTrue: [ cls class ]
    ifFalse: [ cls ].
  actualCls
    compileMethod: self source
    dictionaries: environmentLoader compilationSymbolList
    category: self category
    environmentId: environmentLoader defaultEnvironmentId
%

category: '*cypress-environmental-tools'
method: CypressMethodDefinition
postLoadOver: aDefinition lookupSymbolList: lookupSymbolList environmentId: environmentId
  super
    postLoadOver: aDefinition
    lookupSymbolList: lookupSymbolList
    environmentId: environmentId.
  (self isInitializer
    and: [ aDefinition isNil or: [ self source ~= aDefinition source ] ])
    ifTrue: [ 
      (self theNonMetaClass: lookupSymbolList)
        perform: #'initialize'
        env: environmentId ]
%

category: '*cypress-environmental-tools'
method: CypressMethodDefinition
theNonMetaClass: lookupSymbolList
  ^ self
    resolveGlobalNamed: self className
    lookupSymbolList: lookupSymbolList
    or: [  ]
%

! Class extensions for 'CypressMethodStructure'

!		Instance methods for 'CypressMethodStructure'

category: '*Cypress-MesssageDigest'
method: CypressMethodStructure
addToDigest: aMessageDigestStream

	aMessageDigestStream
		tab;
		tab;
		tab;
		tab;
		nextPutAll: self class name;
		cr;
		tab;
		tab;
		tab;
		tab;
		tab;
		nextPutAll: self selector;
		cr;
		tab;
		tab;
		tab;
		tab;
		tab;
		nextPutAll: 'properties:';
		cr;
		tab;
		tab;
		tab;
		tab;
		tab;
		tab.
	self properties _writeCypressJsonOn: aMessageDigestStream indent: 6.
	aMessageDigestStream
		cr;
		tab;
		tab;
		tab;
		tab;
		nextPutAll: 'source:';
		nextPutAll: self source;
		cr
%

category: '*Cypress-MesssageDigest'
method: CypressMethodStructure
isSkeleton

	^source isNil
		and: [classStructure isNil
		and: [isMetaclass isNil]]
%

! Class extensions for 'CypressModification'

!		Instance methods for 'CypressModification'

category: '*cypress-environmental-tools'
method: CypressModification
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader
  self modification
    loadClassDefinition: aSymbolDictionaryName
    environmentLoader: environmentLoader
%

category: '*cypress-environmental-tools'
method: CypressModification
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader
  self modification
    loadMethodDefinition: lookupSymbolList
    environmentLoader: environmentLoader
%

category: '*cypress-environmental-tools'
method: CypressModification
postLoadDefinition: lookupSymbolList environmentId: environmentId
  self modification
    postLoadOver: self obsoletion
    lookupSymbolList: lookupSymbolList
    environmentId: environmentId
%

! Class extensions for 'CypressObject'

!		Instance methods for 'CypressObject'

category: '*cypress-environmental-tools'
method: CypressObject
resolveGlobalNamed: aString lookupSymbolList: lookupSymbolList or: aBlock
  ^ ((lookupSymbolList resolveSymbol: aString) ifNil: [ ^ aBlock value ]) value
%

! Class extensions for 'CypressPackageManager'

!		Instance methods for 'CypressPackageManager'

category: '*Cypress-Comparison'
method: CypressPackageManager
comparePackagesFrom: someCypressPackageInformations

	| packageNames savedLocation comparator |
	packageNames := someCypressPackageInformations collect: 
					[:each |
					savedLocation := each savedLocation.
					each name].
	comparator := CypressPackageComparator new.
	comparator comparingPackages: packageNames fromDirectory: savedLocation.
	^comparator getDifferences
%

! Class extensions for 'CypressPackageStructure'

!		Instance methods for 'CypressPackageStructure'

category: '*Cypress-MesssageDigest'
method: CypressPackageStructure
addToDigest: aMessageDigestStream

	aMessageDigestStream
		nextPutAll: self class name;
		cr;
		tab;
		nextPutAll: 'name:';
		nextPutAll: self name;
		cr;
		tab;
		nextPutAll: 'properties:';
		cr;
		tab;
		tab.
	self properties _writeCypressJsonOn: aMessageDigestStream indent: 2.
	aMessageDigestStream
		cr;
		tab;
		nextPutAll: 'classes:';
		cr.
	(self classes asSortedCollection: [:a :b | a name < b name])
		do: [:each | each addToDigest: aMessageDigestStream].
	aMessageDigestStream
		tab;
		nextPutAll: 'extensions:';
		cr.
	(self extensions asSortedCollection: [:a :b | a name < b name])
		do: [:each | each addToDigest: aMessageDigestStream]
%

category: '*Cypress-MesssageDigest'
method: CypressPackageStructure
isSkeleton

	^(properties isNil or: [properties isEmpty])
		and: [classes isNil
		and: [extensions isNil]]
%

! Class extensions for 'CypressRemoval'

!		Instance methods for 'CypressRemoval'

category: '*cypress-environmental-tools'
method: CypressRemoval
loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader
  CypressError
    signal:
      'inappropriate to send #loadClassDefinition:environmentLoader: to a removal operation'
%

category: '*cypress-environmental-tools'
method: CypressRemoval
loadMethodDefinition: lookupSymbolList environmentLoader: environmentLoader
  CypressError
    signal:
      'inappropriate to send #loadMethodDefinition:environmentLoader: to a removal operation'
%

category: '*cypress-environmental-tools'
method: CypressRemoval
postLoadDefinition: lookupSymbolList environmentId: environmentId
  CypressError
    signal:
      'inappropriate to send #postLoadDefinition:environmentId: to a removal operation'
%

! Class extensions for 'CypressStructure'

!		Instance methods for 'CypressStructure'

category: '*Cypress-MesssageDigest'
method: CypressStructure
addToDigest: aMessageDigestStream

	self subclassResponsibility: #addToDigest:
%

category: '*Cypress-MesssageDigest'
method: CypressStructure
digest
	"Answer a digest of the receiver, unless it is a skeleton (never populated).
	 The digest is not constrained, but is typically a ByteArray or an Integer.
	 In the case of a skeleton, answer nil so there is a distinction between
	 no such package and an empty package."

	| stream |
	self isSkeleton ifTrue: [^nil].
	stream := CypressMessageDigestStream characters.
	self addToDigest: stream.
	^stream md5sum
%

category: '*Cypress-MesssageDigest'
method: CypressStructure
isSkeleton

	^self subclassResponsibility: #isSkeleton
%

! Class extensions for 'Dictionary'

!		Instance methods for 'Dictionary'

category: '*Cypress-PackageManagement'
method: Dictionary
_writeCypressJsonOn: fileStream
	"Private method which may be removed in a future GemStone version."

	self _writeCypressJsonOn: fileStream indent: 0.
	fileStream lf
%

category: '*Cypress-PackageManagement'
method: Dictionary
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	| indent cnt |
	indent := startIndent.
	aStream
		nextPutAll: '{';
		lf.
	cnt := 0.
	indent := indent + 1.
	self keys asSortedCollection do: 
			[:key |
			| value |
			value := self at: key.
			cnt := cnt + 1.
			indent timesRepeat: [aStream tab].
			key _writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value _writeCypressJsonOn: aStream indent: indent.
			cnt < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [indent timesRepeat: [aStream tab]].
	aStream nextPutAll: ' }'
%

! Class extensions for 'Number'

!		Instance methods for 'Number'

category: '*Cypress-PackageManagement'
method: Number
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	aStream nextPutAll: self printString
%

! Class extensions for 'Object'

!		Instance methods for 'Object'

category: '*Cypress-PackageManagement'
method: Object
_writeCypressJsonOn: fileStream
	"Private method which may be removed in a future GemStone version."

	self _writeCypressJsonOn: fileStream indent: 0
%

! Class extensions for 'String'

!		Instance methods for 'String'

category: '*Cypress-PackageManagement'
method: String
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	aStream nextPutAll: '"'.
	CypressUrl
		writeWithHttpEscapes: (CypressObject normalizeLineEndingsOf: self)
		on: aStream.
	aStream nextPutAll: '"'
%

! Class Initialization

run
CypressAbstractRepository initialize.
CypressGemStoneDirectoryUtilities initialize.
CypressPackageManager2 initialize.
%

