tests
testWriterReader_Rowan_pharo

	"write Rowan project to alternate directory, using the pharo project attributes to read the project in the first place"

	"execute without errors"

	| platformConfigurationAttributes projectDefinition projectSetDefinition projectSetModification visitor
		projectHome rowanSpec projectAlias writerVisitorClass |

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := (rowanSpec repositoryRootPath , '/test/testRepositories/repos/') asFileReference.
	projectAlias := 'Rowan_', self _repositoryFormat.

	(projectHome / projectAlias / 'src') 
		ensureCreateDirectory;
		deleteAllChildren;
		yourself.

"Read project and packages from disk for Pharo"
	platformConfigurationAttributes := {
		'common'.
		'pharo'.	"want to read pharo packages"
	}.
	projectDefinition := RwComponentProjectDefinition 
		newForUrl: 'file:', rowanSpec repositoryRootPath, '/platforms/pharo/rowan/specs/Rowan_component.ston'.
	projectDefinition projectHome: rowanSpec repositoryRootPath asFileReference parent.
	projectDefinition read: platformConfigurationAttributes.

"Write project and packages to disk in filetree format, for bootstrapping Pharo"
	projectDefinition
		packagesPath: 'src';
		projectHome: projectHome;
		projectAlias: projectAlias;
		packageFormat: self _repositoryFormat;
		yourself.
	projectSetDefinition := RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself.
	projectSetModification := projectSetDefinition compareAgainstBase: RwProjectSetDefinition new.
	writerVisitorClass := self _repositoryFormat = 'tonel'
		ifTrue: [ RwModificationTonelWriterVisitor ]
		ifFalse: [ RwModificationFiletreeWriterVisitor ].
	visitor := writerVisitorClass new.
	visitor visit: projectSetModification.