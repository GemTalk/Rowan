tests
testAdopt_issue481_C

	"https://github.com/GemTalk/Rowan/issues/481"

	"missing instance method extension for unpackaged class"

	| projectName packageNames className1 className2 packageName1 packageName2 
		symDictName projectDefinition notified audit |

	projectName := 'AdoptProject_481'.
	packageName1 := 'Adopt-Core'.
	packageName2 := 'Adopt-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AdoptedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create image artifacts and projectDefinition"
	projectDefinition := self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"remove #foo method"
	(Rowan globalNamed: className2) removeSelector: #foo.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and expect an error"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: Error
		do: [:ex | notified := true ].
	self assert: notified.

"disown the project"
	Rowan projectTools disown disownProjectNamed: projectDefinition name.

"create an empty loaded project"
	self
		_issue481_loadProjectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			symDictName: symDictName.

"adopt and handle the missing method"
	notified := false.
	[ Rowan projectTools adopt adoptProjectDefinition: projectDefinition ]
		on: RwAdoptAuditMethodErrorNotification
		do: [:ex |
			notified := true.
			self assert: ex className = className2.
			self assert: ex isClassExtension.
			self assert: ex selector = #foo.
			self deny: ex isMetaclass.
			self assert: ex packageName = packageName2.
			ex resume ].
	self assert: notified.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.