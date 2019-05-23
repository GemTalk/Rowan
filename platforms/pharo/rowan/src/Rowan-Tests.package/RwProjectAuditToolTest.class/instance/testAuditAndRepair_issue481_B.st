tests
testAuditAndRepair_issue481_B

	"https://github.com/GemTalk/Rowan/issues/481"

	"extra class method for packaged class"

	| projectName packageNames className1 className2 packageName1 packageName2
		symDictName audit symbolList |

	symbolList := Rowan image symbolList.

	projectName := 'AuditProject_481'.
	packageName1 := 'Audit-Core'.
	packageName2 := 'Audit-Extension'.
	packageNames := {packageName1 .  packageName2}.
	symDictName := self _symbolDictionaryName2.
	className1 := 'AuditedClass'.
	className2 := 'ExtendedUnpackagedClass'.

"create loadedProject"
	self 
		_issue481_projectDefinition: projectName  
			packageName1: packageName1 
			packageName2: packageName2 
			className1: className1 
			className2: className2 
			symDictName: symDictName.

"add extra class method"
	(Rowan globalNamed: className1) class
		compileMethod: 'foobar ^''foobar'''
		dictionaries: symbolList
		category: 'accessing'
		environmentId: 0.

"audit - audit errors expected"
	self deny: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"audit - repair audit errors"
	[audit := Rowan projectTools audit auditForProjectNamed: projectName ]
		on: RwAuditMethodErrorNotification
		do: [:ex | 
			"adopt the method"
			Rowan packageTools adopt 
				adoptMethod: ex selector 
					inClassNamed: ex className  
					isMeta: ex isMetaclass 
					intoPackageNamed: ex packageName.
			ex resume: false ].
	self assert: audit isEmpty.

"double check audit - pass"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.