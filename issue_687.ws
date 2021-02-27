| rowan client coreComponent coreComponentName rowanResolved clientResolved 
	clientPackageNames clientComponentNames platformConditionalAttributes |
"
	Script used to create RowanClientServices component and package structure
		from Rowan:masterV2.1
"
platformConditionalAttributes := {'tests' . 'v2' . 'v2Only' . 'testsV2' . 'stubs' . 'tonel'. 'common' . 'gemstone' . '3.2.15' asRwGemStoneVersionNumber . '3.5.0' asRwGemStoneVersionNumber . '3.6.0' asRwGemStoneVersionNumber . '3.6.1' asRwGemStoneVersionNumber . '3.7.0' asRwGemStoneVersionNumber . }.
rowan := (Rowan projectNamed: 'Rowan') defined.
rowan := rowan read: platformConditionalAttributes.
rowanResolved := rowan _resolvedProject.
coreComponentName := 'RowanClientServices'.
client := (Rowan newProjectNamed: 'RowanClientServices')
	projectsHome: '$ROWAN_PROJECTS_HOME';
	specName: 'RowanClientServices';
	gitUrl: 'file:$ROWAN_PROJECTS_HOME/RowanClientServices';
	customConditionalAttributes: { 'v2' . 'v2Only' .  'stubs' . 'tonel'};
	yourself.
clientResolved := client _resolvedProject resolve.
coreComponent := client addTopLevelComponentNamed: coreComponentName.
clientPackageNames := {}.
clientComponentNames := { 'common/Services' }.
(rowan componentNamed: 'common/Services') componentNames do: [:compName_1 |
	| comp_1 componentPathName pathArray conditionArray pathSegments |
	comp_1 := rowan componentNamed: compName_1.
	(#('v1') includes: comp_1 condition) ifFalse: [
		clientComponentNames add: compName_1.
		pathSegments := compName_1 asFileReference pathSegments.
		pathArray := pathSegments copyFrom: 1 to: pathSegments size -1.
		conditionArray := Array new: pathArray size withAll: 'common'.
		clientResolved
			addComponentStructureFor: 'Services'
			startingAtComponentNamed: coreComponentName
			pathNameArray: pathArray
			conditionPathArray: conditionArray
			comment: ''.
		comp_1 packageNames isEmpty ifFalse: [ self halt ].
		comp_1 componentNames do: [:compName_2 |
			| comp_2 componentPathName pathArray conditionArray pathSegments clientComp_2 |
			comp_2 := rowan componentNamed: compName_2.
			(#('testsV1') includes: comp_2 condition) ifFalse: [
				clientComponentNames add: compName_2.
				pathSegments := compName_2 asFileReference pathSegments.
				pathArray := pathSegments copyFrom: 1 to: pathSegments size -1.
				conditionArray := Array new: pathArray size withAll: 'common'.
				conditionArray at: conditionArray size put: comp_2 condition.
				clientResolved
					componentNamed: compName_2
					ifAbsent: [
						clientResolved
							addComponentStructureFor: 'Services'
							startingAtComponentNamed: coreComponentName
							pathNameArray: pathArray
							conditionPathArray: conditionArray
							comment: '' ].
				clientComp_2 := client componentNamed: compName_2.
				clientComp_2 condition: comp_2 condition.
				comp_2 packageNames isEmpty ifFalse: [
					clientComp_2
						conditionalPackageMapSpecs: comp_2 conditionalPackageMapSpecs;
						packageNames: comp_2 packageNames.
					clientPackageNames addAll: comp_2 packageNames].
				comp_2 componentNames do: [:compName_3 |
					| comp_3 componentPathName pathArray conditionArray pathSegments clientComp_3 |
					clientComponentNames add: compName_3.
					comp_3 := rowan componentNamed: compName_3.
					pathSegments := compName_3 asFileReference pathSegments.
					pathArray := pathSegments copyFrom: 1 to: pathSegments size -1.
					conditionArray := Array new: pathArray size withAll: 'common'.
					conditionArray at: conditionArray size put: comp_3 condition.
					clientResolved
						componentNamed: compName_3
						ifAbsent: [
							clientResolved
								addComponentStructureFor: 'Services'
								startingAtComponentNamed: coreComponentName
								pathNameArray: pathArray
								conditionPathArray: conditionArray
								comment: '' ].
					clientComp_3 := client componentNamed: compName_3.
					clientComp_3 condition: comp_3 condition.
					comp_3 packageNames isEmpty ifFalse: [
						clientComp_3
							conditionalPackageMapSpecs: comp_3 conditionalPackageMapSpecs;
							packageNames: comp_3 packageNames.
						clientPackageNames addAll: comp_3 packageNames].
					comp_3 componentNames do: [:compName_4 |
						| comp_4 componentPathName pathArray conditionArray pathSegments clientComp_4 |
						comp_4 := rowan componentNamed: compName_4.
						clientComponentNames add: compName_4.
						pathSegments := compName_4 asFileReference pathSegments.
						pathArray := pathSegments copyFrom: 1 to: pathSegments size -1.
						conditionArray := Array new: pathArray size withAll: 'common'.
						conditionArray at: conditionArray size put: comp_4 condition.
						clientResolved
							componentNamed: compName_4
							ifAbsent: [
								clientResolved
									addComponentStructureFor: 'Services'
									startingAtComponentNamed: coreComponentName
									pathNameArray: pathArray
									conditionPathArray: conditionArray
									comment: '' ].
						clientComp_4 := client componentNamed: compName_4.
						clientComp_4 condition: comp_4 condition.
						comp_4 packageNames isEmpty ifFalse: [
							clientComp_4
								conditionalPackageMapSpecs: comp_4 conditionalPackageMapSpecs;
								packageNames: comp_4 packageNames.
							clientPackageNames addAll: comp_4 packageNames].
						comp_4 componentNames do: [:compName_5 |
							| comp_5 componentPathName pathArray conditionArray pathSegments clientComp_5 |
							comp_5 := rowan componentNamed: compName_5.
							clientComponentNames add: compName_5.
							pathSegments := compName_5 asFileReference pathSegments.
							pathArray := pathSegments copyFrom: 1 to: pathSegments size -1.
							conditionArray := Array new: pathArray size withAll: 'common'.
							conditionArray at: conditionArray size put: comp_5 condition.
							clientResolved
								componentNamed: compName_5
								ifAbsent: [
									clientResolved
										addComponentStructureFor: 'Services'
										startingAtComponentNamed: coreComponentName
										pathNameArray: pathArray
										conditionPathArray: conditionArray
										comment: '' ].
							clientComp_5 := client componentNamed: compName_5.
							clientComp_5 condition: comp_5 condition.
							comp_5 packageNames isEmpty ifFalse: [
								clientComp_5
									conditionalPackageMapSpecs: comp_5 conditionalPackageMapSpecs;
									packageNames: comp_5 packageNames.
								clientPackageNames addAll: comp_5 packageNames].
							comp_5 componentNames isEmpty ifFalse: [ self halt ].
		]]]]]]].
true ifTrue: [
	| rowanPackageDir |
	rowanPackageDir := rowanResolved repositoryRoot / 'rowan' / 'src'.
	clientPackageNames do: [:packageName |
		clientResolved _projectDefinition _addPackage: (rowanResolved packageNamed: packageName).
		rowanResolved removePackageNamed: packageName.
		].
	clientResolved 
		export;
		exportLoadSpecification;
		yourself.
	clientComponentNames do: [:componentName |
		rowanResolved removeComponentNamed: componentName
		].
	(rowanResolved repositoryRoot / 'rowan' / 'components') ensureDeleteAllChildren.
	rowanPackageDir ensureDeleteAllChildren.
	rowanResolved
		export.
].
{clientResolved . rowanResolved . clientPackageNames . clientComponentNames}
