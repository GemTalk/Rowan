Class {
	#name : 'RwProjectComponentVisitorV2Test',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests-ComponentsV2'
}

{ #category : 'private' }
RwProjectComponentVisitorV2Test >> _visitVastTonelDemo_555: platformConditionalAttributes projectAlias: projectAlias projectPath: projectPath [
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
]

{ #category : 'tests' }
RwProjectComponentVisitorV2Test >> testBasicVisit_independent [
	"test of RwProjectLoadComponentVisitorV2 as it would be used without a RwResolvedProject."

	| platformConditionalAttributes visitor componentNamesToLoad projectAlias projectPath projectSpecUrl projectSpec |
	platformConditionalAttributes := {'common'.
	'gemstone'.
	('3.5.0' asRwGemStoneVersionNumber)}.
	projectAlias := 'RowanSample9_DiskConfig_Test'.
	componentNamesToLoad := #('Core').

	projectPath := self _cloneRowanSample9: projectAlias.	"clone https://github.com/dalehenrich/RowanSample9"

	visitor := RwIndependentComponentVisitorV2 new
		platformConditionalAttributes: platformConditionalAttributes;
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
]

{ #category : 'tests' }
RwProjectComponentVisitorV2Test >> testVisitVastTonelDemo_555_independent [
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
]
