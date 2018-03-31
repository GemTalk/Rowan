Class {
	#name : 'TDRowanClassProtocolListElementBuilder',
	#superclass : 'TDClassicClassProtocolListElementBuilder',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
TDRowanClassProtocolListElementBuilder >> browseSelectorListFor: protocolDefinition topez: aTopez [

	protocolDefinition
		editUsing:
			((TDEditorSpec topez: aTopez editorAspect: #'rowanProtocolSelectors')
				windowName: #'classicSelectors';
				yourself)
]

{ #category : 'accessing' }
TDRowanClassProtocolListElementBuilder >> clientList: listElement [
  | appEnv items ar className isMeta selectors |
  listElement ifNotNil: [ appEnv := listElement topez applicationEnvironment ].
  ar := {(self allProtocol)}
    ,
      ((appEnv categoryNamesFor: self object theBehavior)
        collect: [ :each | each asString ]) sorted.
  className := self object className.
  isMeta := self object isMeta.
  items := ar
    collect: [ :protocol | 
      | def |
      def := (self object asTDProtocolDefinition: protocol)
        appEnv: appEnv;
        yourself.
      protocol = self allProtocol
        ifTrue: [ def syntheticProtocol: true ].
      def ].
  listElement ifNotNil: [ listElement itemCache: items ].
  ^ ar -> nil
]

{ #category : 'menu actions' }
TDRowanClassProtocolListElementBuilder >> newMethodTemplateMenuAction: listElement selectionIndex: selectionIndex [

	| def methodProtocol |
	(selectionIndex == 0
		or: [ self isAllProtocolSelected: listElement selection: selectionIndex ])
		ifTrue: [ methodProtocol := nil ]
		ifFalse: [ methodProtocol := (listElement itemCache at: selectionIndex) methodProtocol ].
	def := (TDMethodTemplateDefinition
		className: self theClass name
		protocol: methodProtocol
		isMeta: self isMeta)
		appEnv: self object appEnv;
		yourself.
	^ def
		editUsing:
			((TDEditorSpec topez: listElement topez editorAspect: #'rowanMethod')
				windowName: #'method';
				yourself)
]
