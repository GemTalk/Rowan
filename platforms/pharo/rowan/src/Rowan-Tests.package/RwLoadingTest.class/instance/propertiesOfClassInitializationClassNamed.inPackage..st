support - classInitialization definitions
propertiesOfClassInitializationClassNamed: className inPackage: packageName
  ^ self
    propertiesForClassNamed: className
    super: 'Object'
    instvars: {}
    classinstvars:
      {'classInstVar1'.
      'classInstVar2'}
    classvars: {'ClassVar1'}
    type: 'normal'
    gs_options: {}
    inPackage: packageName