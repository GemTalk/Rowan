support - basic definitions
basicClassDefinitions: packageName
  ^ Dictionary new
    at: 'TestNormalClass'
      put:
        (self definitionOfNormalClassNamed: 'TestNormalClass' inPackage: packageName);
    at: 'TestNormalDbTransientClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestNormalDbTransientClass'
            type: 'normal'
            instvars: {'instvar1'}
            gs_options: {'dbTransient'}
            inPackage: packageName);
    at: 'TestVariableClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestVariableClass'
            type: 'variable'
            instvars: {}
            gs_options: nil
            inPackage: packageName);
    at: 'TestMyBagClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestMyBagClass'
            superclass: 'Bag'
            type: 'normal'
            instvars: {}
            gs_options: {}
            inPackage: packageName);
    at: 'TestMyRcIdentityBagClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestMyRcIdentityBagClass'
            superclass: 'RcIdentityBag'
            type: 'normal'
            instvars: {}
            gs_options: {}
            inPackage: packageName);
    at: 'TestCustomByteArrayClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestCustomByteArrayClass'
            superclass: 'Object'
            type: 'bytes'
            instvars: {}
            gs_options: {}
            inPackage: packageName);
    at: 'TestMyByteArrayClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestMyByteArrayClass'
            superclass: 'ByteArray'
            type: 'normal'
            instvars: {}
            gs_options: {}
            inPackage: packageName);
    at: 'TestMyStringClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestMyStringClass'
            superclass: 'String'
            type: 'normal'
            instvars: {}
            gs_options: {}
            inPackage: packageName);
    at: 'TestProtoObjectClass'
      put:
        (self
            definitionOfBasicClassNamed: 'TestProtoObjectClass'
            superclass: 'nil'
            type: 'normal'
            instvars: {}
            gs_options: {}
            inPackage: packageName);
    yourself
