"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testPropertyDictionaryWrite
    | propertyDictionary stream x |
    propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
    stream := WriteStreamPortable on: String new.
    propertyDictionary writeCypressJsonOn: stream indent: 0.
    self assert: (x := stream contents withUnixLineEndings) equals: self sampleJson withUnixLineEndings
