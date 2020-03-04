Class {
	#name : 'RowanDebuggerServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_debugBlanksOnly [

	| string debugString |
	self jadeiteIssueTested: #issue262 withTitle: 'Console debug-it does nothing if selected expression has temp variables'.
	string := '     '. 
	debugString := RowanDebuggerService new debugStringFrom: string. 
	self assert: debugString equals: 'nil halt.'
]

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_debugEmptyString [

	| string debugString |
	self jadeiteIssueTested: #issue262 withTitle: 'Console debug-it does nothing if selected expression has temp variables'.
	string := String new.
	debugString := RowanDebuggerService new debugStringFrom: string. 
	self assert: debugString equals: 'nil halt.'
]

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_debugStringBarInString [

	| string debugString |
	self jadeiteIssueTested: #issue262 withTitle: 'Console debug-it does nothing if selected expression has temp variables'.
	string := '| var |
var := ''|'''. 
	debugString := RowanDebuggerService new debugStringFrom: string. 
	self assert: debugString equals: '| var | nil halt. 
var := ''|'''.
]

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_debugStringLeadingWhitespace [

	| string debugString |
	self jadeiteIssueTested: #issue262 withTitle: 'Console debug-it does nothing if selected expression has temp variables'.
	string := '   | var |
var := ''|'''. 
	debugString := RowanDebuggerService new debugStringFrom: string. 
	self assert: debugString equals: '| var | nil halt. 
var := ''|'''.
]

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_debugStringNoTemps [

	| string debugString |
	self jadeiteIssueTested: #issue262 withTitle: 'Console debug-it does nothing if selected expression has temp variables'.
	string := 'abc'. 
	debugString := RowanDebuggerService new debugStringFrom: string. 
	self assert: debugString equals: 'nil halt. ', string
]

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_debugStringTemps [

	| string debugString |
	self jadeiteIssueTested: #issue262 withTitle: 'Console debug-it does nothing if selected expression has temp variables'.
	string := '| ps |
ps := RowanProjectService newNamed: ''RowanSample1''.
ps refresh'. 
	debugString := RowanDebuggerService new debugStringFrom: string. 
	self assert: debugString equals: '| ps | nil halt. 
ps := RowanProjectService newNamed: ''RowanSample1''.
ps refresh'
]

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_malformed [
	"it won't compile but it shouldn't walkback either"
	| string debugString |
	self jadeiteIssueTested: #issue262 withTitle: 'Console debug-it does nothing if selected expression has temp variables'.
	string := 'abc | def | ^abc'.
	debugString := RowanDebuggerService new debugStringFrom: string. 
	self assert: debugString equals: 'nil halt. abc | def | ^abc'.
]

{ #category : 'tests' }
RowanDebuggerServiceTest >> test_saveUniqueProcess [
  "sanity test that saving processes handles extra saves and release attempts"

  | debuggerService jadeiteProcesses |
  self
    jadeiteIssueTested: #'issue729'
    withTitle: 'On opening debugger, get walkback MNU #stackDepth (3.0.93-5)'.
  jadeiteProcesses := SessionTemps current
    at: #'jadeiteProcesses'
    put: Array new.
  self assert: jadeiteProcesses size equals: 0.
  debuggerService := RowanDebuggerService new.
  debuggerService saveProcessOop: GsProcess _current asOop.
  self assert: jadeiteProcesses size equals: 1.
  debuggerService saveProcessOop: GsProcess _current asOop.
  self assert: jadeiteProcesses size equals: 1.	"no growth in processes saved"
  debuggerService saveProcessOop: GsProcess _current asOop.
  self assert: jadeiteProcesses size equals: 1.
  debuggerService releaseProcessOop: GsProcess _current asOop.
  self assert: jadeiteProcesses size equals: 0.
  debuggerService releaseProcessOop: GsProcess _current asOop.
  self assert: jadeiteProcesses size equals: 0.
  debuggerService releaseProcessOop: GsProcess _current asOop.
  self assert: jadeiteProcesses size equals: 0	"handle extra release attempts."
]
