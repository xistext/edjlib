unit BehaviorTree;

{ Base Behavior Tree Classes
  rudimentary level implementation }

{ based on ideas from primarily the first of these links }
{ https://www.gamedeveloper.com/programming/behavior-trees-for-ai-how-they-work#close-modal }
{ https://robohub.org/introduction-to-behavior-trees/ }

{ Erik Donovan Johnson erikquiet@gmail.com
  Free to use and modify in any way you want.
  Example code. No warranty.  Use at your own risk. }

{ TBehaviorRunner keeps track of active node so doesn't have to traverse to find it,
  and provide blackboard data to agent objects (as defined in subclasses)
  Designed so a behavior tree can be shared by many objects without taking more
  data than the TBehaviorRunner and its data stack per object. }

interface


//{$define dbgbehavior} { compiler directive to turn on behavior debug output defined in project options }

uses Classes, SysUtils,
     basedata;

{ behavior status values }
const behavior_notrun  = 0; { not used }
      behavior_running = 1;
      behavior_success = 2;
      behavior_fail    = 3;

type TBehaviorStatus = integer;

     TBehaviorNode = class; { forward }

     { manages the running, data and active state of a behavior tree }
     TBehaviorRunner = class; { forward }

     TBehaviorNotification = procedure( behaviornode : TBehaviorNode;
                                        status       : TBehaviorStatus );

     { root behavior tree class that everything derives from }
     TBehaviorNode = class

       name : string;
       comment : string;

       constructor create( iname : string = '' );
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; virtual;
       function childcount : integer; dynamic;
       function description : string; dynamic;

       class function behaviorclass : string; dynamic;

       function processchildstatus( runner : TBehaviorRunner;
                                    childstatus : TBehaviorStatus ) : TBehaviorStatus; virtual;

     end;

    { single child behavior node that can modify the results of that child in subclasses }
    TBehaviorDecorator = class( TBehaviorNode )
       child : TBehaviorNode;
       constructor create( ichild : TBehaviorNode;
                           iname : string = '' );
       { non core functionality }
       function childcount : integer; override;
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; override;
     end;

    { multichild behavior node.  how those children are processed depends on subclasses }
    TBehaviorComposite = class( TBehaviorNode )
       children : array of TBehaviorNode;
       constructor create2( Child0, Child1 : TBehaviorNode;
                            iname : string = '');
       constructor create3( Child0, Child1, Child2 : TBehaviorNode;
                            iname : string = '' );
       constructor create4( Child0, Child1, Child2, Child3 : TBehaviorNode;
                            iname : string = '' );
       destructor destroy; override;
       function childcount : integer; override;
       procedure add( item : TBehaviorNode );
       function RunNextChild( runner : TBehaviorRunner;
                              secondspassed : single ) : TBehaviorStatus;
       function incchildindex( runner : TBehaviorRunner ) : boolean;
     end;

    { sublass this to make checks and actions in overloaded Run methods }
    TBehaviorLeaf = class( TBehaviorNode )
       function description : string; override;
       class function behaviorclass : string; override;
     end;

    { this leaf does nothing and returns success }
    TBehaviorSucceed = class( TBehaviorLeaf )
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; override;
       class function behaviorclass : string; override;
     end;

    { this leaf does nothing and returns fail }
    TBehaviorFail = class( TBehaviorLeaf )
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; override;
       class function behaviorclass : string; override;
     end;

    { this leaf does nothing and returns running }
    TBehaviorRunning  = class( TBehaviorLeaf )
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; override;
       class function behaviorclass : string; override;
     end;

     (*
     void Wait (float duration)	Run for duration seconds then succeed.
     void Wait (int ticks)	Run for ticks ticks then succeed.
     void RealtimeWait (float duration)	Run for duration unscaled seconds then succeed.
       *)

{ decorators }

    {regardless if child is done running returns success regardless of childsuccess }
    TBehavior_ForceSuccess = class( TBehaviorDecorator )
       function processchildstatus( runner : TBehaviorRunner;
                                    childstatus : TBehaviorStatus ) : TBehaviorStatus; override;
       function description : string; override;
       class function behaviorclass : string; override;
     end;
    {regardless if child is done running returns success regardless of childsuccess }
    TBehavior_ForceFail  = class( TBehaviorDecorator )
       function processchildstatus( runner : TBehaviorRunner;
                                    childstatus : TBehaviorStatus ) : TBehaviorStatus; override;
       function description : string; override;
       class function behaviorclass : string; override;
     end;
    {inverts child results if not running }
    TBehavior_Not  = class( TBehaviorDecorator ) { Not }
       function processchildstatus( runner : TBehaviorRunner;
                                    childstatus : TBehaviorStatus ) : TBehaviorStatus; override;
       function description : string; override;
       class function behaviorclass : string; override;
     end;

    { others to implement... }
    { Repeat }
    { RetryUntilSuccessful }
    { KeepRunningUntilFailure }
    { Delay }
    { MaxNTries }
    { MaxTSecs }

    { https://www.behaviortree.dev/docs/nodes-library/decoratornode/ }

{ composite nodes }

    { Sequence : visit each child in order, starting with the first, and when that
      succeeds will call the second, and so on down the list of children.
      If any child fails it will immediately return failure to the parent. If the
      last child in the sequence succeeds, then the sequence will return success to
      its parent. }

    TBehaviorSequence = class( TBehaviorComposite )
       function Tick( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; override;
       function processchildstatus( runner : TBehaviorRunner;
                                    childstatus : TBehaviorStatus ) : TBehaviorStatus; override;
       function description : string; override;
       class function behaviorclass : string; override;
     end;

    { Selector : Unlike sequence which is AND, requiring all children to succeed to
      return a success, a selector will return a success if any of its children
      succeed and not process any further children. It will process the first child,
      and if it fails will process the second, and if that fails will process the third,
      until a success is reached, at which point it will instantly return success.
      It will fail if all children fail. This means a selector is analagous with an
      OR gate, and as a conditional statement can be used to check multiple conditions
      to see if any one of them is true.  Some people call this a Fallback. }

    TBehaviorSelector = class( TBehaviorComposite )
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; override;
       function processchildstatus( runner : TBehaviorRunner;
                                    childstatus : TBehaviorStatus ) : TBehaviorStatus; override;
       function description : string; override;
       class function behaviorclass : string; override;
     end;

{ keep track of active behavior node and other info (in subclass) }
TBehaviorRunner = class
   RootNode   : TBehaviorNode;
   ActiveNode : TBehaviorNode;
   DataStack  : TBehaviorDataStack;
   NotificationCallback : TBehaviorNotification;
   constructor create( iRootNode : TBehaviorNode );
   destructor destroy; override;
   procedure StackActiveNode( NewActiveNode : TBehaviorNode );
   function RunTick( secondspassed : single ) : TBehaviorStatus; virtual;
   { all Run calls must call this when finished with a current status to update the active node from the stack as needed }
   procedure UpdateActiveRunStatus( istatus : TBehaviorStatus );
 end;

type ttreecallback = procedure( node : TBehaviorNode;
                                parent : TBehaviorNode;
                                data : pointer;
                                indent : integer );

procedure treecallback( node : TBehaviorNode;
                        parent : TBehaviorNode;
                        data : pointer;
                        indent : integer );
procedure iteratetree( rootnode : TBehaviorNode;
                       callback : ttreecallback;
                       data : pointer );

{ branchless ways to set statuses }
function SuccessOrFail( condition : boolean ) : TBehaviorStatus;
function SuccessOrRunning( condition : boolean ) : TBehaviorStatus;
function FailOrRunning( condition : boolean ) : TBehaviorStatus;

procedure NullNotification( behaviornode : TBehaviorNode;
                            status       : TBehaviorStatus );

implementation  //==============================================================

function SuccessOrFail( condition : boolean ) : TBehaviorStatus;
 { branchless test of condition to assign behavior_success or behavior_fail }
 begin
   result := ord( condition ) * behavior_success + ord( not condition ) * behavior_fail;
 end;

function SuccessOrRunning( condition : boolean ) : TBehaviorStatus;
 { branchless test of condition to assign behavior_success or behavior_running }
 begin
   result := ord( condition ) * behavior_success + ord( not condition ) * behavior_running;
 end;

function FailOrRunning( condition : boolean ) : TBehaviorStatus;
{ branchless test of condition to assign behavior_fail or behavior_running }
 begin
   result := ord( condition ) * behavior_fail + ord( not condition ) * behavior_running;
 end;

function limitmax( value : integer;
                   max   : integer) : integer;
 { branchless limit integer value to max }
 var limit : boolean;
 begin
   limit := value > max;
   result := ord( not limit ) * value + ord( limit ) * max;
 end;

procedure NullNotification( behaviornode : TBehaviorNode;
                            status       : TBehaviorStatus );
 begin
 end;

//---------------------------

constructor TBehaviorNode.create( iname : string = '');
 begin
   name := iname;
 end;

function TBehaviorNode.description : string;
 begin
   result := name;
 end;

class function TBehaviorNode.behaviorclass : string;
 begin
   result := 'BehaviorNode';
 end;

function TBehaviorNode.processchildstatus( runner : TBehaviorRunner;
                                          childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := childstatus;
 end;

function TBehaviorNode.childcount : integer;
 begin
   result := 0;
 end;

function TBehaviorNode.Tick( runner : TBehaviorRunner;
                             secondspassed : single ) : TBehaviorStatus;
 begin
   result := behavior_running;
   runner.NotificationCallback( self, result );
  {$ifdef dbgBehavior}
  write( description + '(' );
  {$endif}
 end;

//---------------------------------

function TBehaviorLeaf.description : string;
 begin
   result := behaviorclass;
 end;

class function TBehaviorLeaf.behaviorclass : string;
 begin
   result := 'BehaviorLeaf';
 end;

//---------------------------------

function TBehaviorSucceed.Tick( runner : TBehaviorRunner;
                                secondspassed : single ) : TBehaviorStatus;
 begin
   inherited;
   result := behavior_success;
   runner.UpdateActiveRunStatus( result );
 end;

class function TBehaviorSucceed.behaviorclass : string;
 begin
   result := 'Succeed';
 end;

function TBehaviorFail.Tick( runner : TBehaviorRunner;
                             secondspassed : single ) : TBehaviorStatus;
 begin
   inherited;
   result := behavior_fail;
 end;

class function TBehaviorFail.behaviorclass : string;
 begin
   result := 'Fail';
 end;

//---------------------------------

function TBehaviorRunning.Tick( runner : TBehaviorRunner;
                                secondspassed : single ) : TBehaviorStatus;
 begin
   inherited;
   result := behavior_running;
   runner.UpdateActiveRunStatus( result );
 end;

class function TBehaviorRunning.behaviorclass : string;
 begin
   result := 'Running';
 end;

//---------------------------------

constructor TBehaviorDecorator.create( ichild : TBehaviorNode;
                                       iname : string = '' );
 begin
   inherited create( iname );
   assert( assigned( ichild ));
   child := ichild;
 end;

function TBehaviorDecorator.childcount : integer;
 begin
   result := 1;
 end;

function TBehaviorDecorator.Tick( runner : TBehaviorRunner;
                                  secondspassed : single ) : TBehaviorStatus;
 { inverts child results if not running }
 begin
   inherited; { debug output if needed }
   runner.stackactivenode( child );
   runner.activenode := child;
   result := child.Tick( runner, secondspassed );
   result := processchildstatus( runner, result );
   runner.UpdateActiveRunStatus( result );
 end;

//--------------------------------
{ Composite node is parent class for the list nodes
  !don't use directly }

constructor TBehaviorComposite.create2( Child0, Child1 : TBehaviorNode;
                                        iname : string = '' );
 begin
   inherited create( iname );
   setlength( children, 2 );
   children[0] := Child0;
   children[1] := Child1;
 end;

constructor TBehaviorComposite.create3( Child0, Child1, Child2 : TBehaviorNode;
                                        iname : string = '' );
 begin
   inherited create( iname );
   setlength( children, 3 );
   children[0] := Child0;
   children[1] := Child1;
   children[2] := Child2;
 end;

constructor TBehaviorComposite.create4( Child0, Child1, Child2, Child3 : TBehaviorNode;
                                        iname : string = '' );
 begin
   inherited create( iname );
   setlength( children, 4 );
   children[0] := Child0;
   children[1] := Child1;
   children[2] := Child2;
   children[3] := Child3;
 end;

destructor TBehaviorComposite.destroy;
 var i : integer;
 begin
   for i := 0 to length( children ) - 1 do
      children[i].free;
   setlength( children, 0 );
 end;

function TBehaviorComposite.childcount : integer;
 begin
   result := length( children );
 end;

procedure TBehaviorComposite.add( item : TBehaviorNode );
 var l : integer;
 begin
   l := length( children );
   setlength( children, l + 1 );
   children[l] := item;
 end;

function TBehaviorComposite.RunNextChild( runner : TBehaviorRunner;
                                          secondspassed : single ) : TBehaviorStatus;
 var childix : integer;
     child : TBehaviorNode;
 begin
   if not runner.datastack.peekint( childix ) then
      runner.datastack.pushint( childix ); { push childix to the stack if it wasn't there }
   {$ifdef dbgBehavior}
   write( '['+inttostr( childix )+']');
   {$endif}

   assert( childix < childcount );
   child := children[childix];
   runner.stackactivenode( child );
   result := child.Tick( runner, secondspassed );
 end;

function TBehaviorComposite.incchildindex( runner : TBehaviorRunner ) : boolean;
 var childix : integer;
 begin
   childix := runner.datastack.popint;
   inc( childix );
   result := childix < length( children );
   if result then
      runner.datastack.pushint(childix); { add child ix back to stack }
 end;

//--------------------------------

  function _sequencesuccess( sequence : TBehaviorSequence;
                             runner   : TBehaviorRunner ) : TBehaviorStatus;
   begin
     result := successorrunning( not sequence.incchildindex( runner ));
     { else finished with success }
   end;

  function _sequencefail( sequence : TBehaviorSequence;
                          runner   : TBehaviorRunner ) : TBehaviorStatus;
   var dummy : integer;
   begin
     result := behavior_fail;
     if runner.datastack.peekint( dummy ) then
        runner.datastack.popint; { finished with fail }
   end;

  function _sequencerunning( sequence : TBehaviorSequence;
                             runner   : TBehaviorRunner ) : TBehaviorStatus;
   begin
     result := behavior_running; { running will continue waiting for child, success will halt iteration with success }
   end;

  type tsequenceproc = function( sequence : TBehaviorSequence;
                                 runner : TBehaviorRunner ) : TBehaviorStatus;
  const sequenceprocs : array[behavior_running..behavior_fail] of tsequenceproc =
                             ( {$ifdef fpc}@{$endif}_sequencerunning,
                               {$ifdef fpc}@{$endif}_sequencesuccess,
                               {$ifdef fpc}@{$endif}_sequencefail );

function TBehaviorSequence.processchildstatus( runner : TBehaviorRunner;
                                               childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := sequenceprocs[childstatus]( self, runner );
 end;

function TBehaviorSequence.Tick( runner : TBehaviorRunner;
                                 secondspassed : single ) : TBehaviorStatus;
 { will run one child per tick, finished with first child that is a fail }
 var childix : integer;
 begin
   inherited; { debug output if needed }
   runner.datastack.peekint( childix ); { peek for childix on stack, will be 0 if none }
   assert( childix < childcount );
   result := RunNextChild( runner, secondspassed );
   result := processchildstatus( runner, result );
   runner.UpdateActiveRunStatus( result );
 end;

function TBehaviorSequence.description : string;
 begin
   result := inherited + '-' + char(16);
 end;

class function TBehaviorSequence.behaviorclass : string;
 begin
   result := 'Sequence';
 end;

//--------------------------------

  function _selectorsuccess( selector : TBehaviorSelector;
                             runner   : TBehaviorRunner ) : TBehaviorStatus;
   var dummy : integer;
   begin
     result := behavior_success;
     if runner.datastack.peekint( dummy ) then
        runner.datastack.popint; { finished with success }
   end;

  function _selectorfail( selector : TBehaviorSelector;
                          runner   : TBehaviorRunner ) : TBehaviorStatus;
   begin
     result := failorrunning( not selector.incchildindex( runner )); { else finished with fail }
   end;

  function _selectorrunning( selector : TBehaviorSelector;
                             runner   : TBehaviorRunner ) : TBehaviorStatus;
   begin
     result := behavior_running; { running will continue waiting for child, success will halt iteration with success }
   end;

  type tselectorproc = function( selector : TBehaviorSelector;
                                 runner : TBehaviorRunner ) : TBehaviorStatus;
  const selectorprocs : array[behavior_running..behavior_fail] of tselectorproc =
                              ( {$ifdef fpc}@{$endif}_selectorrunning,
                                {$ifdef fpc}@{$endif}_selectorsuccess,
                                {$ifdef fpc}@{$endif}_selectorfail );

function TBehaviorSelector.processchildstatus( runner : TBehaviorRunner;
                                               childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := selectorprocs[childstatus]( self, runner );
 end;

function TBehaviorSelector.Tick( runner : TBehaviorRunner;
                                 secondspassed : single ) : TBehaviorStatus;
{ will run one child per tick, finished with first child that is a success }
 var childix : integer;
 begin
   inherited; { debug output if needed }
   runner.datastack.peekint( childix ); { peek for childix on stack, will be 0 if none }
   assert( childix < childcount );
   result := RunNextChild( runner, secondspassed );
   result := processChildStatus( runner, result );
   runner.UpdateActiveRunStatus( result );
 end;

function TBehaviorSelector.description : string;
 begin
   result := inherited + '-?';
 end;

class function TBehaviorSelector.behaviorclass : string;
 begin
   result := 'Selector';
 end;

//------------------------------------
{ decorators }

function TBehavior_ForceSuccess.processchildstatus( runner : TBehaviorRunner;
                                                    childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := childstatus;
   if result = behavior_fail then
      result := behavior_success;
 end;


function TBehavior_ForceSuccess.description : string;
 begin
   result := behaviorclass;
 end;

class function TBehavior_ForceSuccess.behaviorclass : string;
 begin
   result := 'Succeed';
 end;

//------------------------------------

function TBehavior_ForceFail.processchildstatus( runner : TBehaviorRunner;
                                                 childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := childstatus;
   if result = behavior_success then
      result := behavior_fail;
 end;

function TBehavior_ForceFail.description : string;
 begin
   result := behaviorclass;
 end;

class function TBehavior_ForceFail.behaviorclass : string;
 begin
   result := 'Fail';
 end;

//------------------------------------

function TBehavior_Not.processchildstatus( runner : TBehaviorRunner;
                                           childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := childstatus;
   case result of
      behavior_success : result := behavior_fail;
      behavior_fail    : result := behavior_success;
    end;
 end;

function TBehavior_Not.description : string;
 begin
   result := behaviorclass;
 end;

class function TBehavior_Not.behaviorclass : string;
 begin
   result := 'Not';
 end;

//------------------------------------

constructor TBehaviorRunner.create( iRootNode : TBehaviorNode );
 begin
   { unowned references to externally owned nodes }
   RootNode   := iRootNode;
   ActiveNode := RootNode;
   { owned datastack of unowned references to externally owned nodes }
   DataStack := TBehaviorDataStack.create;
   NotificationCallback := @NullNotification;
 end;

destructor TBehaviorRunner.destroy;
 begin
   inherited;
   DataStack.Free;
 end;

procedure TBehaviorRunner.StackActiveNode( NewActiveNode : TBehaviorNode );
 { pushes activenode to the stack and sets activenode to NewActiveNode }
 begin
   assert( assigned( activenode ));
   datastack.push( activenode );
   activenode := NewActiveNode;
 end;

function TBehaviorRunner.RunTick( secondspassed : single ) : TBehaviorStatus;
 begin
   if not assigned( activenode ) then  { start over from root }
      activenode := rootnode;
   result := activenode.Tick( self, secondspassed ); { active node will change to the deepest running child }

   { manage the stack when completing the run of children from a different tick }
   if assigned( activenode ) then
    begin
      while ( result <> behavior_running ) and
            (( activenode is TBehaviorComposite ) or ( activenode is TBehaviorDecorator )) do
       begin
         { if already processed, will be 'running', so will do nothing, otherwise
           insures child results of composites and decorators are handled, even when run directly from here. }
         result := activenode.processchildstatus( self, result );
         if result <> behavior_running then
            UpdateActiveRunStatus( result ); { unwind stack if activenode isn't still running }
       end;
    end
   else
    begin
      assert( datastack.IsEmpty );    { stack should be clear when finished }
    end;
 end;

  {$ifdef dbgBehavior}
  const resultstrings : array[behavior_running..behavior_fail] of string =
                             ('):Running','):Success','):Fail' );
  {$endif}

procedure TBehaviorRunner.UpdateActiveRunStatus( istatus : TBehaviorStatus );
 var dum1 : integer;
     dum2 : single;
 begin
   NotificationCallback( activenode, istatus );
   {$ifdef dbgBehavior}
   write( resultstrings[istatus] );
   {$endif}
   if istatus <> behavior_running then
    begin
      while DataStack.peekint( dum1 ) do
         DataStack.popint;                 {! clearing stack like this shouldn't be necessary, but something is leaving this inex on the stack }
      assert( not DataStack.peeksingle( dum2 ));
      if DataStack.IsEmpty then
         activenode := nil
      else
       begin
         activenode := TBehaviorNode( DataStack.pop ); { set active node to prior node in stack after success or fail }

         {!? how to apply istatus to the parent? }
       end;
    end;
 end;

//-----------------------------
// Flat Tree Iterator.
// should be able to replace the oops based recursive iteration

procedure treecallback( node   : TBehaviorNode;
                        parent : TBehaviorNode;
                        data : pointer;
                        indent : integer );
 begin
   {$ifdef dbgBehavior }
   writeln( node.classname + ':' + node.name );
   {$endif}
 end;

type titnodefunc = function( var node : TBehaviorNode ) : boolean of object;

type TTreeIterator = class
        constructor create( icallback : ttreecallback;
                            idata : pointer );
        destructor destroy; override;
        procedure iterate( rootnode : TBehaviorNode );
        protected
        stack : TBehaviorDataStack;
        depth : integer;
        private
        callback : ttreecallback;
        data : pointer;
        itnodefuncs : array[0..2] of titnodefunc;
        function _doleaf( var node : TBehaviorNode ) : boolean;
        function _dodecorator( var node : TBehaviorNode ) : boolean;
        function _docomposite( var node : TBehaviorNode ) : boolean;
      end;

   constructor TTreeIterator.create( icallback : ttreecallback;
                                     idata : pointer );
    begin
      stack := TBehaviorDataStack.create;
      callback := icallback;
      data := idata;
      depth := 0;
      itnodefuncs[0] := {$ifdef fpc}@{$endif}_doleaf;
      itnodefuncs[1] := {$ifdef fpc}@{$endif}_dodecorator;
      itnodefuncs[2] := {$ifdef fpc}@{$endif}_docomposite;
    end;

   destructor TTreeIterator.destroy;
    begin
      stack.free;
    end;

  function TTreeIterator._doleaf( var node : TBehaviorNode ) : boolean;
   var parent : TObject;
   begin
     stack.peekitem(parent);
     callback( node, TBehaviorNode( parent ), data, depth );
     result := true;
   end;

   function TTreeIterator._dodecorator( var node : TBehaviorNode ) : boolean;
    var childix : integer;
        parent : TObject;
    begin
      if stack.peekint(childix) then
         stack.popint;
      result := childix = 1;
      if not result then
       begin
         stack.peekitem(parent);
         callback( node, TBehaviorNode( parent ), data, depth  );
         inc( depth, 1 );
         stack.pushint(1);
         stack.push( node );
         node := TBehaviorDecorator( node ).child;
       end
    end;

   function TTreeIterator._docomposite( var node : TBehaviorNode ) : boolean;
    var c, childix : integer;
        parent : TObject;
    begin
      if stack.peekint(childix) then
         stack.popint
      else
       begin
         stack.peekitem( parent );
         callback( node, TBehaviorNode( parent ), data, depth ); { callback on first pass }
       end;
      c := node.childcount;
      result := childix = c;
      if not result then
       begin
         inc( depth );
         stack.pushint( childix + 1 );
         stack.push( node );
         node := TBehaviorComposite( node ).children[childix];
       end
    end;

   procedure TTreeIterator.iterate( rootnode : TBehaviorNode );
    var currentnode : TBehaviorNode;
    begin
      currentnode := rootnode;
      while assigned( currentnode ) do
       begin
         if itnodefuncs[limitmax( currentnode.childcount, 2 )]( currentnode ) then
          begin { finished }
            dec( depth );
            currentnode := TBehaviorNode( stack.pop );
          end;
       end;
    end;

procedure iteratetree( rootnode : TBehaviorNode;
                       callback : ttreecallback;
                       data : pointer );
 var iterator : TTreeIterator;
 begin
   iterator := TTreeIterator.create( callback, data );
   iterator.iterate( rootnode );
   iterator.free;
 end;

initialization
  {$ifdef dbgBehavior}
  writeln( 'Behavior debugging enabled' );
  {$endif}
end.

