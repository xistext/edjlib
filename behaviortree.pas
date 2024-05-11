unit BehaviorTree;

{$mode ObjFPC}{$H+}

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
      {$ifdef dbgbehavior}
      tickcount : integer = 0;
      {$endif}

type TBehaviorStatus = integer;

     TBehaviorNode = class; { forward }

     { manages the running, data and active state of a behavior tree }
     TBehaviorRunner = class; { forward }

     { root behavior tree class that everything derives from }
     TBehaviorNode = class

       name : string;
       constructor create( iname : string = '' );
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; virtual;
       function description : string; dynamic;

     end;

    { single child behavior node that can modify the results of that child in subclasses }
    TBehaviorDecorator = class( TBehaviorNode )
       child : TBehaviorNode;
       constructor create( ichild : TBehaviorNode;
                           iname : string = '' );
     end;

    { multichild behavior node.  how those children are processed depends on subclasses }
    TBehaviorComposite = class( TBehaviorNode )
       children : array of TBehaviorNode;
       constructor create( iname : string = '' );
       constructor create2( Child0, Child1 : TBehaviorNode;
                            iname : string = '');
       destructor destroy; override;
       procedure add( item : TBehaviorNode );
       function RunNextChild( runner : TBehaviorRunner;
                              secondspassed : single ) : TBehaviorStatus;
       function incchildindex( runner : TBehaviorRunner ) : boolean;
       function processchildstatus( runner : TBehaviorRunner;
                                    childstatus : TBehaviorStatus ) : TBehaviorStatus; virtual; abstract;
     end;

    { sublass this to make checks and actions in overloaded Run methods }
    TBehaviorLeaf = class( TBehaviorNode )

     end;

{ decorators }

    {regardless if child is done running returns success regardless of childsuccess }
    TBehavior_ForceSuccess = class( TBehaviorDecorator )
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; override;
       function description : string; override;
     end;
    {regardless if child is done running returns success regardless of childsuccess }
    TBehavior_ForceFail  = class( TBehaviorDecorator )
       function Tick( runner : TBehaviorRunner;
                      secondspassed : single ) : TBehaviorStatus; override;
       function description : string; override;
     end;
    {inverts child results if not running }
    TBehavior_Inverter  = class( TBehaviorDecorator )
       function Tick( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; override;
       function description : string; override;
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
     end;

{ keep track of active behavior node and other info (in subclass) }
TBehaviorRunner = class
   RootNode   : TBehaviorNode;
   ActiveNode : TBehaviorNode;
   DataStack  : TBehaviorDataStack;
   constructor create( iRootNode : TBehaviorNode );
   destructor destroy; override;
   procedure StackActiveNode( NewActiveNode : TBehaviorNode );
   function RunTick( secondspassed : single ) : TBehaviorStatus;
   { all Run calls must call this when finished with a current status to update the active node from the stack as needed }
   procedure UpdateActiveRunStatus( istatus : TBehaviorStatus );
 end;

{ branchless ways to set statuses }
function SuccessOrFail( condition : boolean ) : TBehaviorStatus;
function SuccessOrRunning( condition : boolean ) : TBehaviorStatus;
function FailOrRunning( condition : boolean ) : TBehaviorStatus;

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
 begin
   result := ord( condition ) * behavior_fail + ord( not condition ) * behavior_running;
 end;

constructor TBehaviorNode.create( iname : string = '');
 begin
   name := iname;
 end;

function TBehaviorNode.description : string;
 begin
   result := name;
 end;

function TBehaviorNode.Tick( runner : TBehaviorRunner;
                             secondspassed : single ) : TBehaviorStatus;
 begin
  {$ifdef dbgBehavior}
  write( description + '(' );
  {$endif}
 end;

constructor TBehaviorDecorator.create( ichild : TBehaviorNode;
                                       iname : string = '' );
 begin
   inherited create( iname );
   assert( assigned( ichild ));
   child := ichild;
 end;

//--------------------------------

constructor TBehaviorComposite.create( iname : string = '' );
 begin
   inherited;
 end;

constructor TBehaviorComposite.create2( Child0, Child1 : TBehaviorNode;
                                        iname : string = '' );
 begin
   inherited create( iname );
   setlength( children, 2 );
   children[0] := Child0;
   children[1] := Child1;
 end;

destructor TBehaviorComposite.destroy;
 var i : integer;
 begin
   for i := 0 to length( children ) - 1 do
      children[i].free;
   setlength( children, 0 );
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
 var childcount, childix : integer;
     child : TBehaviorNode;
 begin
   childcount := length( children );
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
   if runner.datastack.peekint( childix ) then
      runner.datastack.popint;
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
                             ( @_sequencerunning, @_sequencesuccess, @_sequencefail );

function TBehaviorSequence.processchildstatus( runner : TBehaviorRunner;
                                               childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := sequenceprocs[childstatus]( self, runner );
 end;

function TBehaviorSequence.Tick( runner : TBehaviorRunner;
                                 secondspassed : single ) : TBehaviorStatus;
 { will run one child per tick, finished with first child that is a fail }
 var childcount : integer;
     childix : integer;
 begin
   inherited; { debug output if needed }
   childcount := length( children );
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
                              ( @_selectorrunning, @_selectorsuccess, @_selectorfail );

function TBehaviorSelector.processchildstatus( runner : TBehaviorRunner;
                                               childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := selectorprocs[childstatus]( self, runner );
 end;

function TBehaviorSelector.Tick( runner : TBehaviorRunner;
                                 secondspassed : single ) : TBehaviorStatus;
{ will run one child per tick, finished with first child that is a success }
 var childcount : integer;
     childix : integer;
 begin
   inherited; { debug output if needed }
   childcount := length( children );
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

//------------------------------------
{ decorators }

function TBehavior_ForceSuccess.Tick( runner : TBehaviorRunner;
                                      secondspassed : single ) : TBehaviorStatus;
 { regardless if child is done running returns success regardless of childsuccess }
 begin
   inherited; { debug output if needed }
   assert( assigned( child ));
   runner.stackactivenode( child );
   result := child.Tick( runner, secondspassed );
   result := successorrunning( result <> behavior_running );
   runner.UpdateActiveRunStatus( result );
 end;

function TBehavior_ForceSuccess.description : string;
 begin
   result := 'ForceSuccess'
 end;

function TBehavior_ForceFail.Tick( runner : TBehaviorRunner;
                                   secondspassed : single ) : TBehaviorStatus;
 { regardless if child is done running returns success regardless of childsuccess }
 begin
   inherited; { debug output if needed }
   assert( assigned( child ));
   runner.stackactivenode( child );
   result := child.Tick( runner, secondspassed );
   result := failorrunning( result <> behavior_running );
   runner.UpdateActiveRunStatus( result );
 end;

function TBehavior_ForceFail.description : string;
 begin
   result := 'ForceFail'
 end;

function TBehavior_Inverter.Tick( runner : TBehaviorRunner;
                                  secondspassed : single ) : TBehaviorStatus;
 { inverts child results if not running }
 begin
   inherited; { debug output if needed }
   runner.stackactivenode( child );
   runner.activenode := child;
   result := child.Tick( runner, secondspassed );
   case result of
      behavior_success : result := behavior_fail;
      behavior_fail    : result := behavior_success;
    end;
   runner.UpdateActiveRunStatus( result );
 end;

function TBehavior_Inverter.description : string;
 begin
   result := 'Invert'
 end;

//------------------------------------

constructor TBehaviorRunner.create( iRootNode : TBehaviorNode );
 begin
   { unowned references to externally owned nodes }
   RootNode   := iRootNode;
   ActiveNode := RootNode;
   { owned datastack of unowned references to externally owned nodes }
   DataStack := TBehaviorDataStack.create;
 end;

destructor TBehaviorRunner.destroy;
 begin
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
   {$ifdef dbgBehavior}
   write( ':Tick'+IntToStr( tickcount )+':');
   inc( tickcount );
   {$endif }
   if not assigned( activenode ) then  { start over from root }
      activenode := rootnode;
   result := activenode.Tick( self, secondspassed ); { active node will change to the deepest running child }

   { manage the stack when completing the run of children from a different tick }
   if assigned( activenode ) then
    begin
      while ( result <> behavior_running ) and ( activenode is TBehaviorComposite ) do
       begin
         { if already processed, will be 'running', so will do nothing, otherwise
           insures child results of composites are handled, even when run directly from here. }
         result := TBehaviorComposite( activenode ).processchildstatus( self, result );
         if result <> behavior_running then
            UpdateActiveRunStatus( result ); { unwind stack if activenode isn't still running }
       end;
    end
   else
    begin
      assert( datastack.IsEmpty );    { stack should be clear when finished }
      {$ifdef dbgBehavior}write('.');{$endif}
    end;
   {$ifdef dbgBehavior}writeln( '' );{$endif}
 end;

  {$ifdef dbgBehavior}
  const resultstrings : array[behavior_running..behavior_fail] of string =
                             ('):Running','):Success','):Fail' );
  {$endif}

procedure TBehaviorRunner.UpdateActiveRunStatus( istatus : TBehaviorStatus );
 var dum1 : integer;
     dum2 : single;
 begin
   {$ifdef dbgBehavior}
   write( resultstrings[istatus] );
   {$endif}
   if istatus <> behavior_running then
    begin
      assert( not DataStack.peekint( dum1 ));
      assert( not DataStack.peeksingle( dum2 ));
      {!!! how can this be invalid typecast after the above assertions?}
      if DataStack.IsEmpty then
         activenode := nil
      else
         activenode := TBehaviorNode( DataStack.pop ); { set active node to prior node in stack after success or fail }
    end;
 end;

initialization
  {$ifdef dbgBehavior}
  writeln( 'Behavior debugging enabled' );
  {$endif}
end.

