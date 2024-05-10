unit BehaviorTree;

{$mode ObjFPC}{$H+}

{ Base Behavior Tree Classes
  rudimentary level implementation }

{ based on ideas from primarily the first of these links }
{ https://www.gamedeveloper.com/programming/behavior-trees-for-ai-how-they-work#close-modal }
{ https://robohub.org/introduction-to-behavior-trees/ }

{ Erik Donovan Johnson erikquiet@edj.net
  Free to use and modify in any way you want.
  Example code. No warranty.  Use at your own risk. }

{ TBehaviorRunner keeps track of active node so doesn't have to traverse to find it.
  Designed so a behavior tree can be shared by many objects without taking more
  data than the TBehaviorRunner and its data stack per object. }

interface

{ use compiler directive dbgbehavior to turn on debugging }
{$define dbgbehavior}

uses Classes, SysUtils,
     basedata,
     windows ; { for debug output }



{ behavior status values }
const behavior_notrun  = 0; { not used }
      behavior_running = 1;
      behavior_success = 2;
      behavior_fail    = 3;

type TBehaviorStatus = integer;

     TBehaviorNode = class; { forward }

     { manages the running, data and active state of a behavior tree }
     TBehaviorRunner = class; { forward }

     { root behavior tree class that everything derives from }
     TBehaviorNode = class

       name : string;
       function Run( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; virtual;
       function description : string; dynamic;

     end;

    { single child behavior node that can modify the results of that child in subclasses }
    TBehaviorDecorator = class( TBehaviorNode )
       child : TBehaviorNode;
       constructor create( ichild : TBehaviorNode );
     end;

    { multichild behavior node.  how those children are processed depends on subclasses }
    TBehaviorComposite = class( TBehaviorNode )
       children : array of TBehaviorNode;
       constructor create;
       constructor create2( Child0, Child1 : TBehaviorNode );
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
       function Run( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; override;
     end;
    {regardless if child is done running returns success regardless of childsuccess }
    TBehavior_ForceFail  = class( TBehaviorDecorator )
       function Run( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; override;
     end;
    {inverts child results if not running }
    TBehavior_Inverter  = class( TBehaviorDecorator )
       function Run( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; override;
     end;

   { loop decorator }



{ composite nodes }

    { Sequence : visit each child in order, starting with the first, and when that
      succeeds will call the second, and so on down the list of children.
      If any child fails it will immediately return failure to the parent. If the
      last child in the sequence succeeds, then the sequence will return success to
      its parent. }

    TBehaviorSequence = class( TBehaviorComposite )
       function Run( runner : TBehaviorRunner;
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
       function Run( runner : TBehaviorRunner;
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

function TBehaviorNode.description : string;
 begin
   result := name;
 end;

function TBehaviorNode.Run( runner : TBehaviorRunner;
                            secondspassed : single ) : TBehaviorStatus;
 begin
  {$ifdef dbgBehavior}
  outputdebugstring( pchar( description ));
  {$endif}
 end;

constructor TBehaviorDecorator.create( ichild : TBehaviorNode );
 begin
   assert( assigned( ichild ));
   child := ichild;
 end;

//--------------------------------

constructor TBehaviorComposite.create;
 begin
   inherited;
 end;

constructor TBehaviorComposite.create2( Child0, Child1 : TBehaviorNode );
 begin
   inherited create;
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
   outputdebugstring( pchar( ' child'+inttostr( childix )));
   {$endif}

   assert( childix < childcount );
   child := children[childix];
   runner.stackactivenode( child );
   result := child.Run( runner, secondspassed );
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

function TBehaviorSequence.processchildstatus( runner : TBehaviorRunner;
                                               childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := childstatus;
   case childstatus of
      behavior_success : begin
                          if incchildindex( runner ) then
                             result := behavior_running;
                          { else finished with success }
                         end;
      behavior_fail : begin
                        runner.datastack.popint; { finished with fail }
                      end;
      behavior_running : begin
                        { running will continue waiting for child, success will halt iteration with success }
                         end;
    end;
 end;

function TBehaviorSequence.run( runner : TBehaviorRunner;
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
   result := inherited;
   if result = '' then
      result := 'Sequence';
 end;

//--------------------------------

function TBehaviorSelector.processchildstatus( runner : TBehaviorRunner;
                                               childstatus : TBehaviorStatus ) : TBehaviorStatus;
 begin
   result := childstatus;
   case childstatus of
      behavior_success : runner.datastack.popint; { finished with success }
      behavior_fail : begin
                          if incchildindex( runner ) then
                             result := behavior_running;
                          { else finished with fail }
                      end;
      behavior_running : begin
                        { running will continue waiting for child, success will halt iteration with success }
                         end;
    end;
 end;

function TBehaviorSelector.run( runner : TBehaviorRunner;
                                secondspassed : single ) : TBehaviorStatus;
{ will run one child per tick, finished with first child that is a success }
 var childcount : integer;
     childix : integer;
 begin
   inherited; { debug output if needed }
   childcount := length( children );
   runner.datastack.peekint( childix ); { peek for childix on stack, will be 0 if none }
   if childix < childcount then
    begin
      result := RunNextChild( runner, secondspassed );
      result := processChildStatus( runner, result );
    end
   else
      result := behavior_fail;
   runner.UpdateActiveRunStatus( result );
 end;

function TBehaviorSelector.description : string;
 begin
   result := inherited;
   if result = '' then
      result := 'Selector';
 end;

//------------------------------------
{ decorators }

function TBehavior_ForceSuccess.Run( runner : TBehaviorRunner;
                                     secondspassed : single ) : TBehaviorStatus;
 { regardless if child is done running returns success regardless of childsuccess }
 begin
   inherited; { debug output if needed }
   assert( assigned( child ));
   runner.stackactivenode( child );
   result := child.run( runner, secondspassed );
   if result <> behavior_running then
      result := behavior_success;
   runner.UpdateActiveRunStatus( result );
 end;

function TBehavior_ForceFail.Run( runner : TBehaviorRunner;
                                  secondspassed : single ) : TBehaviorStatus;
 { regardless if child is done running returns success regardless of childsuccess }
 begin
   inherited; { debug output if needed }
   assert( assigned( child ));
   runner.stackactivenode( child );
   result := child.run( runner, secondspassed );
   if result <> behavior_running then
      result := behavior_fail;
   runner.UpdateActiveRunStatus( result );
 end;

function TBehavior_Inverter.run( runner : TBehaviorRunner;
                                 secondspassed : single ) : TBehaviorStatus;
 { inverts child results if not running }
 begin
   inherited; { debug output if needed }
   runner.stackactivenode( child );
   runner.activenode := child;
   result := child.run( runner, secondspassed );
   case result of
      behavior_success : result := behavior_fail;
      behavior_fail    : result := behavior_success;
    end;
   runner.UpdateActiveRunStatus( result );
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
   outputdebugstring( '>-RunTick--------');
   {$endif }
   if not assigned( activenode ) then
      activenode := rootnode;
   result := activenode.run( self, secondspassed ); { active node will change to the deepest running child }

   {!!! how to properly manage the stack when completing the run of children in a different tick???}
   if assigned( activenode ) then
    begin
      if activenode is TBehaviorComposite then
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
      assert( datastack.pop = nil );
//     repeat until DataStack.Pop = nil;   { clear stack though it should already be clear }
      activenode := nil;
    end;
 end;

procedure TBehaviorRunner.UpdateActiveRunStatus( istatus : TBehaviorStatus );
 var dum1 : integer;
     dum2 : single;
 begin
   {$ifdef dbgBehavior}
   case istatus of
      behavior_success : outputdebugstring( pchar('  success '+activenode.description ));
      behavior_fail    : outputdebugstring( pchar('  fail '+activenode.description ));
      behavior_running : outputdebugstring( pchar('  running '+activenode.description ));
    end;
   {$endif}
   if istatus <> behavior_running then
    begin
      assert( not DataStack.peekint( dum1 ));
      assert( not DataStack.peeksingle( dum2 ));
      {!!! how can this be invalid typecast after the above assertions?}
      activenode := TBehaviorNode( DataStack.pop ); { set active node to prior node in stack after success or fail }
    end;
 end;


end.

