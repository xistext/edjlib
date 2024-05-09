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

uses Classes,
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

     { root behavior tree class that everything derives from }
     TBehaviorNode = class

       function Run( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; virtual; abstract;

     end;

    { single child behavior node that can modify the results of that child in subclasses }
    TBehaviorDecorator = class( TBehaviorNode )
       child : TBehaviorNode;
       function Run( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; override;
     end;

    { multichild behavior node.  how those children are processed depends on subclasses }
    TBehaviorComposite = class( TBehaviorNode )
       children : array of TBehaviorNode;
       constructor create;
       destructor destroy; override;
       procedure add( item : TBehaviorNode );
       function RunNextChild( runner : TBehaviorRunner;
                              secondspassed : single ) : TBehaviorStatus;
     end;

    { sublass this to make checks and actions in overloaded Run methods }
    TBehaviorLeaf = class( TBehaviorNode )

     end;

{ composite nodes }

    { Sequence : visit each child in order, starting with the first, and when that
      succeeds will call the second, and so on down the list of children.
      If any child fails it will immediately return failure to the parent. If the
      last child in the sequence succeeds, then the sequence will return success to
      its parent. }

    TBehaviorSequence = class( TBehaviorComposite )
       function Run( runner : TBehaviorRunner;
                     secondspassed : single ) : TBehaviorStatus; override;
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
     end;

{ keep track of active behavior node and other info (in subclass) }
TBehaviorRunner = class
   RootNode   : TBehaviorNode;
   ActiveNode : TBehaviorNode;
   DataStack  : TBehaviorDataStack;
   constructor create( iRootNode : TBehaviorNode );
   destructor destroy; override;
   function RunTick( secondspassed : single ) : TBehaviorStatus;
   { all Run calls must call this when finished with a current status to update the active node from the stack as needed }
   procedure UpdateActiveRunStatus( istatus : TBehaviorStatus );
 end;

implementation  //==============================================================

function TBehaviorDecorator.run( runner : TBehaviorRunner;
                                 secondspassed : single ) : TBehaviorStatus;
 begin
   if assigned( child ) then
    begin
      runner.datastack.push( self ); { push this node as parent for the child, it will pop when done }
      runner.activenode := child;
      result := child.run( runner, secondspassed );
    end
   else
      result := behavior_fail;
   runner.UpdateActiveRunStatus( result );
 end;

//--------------------------------

constructor TBehaviorComposite.create;
 begin
   inherited;
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
   childix := runner.datastack.peekint; { peek for childix on stack }
   if childix < childcount then
    begin
      if childix = 0 then
         runner.datastack.pushint( 0 ); { push 0 since it wasn't actually stored }
      runner.datastack.push( self ); { push this node as parent for the child, it will pop when done }
      child := children[childix];
      runner.activenode := child;
      result := child.Run( runner, secondspassed );
    end
   else
    begin
      assert( false ); { this should never happen }
      result := behavior_fail;
    end;
 end;

//--------------------------------

function TBehaviorSequence.run( runner : TBehaviorRunner;
                                secondspassed : single ) : TBehaviorStatus;
 { will run one child per tick, finished with first child that is a fail }
 var childcount : integer;
     childix : integer;
 begin
   runner.activenode := self;
   childcount := length( children );
   childix := runner.datastack.peekint; { peek for childix on stack }
   if childix < childcount then
    begin
      result := RunNextChild( runner, secondspassed );
      { running will continue waiting for child, fail will halt iteration with fail }
      case result of
       behavior_success :
         begin
           childix := runner.datastack.popint;
           inc( childix );
           if childix < childcount then
            begin
              runner.datastack.pushint(childix); { add child ix back to stack }
              result := behavior_running; { keep looping until fail or finished }
            end;
         end;
       behavior_fail : runner.datastack.popint;
       behavior_running :;
      end;
    end
   else
      result := behavior_fail;
   runner.UpdateActiveRunStatus( result );
 end;

//--------------------------------

function TBehaviorSelector.run( runner : TBehaviorRunner;
                                secondspassed : single ) : TBehaviorStatus;
{ will run one child per tick, finished with first child that is a success }
 var childcount : integer;
     childix : integer;
 begin
   runner.activenode := self;
   childcount := length( children );
   childix := runner.datastack.peekint; { peek for childix on stack }
   if childix < childcount then
    begin
      result := RunNextChild( runner, secondspassed );
      { running will continue waiting for child, success will halt iteration with success }
      case result of
       behavior_fail :
         begin
           childix := runner.datastack.popint;
           inc( childix );
           if childix < childcount then
            begin
              runner.datastack.pushint(childix); { add child ix back to stack }
              result := behavior_running; { keep looping until success or finished }
            end;
         end;
       behavior_success : runner.datastack.popint;
       behavior_running :;
      end;
    end
   else
      result := behavior_fail;
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

function TBehaviorRunner.RunTick( secondspassed : single ) : TBehaviorStatus;
 begin
   if not assigned( activenode ) then
      activenode := rootnode;
   result := activenode.run( self, secondspassed ); { active node will change tthe the deepest running child }

   if result <> behavior_running then
    begin
      repeat until DataStack.Pop = nil; { clear stack even though it should already be clear }
      activenode := nil;
    end;
 end;

procedure TBehaviorRunner.UpdateActiveRunStatus( istatus : TBehaviorStatus );
 begin
   if istatus <> behavior_running then
    begin
      assert( DataStack.peekint = 0 );
      assert( DataStack.peeksingle = 0 );
      activenode := TBehaviorNode( DataStack.pop ); { set active node to prior node in stack after success or fail }
    end;
 end;

end.

