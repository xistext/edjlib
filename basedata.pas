unit basedata;

{ Erik Donovan Johnson erikquiet@gmail.com
  Free to use and modify in any way you want.
  Example code. No warranty.  Use at your own risk. }

{ data stack used by Behavior Tree.  could be used for
  any stack, and fleshed out with more classes }

interface

uses
  Classes, SysUtils;

type

{ data class stores integer or single as implemented in subclasses }
TBehaviorData = class
 end;

TBehaviorInt = class( TBehaviorData )
   constructor create( ivalue : integer );
   protected
   value : integer;
 end;

TBehaviorSingle = class( TBehaviorData )
   constructor create( ivalue : single );
   protected
   value : single;
 end;

TBehaviorDataStack = class { for storing data }
    destructor destroy; override;
    procedure Push(item : tobject);
    function Pop : tobject;
    function IsEmpty : boolean;

    procedure pushint( i : integer );
    function popint : integer;
    function peekint( var i : integer ) : boolean;  { look at what would have popped without popping }

    procedure pushsingle( i : single );
    function popsingle : single;
    function peeksingle( var s: single ) : boolean; { look at what would have popped without popping }

    function peekitem( var it : tobject ) : boolean;

    private
    stack : array of tobject;
  end;

implementation

constructor TBehaviorInt.create( ivalue : integer );
 begin
   value := ivalue;
 end;

constructor TBehaviorSingle.create( ivalue : single );
 begin
   value := iValue;
 end;

//-------------------------------

destructor TBehaviorDataStack.destroy;
 var i : integer;
     item : TObject;
 begin
   for i := 0 to length( stack ) - 1 do
    begin
      item := stack[i];
      if Item is TBehaviorData then
         Item.Free;
    end;
 end;


procedure TBehaviorDataStack.Push(item : tobject);
 var l : integer;
 begin
   l := length( stack );
   setlength(stack, l + 1 );
   stack[l] := item;
 end;

function TBehaviorDataStack.Pop : tobject;
 var l : integer;
 begin
   result := nil;
   l := length( stack );
   if l > 0 then
    begin
      dec( l );
      result := stack[l];
      setlength( stack, l );
    end;
 end;

function TBehaviorDataStack.IsEmpty : boolean;
 begin
   result := length( stack ) = 0;
 end;

procedure TBehaviorDataStack.pushint( i : integer );
 begin
   push( TBehaviorInt.create( i ));
 end;

function TBehaviorDataStack.popint : integer;
 var item : tobject;
 begin
   result := 0;
   item := tobject( pop );
   assert( assigned( item ));
   assert( item is TBehaviorInt ); { enforced types to be safer }
   result := TBehaviorInt( item ).value;
   item.free;
 end;

function TBehaviorDataStack.peekint( var i : integer ) : boolean;  { look at what would have popped without popping }
 var l : integer;
     item : tobject;
 begin
    i := 0;
    l := length( stack );
    result := l > 0;
    if result then
     begin
       dec( l );
       item := stack[l];
       result := item is TBehaviorInt;
       if result then
          i := TBehaviorInt( item ).value;
     end;
 end;

procedure TBehaviorDataStack.pushsingle( i : single );
 begin
   push( TBehaviorSingle.create( i ));
 end;

function TBehaviorDataStack.popsingle : single;
var item : tobject;
begin
  result := 0;
  item := tobject( pop );
  assert( assigned( item ));
  assert( item is TBehaviorSingle ); { enforced types to be safer }
  result := TBehaviorSingle( item ).value;
  item.free;
end;

function TBehaviorDataStack.peeksingle( var s : single ) : boolean;  { look at what would have popped without popping }
 var l : integer;
     item : tobject;
 begin
    s := 0;
    l := length( stack );
    result := l > 0;
    if result then
     begin
       dec( l );
       item := stack[l];
       result := item is TBehaviorSingle;
       if result then
          s := TBehaviorSingle( item ).value;
     end;
 end;

function TBehaviorDataStack.peekitem( var it : tobject ) : boolean;
 var l : integer;
     item : tobject;
 begin
    it := nil;
    l := length( stack );
    result := l > 0;
    if result then
     begin
       dec( l );
       item := stack[l];
       result := not ( it is TBehaviorData );
       if result then
          it := item;
     end;
 end;

end.

