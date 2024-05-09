unit basedata;

{$mode ObjFPC}{$H+}

{ Erik Donovan Johnson erikquiet@edj.net
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
   private
   procedure setinteger( ivalue : integer ); virtual;
   function getinteger : integer; virtual;
   procedure setsingle( ivalue : single ); virtual;
   function getsingle : single; virtual;
   public
   property asinteger : integer read getinteger write setinteger;
   property assingle  : single read getsingle write setsingle;
 end;

TBehaviorInt = class( TBehaviorData )
   constructor create( ivalue : integer );
   private
   value : integer;
   procedure setinteger( ivalue : integer ); override;
   function getinteger : integer; override;
 end;

TBehaviorSingle = class( TBehaviorData )
   constructor create( ivalue : single );
   private
   value : single;
   procedure setsingle( ivalue : single ); override;
   function getsingle : single; override;
 end;

TBehaviorDataStack = class { for storing data }
    destructor destroy; override;
    procedure Push(item : tobject);
    function Pop : tobject;
    function IsEmpty : boolean;

    procedure pushint( i : integer );
    function popint : integer;
    function peekint : integer;  { look at what would have popped without popping }

    procedure pushsingle( i : single );
    function popsingle : single;
    function peeksingle : single;  { look at what would have popped without popping }

    private
    stack : array of tobject;
  end;

implementation

procedure TBehaviorData.setinteger( ivalue : integer );
 begin
 end;

function TBehaviorData.getinteger : integer;
 begin
   result := 0;
 end;

procedure TBehaviorData.setsingle( ivalue : single );
 begin
 end;

function TBehaviorData.getsingle : single;
 begin
   result := 0;
 end;

constructor TBehaviorInt.create( ivalue : integer );
 begin
   value := ivalue;
 end;

procedure TBehaviorInt.setinteger( ivalue : integer );
 begin
   value := ivalue;
 end;

function TBehaviorInt.getinteger : integer;
 begin
   result := value;
 end;

constructor TBehaviorSingle.create( ivalue : single );
 begin
   value := iValue;
 end;

procedure TBehaviorSingle.setsingle( ivalue : single );
 begin
   value := ivalue;
 end;

function TBehaviorSingle.getsingle : single;
 begin
   result := value;
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
   result := length( stack ) > 0;
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
   if assigned( item ) then
    begin
      assert( item is TBehaviorInt ); { enforced types to be safer }
      result := TBehaviorData( item ).asinteger;
      item.free;
    end;
 end;

function TBehaviorDataStack.peekint : integer;  { look at what would have popped without popping }
 var l : integer;
     item : tobject;
 begin
    result := 0;
    l := length( stack );
    if l > 0 then
     begin
       dec( l );
       item := stack[l];
       if item is TBehaviorInt then
          result := TBehaviorData( item ).asinteger;
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
  if assigned( item ) then
   begin
     assert( item is TBehaviorSingle ); { enforced types to be safer }
     result := TBehaviorData( item ).assingle;
     item.free;
   end;
end;

function TBehaviorDataStack.peeksingle : single;  { look at what would have popped without popping }
 var l : integer;
     item : tobject;
 begin
    result := 0;
    l := length( stack );
    if l > 0 then
     begin
       dec( l );
       item := stack[l];
       if item is TBehaviorSingle then
          result := TBehaviorData( item ).assingle;
     end;
 end;


end.
