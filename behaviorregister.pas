unit BehaviorRegister;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BehaviorTree,
  Collect;

type TBehaviorLeafClass = class of TBehaviorLeaf;
     TBehaviorDecoratorClass = class of TBehaviorDecorator;
     TBehaviorNodeClass = class of TBehaviorNode;

procedure RegisterBehaviorLeaf( iBehaviorLeaf : TBehaviorLeafClass );
function BehaviorLeafRegisterCount : integer;
function BehaviorLeafRegisterClassAt( i : integer ) : string;

procedure RegisterBehaviorDecorator( iBehaviorDecorator : TBehaviorDecoratorClass );
function BehaviorDecoratorRegisterCount : integer;
function BehaviorDecoratorRegisterClassAt( i : integer ) : string;

implementation

type tbehaviorlist = class( TSortedCollection )
        function compare( key1, key2 : pointer ) : integer; override;
      end;

function TBehaviorList.compare( key1, key2 : pointer ) : integer;
 var desc1, desc2 : string;
 begin
   desc1 := TBehaviorNodeClass( key1 ).BehaviorClass;
   desc2 := TBehaviorNodeClass( key2 ).BehaviorClass;
   result := comparetext( desc1, desc2 );
 end;

const BehaviorLeafList      : TBehaviorList = nil;
      BehaviorDecoratorList : TBehaviorList = nil;

procedure RegisterBehaviorLeaf( iBehaviorLeaf : TBehaviorLeafClass );
 begin
   BehaviorLeafList.Insert( iBehaviorLeaf );
 end;

function BehaviorLeafRegisterCount : integer;
 begin
   result := BehaviorLeafList.count;
 end;

function BehaviorLeafRegisterClassAt( i : integer ) : string;
 begin
   assert( i < BehaviorLeafList.count );
   result := TBehaviorLeafClass( BehaviorLeafList.at( i )).behaviorclass;
 end;


procedure RegisterBehaviorDecorator( iBehaviorDecorator : TBehaviorDecoratorClass );
 begin
   BehaviorDecoratorList.Insert( iBehaviorDecorator );
 end;

function BehaviorDecoratorRegisterCount : integer;
 begin
   result := BehaviorDecoratorList.count;
 end;

function BehaviorDecoratorRegisterClassAt( i : integer ) : string;
begin
  assert( i < BehaviorDecoratorList.count );
  result := TBehaviorDecoratorClass( BehaviorDecoratorList.at( i )).behaviorclass;
end;


initialization
  BehaviorLeafList := TBehaviorList.create;
  BehaviorLeafList.ownitems := false;
  RegisterBehaviorLeaf( TBehaviorSucceed );
  RegisterBehaviorLeaf( TBehaviorFail );
  BehaviorDecoratorList := TBehaviorList.create;
  BehaviorDecoratorList.ownitems := false;
  RegisterBehaviorDecorator( TBehavior_ForceSuccess );
  RegisterBehaviorDecorator( TBehavior_ForceFail );
  RegisterBehaviorDecorator( TBehavior_Inverter );
finalization
  BehaviorLeafList.Free;
  BehaviorDecoratorList.Free;
end.

