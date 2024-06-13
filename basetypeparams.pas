unit basetypeparams;

{ base class for typeparameter classes }

interface

uses
   Classes, SysUtils,
   CastleVectors,
   CastleScene,
   Collect;

type

   TBaseTypeParams = class
      { type name }
      TypeName  : String;
      { number of animals of this type }
      TypeCount : integer;

      PovTranslation : TVector3;
      PovRotation    : TVector4;

      { model information }
      ModelUrl       : string;
      ModelTemplate  : TCastleScene;
      ModelTranslate : TVector3;
      ModelScale     : single;
      ModelRotate    : single; { around y axis to rotate model to standard direction }
      FullDetailDist : single; { distance witin which you can see full detail model }

      LowDefColor   : TVector3;
      LowDefTopUrl  : String;
      LowDefSideUrl : String;
      LowDefFrontUrl: String;
      LowDefBackUrl : String;

      constructor create( iName : string;
                          iURL  : string =  '';
                          scale : single = 1.0;
                          rotate: single = 0.0);
    end;

   TBaseTypeList = class( TSortedCollection )
      function findBaseType( typename : string ) : TBaseTypeParams;
      procedure setModelTemplate( typename : string; Template : TCastleScene );

      function compare(item1, item2 : pointer) : integer; override;
      function keyof( item : pointer ) : pointer; override;
    end;


implementation

constructor TBaseTypeParams.create( iName : string;
                            iURL  : string =  '';
                            scale : single = 1.0;
                            rotate: single = 0.0);
 begin
   inherited create;
   TypeName := iName;
   PovTranslation := Vector3( 0, 0, 0 );
   PovRotation    := Vector4( 0, 0, 0, 0 );
   ModelUrl := iURL;
   ModelRotate := rotate;
   ModelScale  := scale;
   ModelTranslate := Vector3( 0, 0, 0 );
   TypeCount := 0;
   ModelTemplate := nil;
   FullDetailDist := 40;
   LowDefColor := Vector3( 0.5, 0.5, 0.5 );
 end;

function TBaseTypeList.compare(item1, item2 : pointer) : integer;
 begin
   result := comparetext( pchar( item1 ), pchar( item2 ));
 end;

function TBaseTypeList.keyof( item : pointer ) : pointer;
 begin
   Result := PChar(TBaseTypeParams( item ).TypeName);
 end;

function TBaseTypeList.findBaseType( typename : string ) : TBaseTypeParams;
 var i : integer;
 begin
   Result := nil;
   if search( pchar(typename), i ) then
      Result := TBaseTypeParams( At( i ))
 end;

procedure TBaseTypeList.setModelTemplate( typename : string; Template : TCastleScene );
 var params : TBaseTypeParams;
 begin
   params := findBaseType( typename );
   if assigned( Params ) then
      Params.ModelTemplate := Template;
 end;


end.

