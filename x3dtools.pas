unit x3dtools;

interface

uses
  {$ifndef FPC}System.types,{$endif}
  Classes, SysUtils, Math,
  CastleVectors, CastleUtils,
  CastleColors,
  CastleRenderOptions,
  x3dnodes, BaseTools;

function buildcolorline( count : integer;
                         color : TCastleColorRGB;
                         var Lines : TCoordinateNode;
                         LineWidth : single = 1;
                         LineType  : TLineType = ltSolid ) : TX3dRootNode;

function makePhysicalMaterial( color : TVector3 ) : TPhysicalMaterialNode;
function makeUnlitMaterial( color : TVector3 ) : TUnlitMaterialNode;

procedure addtexture( Shape : TShapeNode;
                         turl  : string );

type TTriSetWrapper = class
         constructor create( iCapacity : dword = 4 );
         destructor destroy; override;
         procedure build( texture : string = '' ); virtual;
         function initializeNode : TAbstractComposedGeometryNode; virtual;

         procedure addpt( const pt : TVector3;
                          const texcoord : TVector2 );
         procedure addsquare2( const pL0, pR0, pL1, pR1 : TVector3;
                               t0, t1 : single );
         procedure addsquare4( const pL0, pR0, pL1, pR1 : TVector3;
                               t0, t1 : single;
                               terrainheight : THeightAboveTerrainEvent;
                               h : single = 0 );
         public

         TrianglesNode : TAbstractComposedGeometryNode;
         Points        : TVector3List;
         TexCoords     : TVector2List;
         CoordNode     : TCoordinateNode;
         TexCoordNode  : TTextureCoordinateNode;
         Shape         : TShapeNode;
         Delta         : TVector2;
       end;

     TTriFanWrapper = class( TTriSetWrapper )
        procedure build( texture : string = '' ); override;
        function initializeNode : TAbstractComposedGeometryNode; override;
      end;

     TIndexedTriSetWrapper = class( TTriSetWrapper )
       function initializeNode : TAbstractComposedGeometryNode; override;
      end;

  tmaterialbuilder = class
    public
    BaseColor : TVector3;
    TextureUrl : string;
    function buildmaterial : TPhysicalMaterialNode;
  end;

  tshapebuilder = class( tmaterialbuilder )
     public
     Vertices : TVector3List;
     Indexes  : TInt32List;
     constructor create;
     destructor destroy; override;
   end;

  tstripbuilder = class( tshapebuilder )
     function buildshape( const p1, p2 : TVector2;
                          terrainheight : THeightAboveTerrainEvent;
                          h : single = 0;
                          w : single = 1;
                          stepdist : single = 1 ) : TShapeNode;
   end;

implementation //===============================================================

function makePhysicalMaterial( color : TVector3 ) : TPhysicalMaterialNode;
 begin
   Result := TPhysicalMaterialNode.create;
   result.BaseColor := color;
 end;

function makeUnlitMaterial( color : TVector3 ) : TUnlitMaterialNode;
 begin
   Result := TUnlitMaterialNode.Create;
   Result.EmissiveColor := color;
 end;

procedure addtexture( Shape : TShapeNode;
                      turl  : string );
 var Texture : TImageTextureNode;
 begin
   Shape.Appearance := TAppearanceNode.Create;
   Texture := TImageTextureNode.Create;
   Texture.SetUrl([turl]);
   Shape.Appearance.Texture := Texture;
   Shape.Appearance.Material := makePhysicalMaterial( Vector3( 1, 1, 1 ));
 end;

function buildcolorline( count : integer;
                         color : TCastleColorRGB;
                         var Lines : TCoordinateNode;
                         LineWidth : single = 1;
                         LineType  : TLineType = ltSolid ) : TX3dRootNode;
 var Material: TUnlitMaterialNode;
     Appearance: TAppearanceNode;
     Shape     : TShapeNode;
     LineSet : TLineSetNode;
     LineProperties : TLinePropertiesNode;
 begin
   result := TX3dRootNode.create;
   Material := makeUnlitMaterial( color );
   Material.Transparency := 0.5;

   LineProperties := TLinePropertiesNode.Create;
   LineProperties.LinewidthScaleFactor := LineWidth;
   LineProperties.LineType := LineType;

   Appearance := TAppearanceNode.Create;
   Appearance.Material := Material;
   Appearance.LineProperties := LineProperties;

   LineSet := TLineSetNode.CreateWithShape(Shape);
   LineSet.mode := lmStrip;
   LineSet.SetVertexCount(count);

   Shape.Appearance := Appearance;
   Result.AddChildren( Shape );

   Lines := TCoordinateNode.Create;
   Lines.FdPoint.Items.Count := count;
   LineSet.Coord := Lines;
 end;

//-------------------------------------

constructor TTriSetWrapper.create( iCapacity : dword = 4 );
 begin
   TrianglesNode := initializeNode;
   TrianglesNode.Solid := false;
   CoordNode := TCoordinateNode.Create;
   Points := TVector3List.Create;
   Points.Capacity := iCapacity;
   TexCoordNode := TTextureCoordinateNode.Create;
   TexCoords := TVector2List.Create;
   TexCoords.Capacity := iCapacity;
 end;

destructor TTriSetWrapper.destroy;
 begin
   inherited;
   Points.Free;
   TexCoords.Free;
 end;

function TTriSetWrapper.initializeNode : TAbstractComposedGeometryNode;
 begin
   Result := TTriangleSetNode.CreateWithShape(Shape);
 end;

procedure TTriSetWrapper.build( texture : string = '' );
 begin
   CoordNode.SetPoint(Points);
   TrianglesNode.Coord := CoordNode;
   TexCoordNode.SetPoint(TexCoords);
   TrianglesNode.TexCoord := TexCoordNode;
   if texture <> '' then
      AddTexture( Shape, texture );
 end;

procedure TTriSetWrapper.addpt( const pt : TVector3;
                                const texcoord : TVector2 );
 begin
   Points.Add( pt );
   TexCoords.Add( texcoord );
 end;

procedure TTriSetWrapper.addsquare2( const pL0, pR0, pL1, pR1 : TVector3;
                                       t0, t1 : single );
 { add the 4 point square as two cw triangles }
 begin
   addpt( pR0, Vector2(t0, 0));
   addpt( pL0, Vector2(t0, 1));
   addpt( pR1, Vector2(t1, 0));

   addpt( pR1, Vector2(t1, 0));
   addpt( pL0, Vector2(t0, 1));
   addpt( pL1, Vector2(t1, 1));
 end;

procedure TTriSetWrapper.addsquare4( const pL0, pR0, pL1, pR1 : TVector3;
                                       t0, t1 : single;
                                       terrainheight : THeightAboveTerrainEvent;
                                       h : single = 0 );
 { add the 4 point square as four cw triangles }
 var pC : TVector3;
     tC : single;
 begin
   pC := Vector3( ( pL0.X + pR0.X + pL1.x + pR1.x ) * 0.25, 0,
                  ( pL0.Z + pR0.Z + pL1.Z + pR1.Z ) * 0.25 );
   terrainheight( pC, pC.Y );
   pC.Y := pC.Y + h;
   tC := ( t0 + t1 )*0.5;

   addpt( pR0, Vector2(t0, 0));
   addpt( pL0, Vector2(t0, 1));
   addpt( pC, Vector2(tc, 0.5));

   addpt( pR0, Vector2(t0, 0));
   addpt( pC, Vector2(tc, 0.5));
   addpt( pR1, Vector2(t1, 0));

   addpt( pL1, Vector2(t1, 1));
   addpt( pC, Vector2(tc, 0.5));
   addpt( pL0, Vector2(t0, 1));

   addpt( pR1, Vector2(t1, 0));
   addpt( pC, Vector2(tc, 0.5));
   addpt( pL1, Vector2(t1, 1));
 end;

//------------------------------------

function TTriFanWrapper.initializeNode : TAbstractComposedGeometryNode;
 begin
   Result := TTriangleFanSetNode.CreateWithShape(Shape);
 end;

procedure TTriFanWrapper.build( texture : string = '' );
 var c : cardinal;
 begin
   inherited build( texture );
   c := Points.count;
   TTriangleFanSetNode(TrianglesNode).SetFanCount([c]);
 end;

//------------------------------------

function TIndexedTriSetWrapper.initializeNode : TAbstractComposedGeometryNode;
 begin
   Result := TIndexedTriangleFanSetNode.CreateWithShape(Shape);
 end;

//-------------------------------------

function tmaterialbuilder.buildmaterial : TPhysicalMaterialNode;
 var Texture : TImageTextureNode;
 begin
   Result := TPhysicalMaterialNode.Create;
   Result.Roughness := 1;
   Result.BaseColor := BaseColor;
   if TextureUrl <> '' then
    begin
      Texture := TImageTextureNode.Create;
      Texture.SetUrl([TextureURL]);
      Result.BaseTexture := Texture;
    end;
 end;

constructor tshapebuilder.create;
 begin
   Vertices := TVector3List.Create;
   Indexes  := TInt32List.Create;
 end;

destructor tshapebuilder.destroy;
 begin
   Vertices.Free;
   Indexes.Free;
 end;

//-------------------------------------------

function tstripbuilder.buildshape( const p1, p2 : TVector2;
                                   terrainheight : THeightAboveTerrainEvent;
                                   h : single = 0; { height above ground }
                                   w : single = 1;
                                   stepdist : single = 1 ) : TShapeNode;
var Triangles : TIndexedTriangleSetNode;
    Appearance: TAppearanceNode;
    CoordinateNode : TCoordinateNode;
    dist : single;
    delta, delta2 : TVector2;
    i, steps : integer;
    p : TVector2;
    pL, pR : TVector2;
    vL, vR : TVector3;
begin
  Result:= TShapeNode.Create;
  delta := p2 - p1;
  dist := hypot( delta.x, delta.y );
  steps := trunc( dist / stepdist ) + 1;
  delta := delta / steps;
  p := p1;

  Indexes.capacity := ( steps + 1 ) * 6;
  Vertices.Capacity := ( steps + 1 ) * 2;

  { calculate left and right vertex from p }
  w := w * 0.5;
  delta2 := vector2( delta.y, delta.x ).Normalize * w;
  pL := p - delta2;
  pR := p + delta2;

  {! get y elevation for v1, vr }
  vL := vector3( pL.X, 0, pL.Y );
  vR := vector3( pR.X, 0, pR.Y );
  terrainheight( vL, vL.y );
  terrainheight( vR, vR.y );
  Vertices.Add( vL );
  Vertices.Add( vR );
  pl := pl + delta;
  pr := pr + delta;
  for i := 0 to steps - 1 do
   begin
     { get y elevation for v1, vr }
     vL := vector3( pL.X, 0, pL.Y );
     vR := vector3( pR.X, 0, pR.Y );
     terrainheight( vl, vL.y );
     terrainheight( vr, vR.y );
     Vertices.Add( vL );
     Vertices.Add( vR );
     pL := pL + delta;
     pR := pR + delta;
   end;
  Triangles := TIndexedTriangleSetNode.Create;
  Triangles.SetIndex( Indexes );
  CoordinateNode := TCoordinateNode.Create;
  CoordinateNode.SetPoint( Vertices );
  Triangles.Coord := CoordinateNode;

  Result.Geometry := Triangles;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := buildmaterial;

  Result.Appearance := Appearance;
 end;

end.

