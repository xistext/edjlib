unit x3dtools;

interface

uses
  {$ifndef FPC}System.types,{$endif}
  Classes, SysUtils,
  CastleVectors, CastleUtils,
  CastleColors,
  CastleRenderOptions,
  x3dnodes, livetools;

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

end.

