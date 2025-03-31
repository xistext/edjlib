unit basemesh;

interface

uses
  Classes, SysUtils,
  CastleUtils, CastleVectors, CastleScene,
  x3dNodes;

type

TBaseMesh = class( TCastleScene )
   public

   fGridCount  : integer; { vertex count }
   fGridStep   : single;
   CoordinateNode : TCoordinateNode;

   constructor create( aowner : TComponent );
   destructor destroy; override;

   protected

   function getcellcount : integer;

   function inittriangles : TIndexedTriangleSetNode;

   public
   property CellCount : integer read getCellCount;
   procedure UpdateGraphics; virtual; abstract;
   procedure InitializeData; dynamic;
   procedure UpdateMeshProperties; virtual;

   function InitAppearance : TAppearanceNode; dynamic;
   function InitMaterial   : TPhysicalMaterialNode; dynamic;
   procedure InitVertices; dynamic; abstract;

   private
   function initindexes : TInt32List;
 end;

TTextureMesh = class( TBaseMesh )
   public

   destructor destroy; override;
   procedure InitializeData; override;

   function inittexture( TextureUrl : string ) : TImageTextureNode;

   procedure InitVertices; override;

   protected

   TexCoordNode   : TTextureCoordinateNode;

end;


implementation

constructor TBaseMesh.create( aowner : TComponent );
 begin
   inherited create( owner );
   fGridCount := 0;
   fGridStep := 0;
 end;

destructor TBaseMesh.destroy;
 begin
   inherited;
   CoordinateNode.Free;
 end;

function TBaseMesh.getcellcount : integer;
 begin
   result := fGridCount - 1;
 end;

procedure TBaseMesh.initializedata;
 begin
   CoordinateNode := TCoordinateNode.Create;
 end;

function TBaseMesh.initindexes : TInt32List;
 var X, Z : integer;
 begin
   Result := TInt32List.Create;
   for X:= 1 to fGridCount - 1 do for Z := 1 to fGridCount - 1 do with Result do
    begin
      { triangle1 }
      Add( fGridCount * Z + X );
      Add( fGridCount * ( Z - 1 ) + X );
      Add( fGridCount * ( Z - 1 ) + X - 1 );
      { triangle2 }
      Add( fGridCount * Z + X );
      Add( fGridCount * ( Z - 1 ) + X - 1 );
      Add( fGridCount * Z + X - 1 );
    end;
 end;

function TBaseMesh.inittriangles : TIndexedTriangleSetNode;
 var Indexes : TInt32List;
 begin
   Indexes := initindexes;
   Result := TIndexedTriangleSetNode.Create;
   Result.Coord := CoordinateNode;
   Result.SetIndex( Indexes );
   Indexes.Free;
 end;

procedure TBaseMesh.UpdateMeshProperties;
 begin
 end;

function TBaseMesh.InitAppearance : TAppearanceNode;
 begin
   result := TAppearanceNode.create;
     { make the material lit }
   result.Material := initMaterial;

 end;

function TBaseMesh.initmaterial : TPhysicalMaterialNode;
 begin
   Result := TPhysicalMaterialNode.Create;
 end;

//-------------------------------------

procedure TTextureMesh.initializedata;
 var Root : TX3DRootNode;
     Triangles : TIndexedTriangleSetNode;
     Appearance: TAppearanceNode;
     Shape : TShapeNode;
 begin
   inherited;
   TexCoordNode := TTextureCoordinateNode.Create;

   Triangles := initTriangles;
   initvertices;

   Shape := TShapeNode.Create;
   Shape.Geometry := Triangles;

   Appearance := InitAppearance;
   Shape.Appearance := Appearance;

   Root := TX3DRootNode.Create;
   Root.AddChildren( Shape );

   { texture }
   Triangles.TexCoord := TexCoordNode;

   Load(Root, true );
   UpdateMeshProperties;
 end;

destructor TTextureMesh.destroy;
 begin
   inherited;
   TexCoordNode.Free;
 end;

function TTextureMesh.inittexture( TextureUrl : string ) : TImageTextureNode;
 begin
   Result := TImageTextureNode.Create;
   Result.SetUrl([TextureUrl]);
 end;

procedure TTextureMesh.InitVertices;
 var Vertices  : TVector3List;
     step, sz2 : single;
     i, j, vcount : integer;
     VertexPtr : ^TVector3;
     Vertex : TVector3;
     TexCoords : TVector2List;
 begin
   step := fgridstep;
   Vertices := TVector3List.Create;
   vcount := fGridCount * fGridCount;
   Vertices.Count := vcount;
   VertexPtr := Vertices.Ptr(0);
   TexCoords := TVector2List.Create;
   TexCoords.Capacity := vcount;
   sz2 := CellCount * Step * 0.5;
   vertex.y := 0;
   vertex.z := -sz2;
   for i := 0 to fGridCount - 1 do
    begin
      Vertex.x := -sz2; { world x offset to align tiles }
      for j := 0 to fGridCount - 1 do
       begin
         VertexPtr^ := Vertex;
         vertex.x := vertex.x + step;
         TexCoords.Add(Vector2(0,0));
         inc( vertexptr );
       end;
      vertex.z := vertex.z + step;
    end;
   CoordinateNode.SetPoint( Vertices );
   TexCoordNode.SetPoint(TexCoords);
   Vertices.Free;
   TexCoords.Free;
 end;


end.
