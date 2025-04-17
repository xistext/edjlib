unit basemesh;

interface

uses
  Classes, SysUtils,
  CastleUtils, CastleVectors, CastleScene,
  x3dNodes;

type

TAbstractMesh = class( TCastleScene )
   public

   constructor create( aowner : TComponent ); override;

   procedure InitializeData; dynamic;
   procedure UpdateMeshProperties; dynamic;

   protected

   CoordinateNode : TCoordinateNode;  { maintain refererence to CoordinateNode }

   function initindexes : TInt32List;
   function inittriangles : TIndexedTriangleSetNode;

   function InitAppearance : TAppearanceNode; dynamic;
   function InitMaterial   : TPhysicalMaterialNode; dynamic;

   procedure setGridCount( iGridCount : integer ); dynamic; abstract;
   function getGridCount : integer; dynamic; abstract;

   procedure setGridStep( iGridStep : single ); dynamic; abstract;
   function getGridStep : single; dynamic; abstract;

   function getcellcount : integer;

   public

   procedure InitVertices; dynamic; abstract;

   property GridCount : integer read getGridCount write setGridCount;
   property GridStep  : single read getGridStep write setGridStep;
   property CellCount : integer read getCellCount;

   procedure UpdateGraphics; virtual; abstract;

 end;

TAbstractTextureMesh = class( TAbstractMesh )
   public

   procedure InitializeData; override;

   function inittexture( TextureUrl : string ) : TImageTextureNode;

   procedure InitVertices; override;

   protected

   TexCoordNode   : TTextureCoordinateNode;

end;


TBaseMesh = class( TAbstractMesh )
   public

   constructor create( aowner : TComponent ); override;

   private

   fGridCount  : integer; { vertex count }
   fGridStep   : single;

   procedure setGridCount( iGridCount : integer ); override;
   function getGridCount : integer; override;

   procedure setGridStep( iGridStep : single ); override;
   function getGridStep : single; override;

end;

TTextureMesh = class( TBaseMesh )
   public

   procedure InitializeData; override;

   function inittexture( TextureUrl : string ) : TImageTextureNode;

   procedure InitVertices; override;

   protected

   TexCoordNode   : TTextureCoordinateNode;

end;

implementation

constructor TAbstractMesh.create( aowner : TComponent );
 begin
   inherited create( aowner );
   CoordinateNode := nil;
 end;

function TAbstractMesh.getcellcount : integer;
 begin
   result := GridCount - 1;
 end;

function TAbstractMesh.initindexes : TInt32List;
 var X, Z, c : integer;
 begin
   Result := TInt32List.Create;
   c := GridCount;
   for X:= 1 to c - 1 do for Z := 1 to c - 1 do with Result do
    begin
      { triangle1 }
      Add( c * Z + X );
      Add( c * ( Z - 1 ) + X );
      Add( c * ( Z - 1 ) + X - 1 );
      { triangle2 }
      Add( c * Z + X );
      Add( c * ( Z - 1 ) + X - 1 );
      Add( c * Z + X - 1 );
    end;
 end;

function TAbstractMesh.InitAppearance : TAppearanceNode;
 begin
   result := TAppearanceNode.create;
     { make the material lit }
   result.Material := initMaterial;
 end;

function TAbstractMesh.initmaterial : TPhysicalMaterialNode;
 begin
   Result := TPhysicalMaterialNode.Create;
 end;

procedure TAbstractMesh.initializedata;
 begin
   CoordinateNode := TCoordinateNode.Create;
 end;

function TAbstractMesh.inittriangles : TIndexedTriangleSetNode;
 var Indexes : TInt32List;
 begin
   Indexes := initindexes;
   Result := TIndexedTriangleSetNode.Create;
   Result.Coord := CoordinateNode;
   Result.SetIndex( Indexes );
   Indexes.Free;
 end;

procedure TAbstractMesh.UpdateMeshProperties;
 begin
 end;

//--------------------------------------

constructor TBaseMesh.create( aowner : TComponent );
 begin
   inherited create( aowner );
   fGridCount := 0;
   fGridStep := 0;
 end;

procedure TBaseMesh.setGridCount( iGridCount : integer );
 begin
   fGridCount := iGridCount;
 end;

function TBaseMesh.getGridCount : integer;
 begin
   result := fGridCount;
 end;

procedure TBaseMesh.setGridStep( iGridStep  : single );
 begin
   fGridStep := iGridStep;
 end;

function TBaseMesh.getGridStep : single;
 begin
   result := fGridStep;
 end;

//-------------------------------------

procedure TAbstractTextureMesh.initializedata;
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

function TAbstractTextureMesh.inittexture( TextureUrl : string ) : TImageTextureNode;
 begin
   Result := TImageTextureNode.Create;
   Result.SetUrl([TextureUrl]);
 end;

procedure TAbstractTextureMesh.InitVertices;
 var Vertices  : TVector3List;
     step, sz2 : single;
     i, j, vcount : integer;
     VertexPtr : ^TVector3;
     Vertex : TVector3;
     TexCoords : TVector2List;
     GCount : integer;
 begin
   step := gridstep;
   GCount := GridCount;
   Vertices := TVector3List.Create;
   vcount := GCount * GCount;
   Vertices.Count := vcount;
   VertexPtr := Vertices.Ptr(0);
   TexCoords := TVector2List.Create;
   TexCoords.Capacity := vcount;
   sz2 := CellCount * Step * 0.5;
   vertex.y := 0;
   vertex.z := -sz2;
   for i := 0 to GCount - 1 do
    begin
      Vertex.x := -sz2; { world x offset to align tiles }
      for j := 0 to GCount - 1 do
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

//---------------------------------

procedure TTextureMesh.InitializeData;
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
   step := gridstep;
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
