unit pipemesh;

{ TPipeMesh

 }

interface

uses
  Classes, Math, CastleVectors, CastleScene, CastleUtils, x3dNodes, x3dTools;

type tpipemeshbuilder = class( tshapebuilder )

        constructor create( iNodeCount : integer;
                            iRadius    : single;
                            iSlicePtCount : integer );

        function buildshape : TShapeNode;

        public

        nodeCount : integer;
        defaultRadius : single;
        defaultSlicePtCount : integer;

      end;

implementation

constructor tpipemeshbuilder.create( iNodeCount : integer;
                                     iRadius : single;
                                     iSlicePtCount : integer );
 begin
   inherited create;
   nodeCount := iNodeCount;
   defaultRadius := iRadius;
   defaultSlicePtCount := iSlicePtCount;

   Vertices.Capacity := nodeCount * defaultSlicePtCount;
   Indexes.Capacity := ( nodeCount - 1 ) * defaultSlicePtCount * 6;
 end;

function tpipemeshbuilder.buildshape : TShapeNode;
var Triangles : TIndexedTriangleSetNode;
    Material: TPhysicalMaterialNode;
    Appearance: TAppearanceNode;
    CoordinateNode : TCoordinateNode;
begin
  Result:= TShapeNode.Create;

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
