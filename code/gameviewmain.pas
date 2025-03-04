{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Dice Throwing".

  "Dice Throwing" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Dice Throwing" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonThrow,
      ButtonDesired1, ButtonDesired2, ButtonDesired3,
      ButtonDesired4, ButtonDesired5, ButtonDesired6,
      ButtonDiceLook1, ButtonDiceLook2, ButtonDiceLook3: TCastleButton;
    DicePhysics, SceneDice1, SceneDice2, SceneDice3: TCastleTransform;
  private
    ButtonDesired: array [1..6] of TCastleButton;
    ButtonDiceLook: array [1..3] of TCastleButton;
    SceneDice: array [1..3] of TCastleTransform;
    InitialDiceTranslation: TVector3;
    InitialDiceRotation: TVector4;
    DesiredOutcome: 1..6;
    procedure ClickThrow(Sender: TObject);
    procedure ClickDesired(Sender: TObject);
    procedure ClickDiceLook(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  I: Integer;
begin
  inherited;

  { Put named component in an array, for easier working with them in bulk.

    Note: We could have also initialized their names using a loop,
    without the need to make ButtonDesiredX published, using
    TCastleView.DesignedComponent like

      for I := 1 to 6 do
        ButtonDesired[I] := DesignedComponent('ButtonDesired' + IntToStr(I)) as TCastleButton;
  }

  ButtonDesired[1] := ButtonDesired1;
  ButtonDesired[2] := ButtonDesired2;
  ButtonDesired[3] := ButtonDesired3;
  ButtonDesired[4] := ButtonDesired4;
  ButtonDesired[5] := ButtonDesired5;
  ButtonDesired[6] := ButtonDesired6;

  ButtonDiceLook[1] := ButtonDiceLook1;
  ButtonDiceLook[2] := ButtonDiceLook2;
  ButtonDiceLook[3] := ButtonDiceLook3;

  SceneDice[1] := SceneDice1;
  SceneDice[2] := SceneDice2;
  SceneDice[3] := SceneDice3;

  { Assign button events. }
  ButtonThrow.OnClick := {$ifdef FPC}@{$endif} ClickThrow;
  for I := 1 to 6 do
    ButtonDesired[I].OnClick := {$ifdef FPC}@{$endif} ClickDesired;
  for I := 1 to 3 do
    ButtonDiceLook[I].OnClick := {$ifdef FPC}@{$endif} ClickDiceLook;

  InitialDiceTranslation := DicePhysics.Translation;
  InitialDiceRotation := DicePhysics.Rotation;
  DesiredOutcome := 1; // assumes UI has ButtonDesired1 pressed by default
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickThrow(Sender: TObject);
const
  { How much to push in horizontal.
    Should be >= 0, larges values make it go horizontally more. }
  StrengthImpulseHorizontal: Single = 0.5;
  { How much to push in vertical.
    Can be any number - negative, positive, zero. }
  StrengthImpulseVertical: Single = 0.5;
  { Randomize the point of impulse, making dice spin a bit.
    Should be >= 0, larges values make it spin more. }
  ImpulseRandomShift = 0.1;
var
  ImpulseAngle, ImpulseX, ImpulseZ: Single;
  ImpulseDir, ImpulsePos: TVector3;
begin
  DicePhysics.Translation := InitialDiceTranslation;
  DicePhysics.Rotation := InitialDiceRotation;

  { Random impulse, to make the throw look more interesting. }
  ImpulseAngle := Random * 2 * Pi;
  SinCos(ImpulseAngle, ImpulseX, ImpulseZ);
  ImpulseDir := Vector3(
    StrengthImpulseHorizontal * ImpulseX,
    StrengthImpulseVertical,
    StrengthImpulseHorizontal * ImpulseZ);

  ImpulsePos := DicePhysics.WorldTranslation;
  ImpulsePos := ImpulsePos + Vector3(
    RandomFloatRange(-ImpulseRandomShift, ImpulseRandomShift),
    RandomFloatRange(-ImpulseRandomShift, ImpulseRandomShift),
    RandomFloatRange(-ImpulseRandomShift, ImpulseRandomShift)
  );

  DicePhysics.RigidBody.ApplyImpulse(
    ImpulseDir, ImpulsePos);
  // TODO: more interesting throw
end;

procedure TViewMain.ClickDesired(Sender: TObject);
var
  Button: TCastleButton;
  I: Integer;
begin
  Button := Sender as TCastleButton;
  DesiredOutcome := Button.Tag;
  for I := 1 to 6 do
    ButtonDesired[I].Pressed := Button.Tag = I;
  // TODO: make the DesiredOutcome happen
end;

procedure TViewMain.ClickDiceLook(Sender: TObject);
var
  Button: TCastleButton;
  I: Integer;
begin
  Button := Sender as TCastleButton;
  for I := 1 to 3 do
  begin
    SceneDice[I].Exists := Button.Tag = I;
    ButtonDiceLook[I].Pressed := Button.Tag = I;
  end;
end;

end.
