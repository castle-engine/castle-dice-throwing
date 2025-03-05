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
  CastleVectors, CastleComponentSerialize, CastleViewport, CastleTimeUtils,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonThrow: TCastleButton;
    DicePhysics: TCastleTransform;
    TransformDiceToMatchDesired: TCastleTransform;
    EditStrengthImpulseHorizontal, EditStrengthImpulseVertical,
      EditImpulseRandomShift, EditAngularVelocityDamp,
      EditMass, EditFriction, EditAvoidAngleBottom: TCastleFloatEdit;
    MainViewport: TCastleViewport;
    CheckboxHideSimulation: TCastleCheckbox;
  private
    type
      TDiceResult = 1..6;
      TDiceLook = 1..3;
      TRecordedSimulation = record
        { Dice position at this frame. }
        DiceTranslation: TVector3;
        { Dice rotation at this frame. }
        DiceRotation: TVector4;
      end;

    const
      { Frequency of recording DicePhysics state, in frames per second.
        Reasonable values are 10..60.
        Larger values imply longer simulation time (before we actually show
        the dice), but also more accurate animation when playing the recorded
        frames back. }
      RecordFps = 30;
      RecordTimeStep = 1 / RecordFps;
      { We stop simulation after this many seconds, even if DicePhysics
        is still awake.
        This prevents the simulation from running too long. }
      MaxRecordedSimulationSeconds = 3;
      MaxRecordedSimulationFrames = RecordFps * MaxRecordedSimulationSeconds;

    var
      ButtonDesired: array [1..6] of TCastleButton;
      TransformResult: array [1..6] of TCastleTransform;
      ButtonDiceLook: array [TDiceLook] of TCastleButton;
      TransformDice: array [TDiceLook] of TCastleTransform;
      InitialDiceTranslation: TVector3;
      InitialDiceRotation: TVector4;
      DesiredOutcome: TDiceResult;
      AwakeLifeTime: TFloatTime;
      AwakeMeasuring: Boolean;

      RecordedSimulation: array [0..MaxRecordedSimulationFrames - 1] of TRecordedSimulation;
      { Currently valid frames in RecordedSimulation. }
      RecordedSimulationFrames: Integer;
      { Are we now playing the recorded simulation (true)
        or letting physics to run (false). }
      PlayRecordedSimulation: Boolean;

    { Current result of the dice, looking at DicePhysics rotation.
      Answer @false if cannot determine yet, so dice is not exactly on one
      of it's sides (e.g. maybe it leans by the wall, slanted). }
    function CurrentDiceResult(out DiceResult: TDiceResult): Boolean;

    { Rotate the dice to match the desired outcome by setting
      TransformDiceToMatchDesired.Rotation.
      Assumes TransformDiceToMatchDesired is identity at the beginning. }
    procedure AdjustTransformDiceToMatchDesired(const CurrentResult, Desired: TDiceResult);

    procedure ClickThrow(Sender: TObject);
    procedure ClickDesired(Sender: TObject);
    procedure ClickDiceLook(Sender: TObject);
    procedure ChangeAngularVelocityDamp(Sender: TObject);
    procedure ChangeMass(Sender: TObject);
    procedure ChangeFriction(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleUtils, CastleLog, CastleQuaternions;

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

  { Put named components in an array, for easier working with them in bulk.
    And assing some button events }
  for I in TDiceResult do
  begin
    TransformResult[I] := DesignedComponent('TransformResult' + IntToStr(I)) as TCastleTransform;
    ButtonDesired[I] := DesignedComponent('ButtonDesired' + IntToStr(I)) as TCastleButton;
    ButtonDesired[I].OnClick := {$ifdef FPC}@{$endif} ClickDesired;
  end;
  for I in TDiceLook do
  begin
    TransformDice[I] := DesignedComponent('TransformDice' + IntToStr(I)) as TCastleTransform;
    ButtonDiceLook[I] := DesignedComponent('ButtonDiceLook' + IntToStr(I)) as TCastleButton;
    ButtonDiceLook[I].OnClick := {$ifdef FPC}@{$endif} ClickDiceLook;
  end;

  ButtonThrow.OnClick := {$ifdef FPC}@{$endif} ClickThrow;

  InitialDiceTranslation := DicePhysics.Translation;
  InitialDiceRotation := DicePhysics.Rotation;
  DesiredOutcome := 1; // assumes UI has ButtonDesired1 pressed by default

  // enable on 1st dice throw
  MainViewport.Items.EnablePhysics := false;

  // assign edit events, to adjust physics values as dice is rolling
  EditAngularVelocityDamp.OnChange := {$ifdef FPC}@{$endif} ChangeAngularVelocityDamp;
  EditMass.OnChange := {$ifdef FPC}@{$endif} ChangeMass;
  EditFriction.OnChange := {$ifdef FPC}@{$endif} ChangeFriction;
end;

function TViewMain.CurrentDiceResult(out DiceResult: TDiceResult): Boolean;
const
  Epsilon = 0.1;
var
  I: TDiceResult;
begin
  for I in TDiceResult do
    if TVector3.Equals(TransformResult[I].WorldDirection, TVector3.One[1], Epsilon) then
    begin
      DiceResult := I;
      Exit(true);
    end;
end;

procedure TViewMain.AdjustTransformDiceToMatchDesired(const CurrentResult, Desired: TDiceResult);
var
  Axis: Integer;
  RotationAxis: TVector3;
  NewDiceResult: TDiceResult;
begin
  if CurrentResult = Desired then
    Exit; // nothing to do
  { Brute-force solution: rotate 90, -90 or 180 around one axis. }
  for Axis := 0 to 2 do
  begin
    RotationAxis := TVector3.One[Axis];
    TransformDiceToMatchDesired.Rotation := Vector4(RotationAxis, -Pi / 2);
    if CurrentDiceResult(NewDiceResult) and (NewDiceResult = Desired) then
      Exit;
    TransformDiceToMatchDesired.Rotation := Vector4(RotationAxis, Pi / 2);
    if CurrentDiceResult(NewDiceResult) and (NewDiceResult = Desired) then
      Exit;
    TransformDiceToMatchDesired.Rotation := Vector4(RotationAxis, Pi);
    if CurrentDiceResult(NewDiceResult) and (NewDiceResult = Desired) then
      Exit;
  end;
  WritelnWarning('Cannot adjust dice to match desired outcome');
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  { Call this when PlayRecordedSimulation is false.
    Updates AwakeMeasuring, AwakeLifeTime.
    When dice falls asleep, logs it, logs the result,
    and transforms the dice to match the desired outcome.

    This is all just for debug, to investigate the typical sleep time,
    to debug that CurrentDiceResult, AdjustTransformDiceToMatchDesired
    are correct. }
  procedure UpdateCheckAwake;
  var
    DiceResult: TDiceResult;
  begin
    if AwakeMeasuring and DicePhysics.RigidBody.Awake then
      AwakeLifeTime := AwakeLifeTime + SecondsPassed
    else
    if AwakeMeasuring then
    begin
      AwakeMeasuring := false;
      WritelnLog('Was awake for %f seconds', [AwakeLifeTime]);
      if CurrentDiceResult(DiceResult) then
      begin
        WritelnLog('Dice result is %d', [DiceResult]);
        AdjustTransformDiceToMatchDesired(DiceResult, DesiredOutcome);
      end else
        WritelnLog('Dice result is not clear');
    end;
  end;

  procedure UpdatePlayRecordedSimulation;
  var
    PreviousFrame, NextFrame: Int64;
    RemainderInFrame: Double;
  begin
    AwakeLifeTime := AwakeLifeTime + SecondsPassed;
    FloatDivMod(AwakeLifeTime, RecordTimeStep, PreviousFrame, RemainderInFrame);
    if PreviousFrame >= RecordedSimulationFrames -1 then
    begin
      // just show last recorded frame
      DicePhysics.Translation := RecordedSimulation[RecordedSimulationFrames - 1].DiceTranslation;
      DicePhysics.Rotation := RecordedSimulation[RecordedSimulationFrames - 1].DiceRotation;
    end else
    begin
      NextFrame := PreviousFrame + 1;
      DicePhysics.Translation := Lerp(RemainderInFrame / RecordTimeStep,
        RecordedSimulation[PreviousFrame].DiceTranslation,
        RecordedSimulation[NextFrame].DiceTranslation);
      DicePhysics.Rotation := NLerp(RemainderInFrame / RecordTimeStep,
        RecordedSimulation[PreviousFrame].DiceRotation,
        RecordedSimulation[NextFrame].DiceRotation);
    end;
  end;

begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if PlayRecordedSimulation then
    UpdatePlayRecordedSimulation
  else
    UpdateCheckAwake;
end;

procedure TViewMain.ChangeAngularVelocityDamp(Sender: TObject);
begin
  DicePhysics.RigidBody.AngularVelocityDamp := EditAngularVelocityDamp.Value;
end;

procedure TViewMain.ChangeMass(Sender: TObject);
begin
  DicePhysics.Collider.Mass := EditMass.Value;
end;

procedure TViewMain.ChangeFriction(Sender: TObject);
begin
  DicePhysics.Collider.Friction := EditFriction.Value;
end;

procedure TViewMain.ClickThrow(Sender: TObject);

  { Initialize the dice physics and start the simulation,
    randomizing how the dice is thrown. }
  procedure StartSimulation;
  var
    { How much to push in horizontal.
      Should be >= 0, larges values make it go horizontally more. }
    StrengthImpulseHorizontal: Single;
    { How much to push in vertical.
      Can be any number - negative, positive, zero. }
    StrengthImpulseVertical: Single;
    { Randomize the point of impulse, making dice spin a bit.
      Should be >= 0, larges values make it spin more. }
    ImpulseRandomShift: Single;
  var
    ImpulseAngle, ImpulseX, ImpulseZ, AvoidAngleBottom: Single;
    ImpulseDir, ImpulsePos: TVector3;
  begin
    { Start physics (initially disabled, disabled also when playing recorded).
      Note: It's important to enable physics *before*
      DicePhysics.Translation/Rotation reset below.
      Otherwise, in case we just finished playback of recorded simulation
      (with EnablePhysics=false), and we would reset
      DicePhysics.Translation/Rotation with physics disabled, Kraft
      could not realize that the dice is now in a new position *not* colliding
      with ground, and Kraft could make weird initial dice bounce. }
    MainViewport.Items.EnablePhysics := true;

    // reset position and rotation
    DicePhysics.Translation := InitialDiceTranslation;
    DicePhysics.Rotation := InitialDiceRotation;
    DicePhysics.RigidBody.LinearVelocity := TVector3.Zero;
    DicePhysics.RigidBody.AngularVelocity := TVector3.Zero;
    // start with no rotation in TransformDiceToMatchDesired
    TransformDiceToMatchDesired.Rotation := TVector4.Zero;

    // use UI parameters
    StrengthImpulseHorizontal := EditStrengthImpulseHorizontal.Value;
    StrengthImpulseVertical := EditStrengthImpulseVertical.Value;
    ImpulseRandomShift := EditImpulseRandomShift.Value;
    AvoidAngleBottom := EditAvoidAngleBottom.Value;
    DicePhysics.RigidBody.AngularVelocityDamp := EditAngularVelocityDamp.Value;
    DicePhysics.Collider.Friction := EditFriction.Value;
    DicePhysics.Collider.Mass := EditMass.Value;

    { Random impulse, to make the throw look more interesting. }

    ImpulseAngle := RandomFloatRange(AvoidAngleBottom, 2 * Pi - AvoidAngleBottom);
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

    DicePhysics.RigidBody.ApplyImpulse(ImpulseDir, ImpulsePos);
  end;

  procedure RecordSimulation;
  var
    TimeStart: TTimerResult;
    NowDiceResult: TDiceResult;
  begin
    TimeStart := Timer;

    repeat
      { Run one simulation and see do we get clear result,
        i.e. CurrentDiceResult returns true.

        CurrentDiceResult may return false if simulation stopped too soon
        (MaxRecordedSimulationSeconds was not enough to make DiscPhysics go to
        sleep) or if dice is not exactly on one of it's sides (e.g. maybe it
        leans by the wall, slanted).
        In any of these cases, just run another simulation,
        StartSimulation randomizes the start conditions. }

      StartSimulation;
      RecordedSimulationFrames := 0;

      // frame 0 = initial state
      RecordedSimulation[0].DiceTranslation := DicePhysics.Translation;
      RecordedSimulation[0].DiceRotation := DicePhysics.Rotation;
      Inc(RecordedSimulationFrames);

      repeat
        MainViewport.Items.UpdateIncreaseTime(RecordTimeStep);
        RecordedSimulation[RecordedSimulationFrames].DiceTranslation := DicePhysics.Translation;
        RecordedSimulation[RecordedSimulationFrames].DiceRotation := DicePhysics.Rotation;
        // debug:
        // WritelnLog('Recorded frame %d, pos: %s', [RecordedSimulationFrames, DicePhysics.Translation.ToString]);
        Inc(RecordedSimulationFrames);
      until
        (RecordedSimulationFrames = MaxRecordedSimulationFrames) or
        (not DicePhysics.RigidBody.Awake);

      WritelnLog('Recorded %d frames', [RecordedSimulationFrames]);
    until CurrentDiceResult(NowDiceResult);

    AdjustTransformDiceToMatchDesired(NowDiceResult, DesiredOutcome);

    WritelnLog('Took %f secs to find and record a good simulation', [
      TimeStart.ElapsedTime
    ]);
  end;

begin
  PlayRecordedSimulation := CheckboxHideSimulation.Checked;
  if PlayRecordedSimulation then
  begin
    RecordSimulation;
    // no physics during playback
    MainViewport.Items.EnablePhysics := false;
  end else
    StartSimulation;

  { Measure awake time.
    When PlayRecordedSimulation=false: useful to later know how long to simulate.
    When PlayRecordedSimulation=true: useful to pick recorded frame. }
  AwakeLifeTime := 0;
  AwakeMeasuring := true;
end;

procedure TViewMain.ClickDesired(Sender: TObject);
var
  Button: TCastleButton;
  I: TDiceResult;
begin
  Button := Sender as TCastleButton;
  DesiredOutcome := Button.Tag;
  for I in TDiceResult do
    ButtonDesired[I].Pressed := Button.Tag = I;
end;

procedure TViewMain.ClickDiceLook(Sender: TObject);
var
  Button: TCastleButton;
  I: TDiceLook;
begin
  Button := Sender as TCastleButton;
  for I in TDiceLook do
  begin
    TransformDice[I].Exists := Button.Tag = I;
    ButtonDiceLook[I].Pressed := Button.Tag = I;
  end;
end;

end.
