program fpcunitproject1;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  GuiTestRunner,
  fpcunittestrunner,
  lazcontrols,
  DmBasePackage,
  DmBaseIntfPackage,
  DmBaseNNetwork,
  DmBaseGraphics,
  TestCaseBaseInterface,
  TestCaseForBaseEntry,
  TestCaseForBaseMap,
  TestCaseForBaseMapConcurency,
  TestCaseForBaseValue,
  TestCaseForBvalueconcurency,
  TestCaseForListsAndSets,
  TestCaseNeuronNet,
  TestCaseBaseBitmap;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
