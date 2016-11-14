program fpcunitproject1;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  GuiTestRunner,
  fpcunittestrunner,
  lazcontrols,
  dmbasepackage,
  dmbaseintfpackage,
  dmbasennetwork,
  TestCaseBaseInterface,
  TestCaseForBaseEntry,
  TestCaseForBaseMap,
  TestCaseForBaseMapConcurency,
  TestCaseForBaseValue,
  TestCaseForBvalueconcurency,
  TestCaseForListsAndSets,
  TestCaseNeuronNet;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
