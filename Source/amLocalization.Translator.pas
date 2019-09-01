unit amLocalization.Translator;

(*
 * Copyright © 2019 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

interface

uses
  amLocale,
  amLocalization.Model;

type
  ITranslationService = interface
    ['{254E24D5-8F3C-422E-9304-1E21126C8B3C}']
    function BeginLookup(SourceLanguage, TargetLanguage: TLocaleItem): boolean;
    procedure EndLookup;
    function Lookup(Prop: TLocalizerProperty; SourceLanguage, TargetLanguage: TLocaleItem; const SourceValue: string; var TargetValue: string): boolean;

    function GetServiceName: string;
    property ServiceName: string read GetServiceName;
  end;

implementation

end.
