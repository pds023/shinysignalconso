// highcharter 0.9.5 captures HCDefaults before optional modules load.
// Its per-widget reset would otherwise delete the accessibility defaults.
// ponytail: remove this once highcharter preserves optional-module defaults.
HCDefaults = Highcharts.merge(HCDefaults, Highcharts.getOptions());
