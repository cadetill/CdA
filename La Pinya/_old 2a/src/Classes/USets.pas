unit USets;

interface

type
  TFreq = (fYearly, fMontly, fWeekly, fDaily, fHourly, fMinutely, fUndefined);

  TRolsList = (rAdmin=1, rJunta, rTecnica, rCasteller, rSanitari, rConvidat, rComunicacio);
  TRols = set of TRolsList;

implementation

end.
