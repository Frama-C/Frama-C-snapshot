typedef enum {
  AU_NO_MODE = 0,
  AU_ANLAUF = (0x0001),
  AU_BETRIEB = (0x0002),
  AU_PARAMETRIEREN = (0x0004),
  AU_FUNKTIONSPRFNG = (0x0008),
  AU_DIAGNOSE = (0x0010),
  AU_RESET = (0x0020)
} auModeStates_t;

static auModeStates_t mode;
auModeStates_t G = AU_NO_MODE;
int G_int = 75, mode_int;

void main (void) {
 
  if ((AU_DIAGNOSE == mode)) // && ((void *) 0 != auDiagnostics_p)) 
    {G = mode;}

  if ((0 == mode_int)) // && ((void *) 0 != auDiagnostics_p)) 
    {G_int = mode_int;}
  
  return;
  
}
