
//#pragma AnnotationPolicy(StrongPre)
//#pragma AbstractDomain(Pol)

#define TAILLE_MAXIMUM 1000000

#define ECHEC 0
#define SUCCES 1

typedef int resultat_t;

typedef struct {
  unsigned longueur;
  char* message;
} *paquet_t;

/*@ type invariant paquet_bien_forme (paquet_t paquet) =
  @      \valid_range (paquet->message, 0, paquet->longueur - 1)
  @          // le message est bien alloué dans la mémoire
  @   && valid_string(paquet->message)
  @          // le message est une chaîne de caractères
  @ ;
  @*/

//@ requires taille < TAILLE_MAXIMUM;
char* strncpy (char* destination, const char* source, unsigned taille) 
{
  char* curseur = destination;

  if ( taille > 0 ) {
    while((*curseur++ = *source++) && --taille > 0)
      /* ne rien faire */ ;
    if ((*--source == '\0') && --taille > 0) {
      do {
	*destination++ = '\0';
      } while(--taille > 0);
    }
  }
  return destination;
}

/*@ requires \valid(paquet) && \valid_range(destination, 0, taille - 1);
  @ behavior echec:
  @   assumes taille < paquet->longueur;
  @   assigns \nothing;
  @   ensures \result == ECHEC;
  @ behavior succes:
  @   assumes taille >= paquet->longueur;
  @   assigns destination[0 .. taille - 1];
  @   ensures \result == SUCCES;
  @*/
resultat_t recevoir_un_paquet (char* destination, unsigned taille, paquet_t paquet)
{
  if ( taille < paquet->longueur ) return ECHEC;
  strncpy (destination, paquet->message, taille);
  return SUCCES;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make forum_cifre"
End:
*/
