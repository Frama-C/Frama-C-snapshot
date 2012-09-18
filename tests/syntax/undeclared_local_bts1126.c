

typedef struct gnutls_session_t {
  int f;
} gnutls_session_t;

typedef void(*gnutls_cipher_algorithm_t)(int);

typedef void(*gnutls_mac_algorithm_t)(int);

#define GNUTLS_CIPHER_UNKNOWN 0
#define GNUTLS_MAC_UNKNOWN 0
#define GNUTLS_E_INTERNAL_ERROR 0
#define GNUTLS_E_UNWANTED_ALGORITHM 0

typedef struct record_parameters_st {
  int initialized;
  gnutls_cipher_algorithm_t cipher_algorithm;
  gnutls_mac_algorithm_t mac_algorithm;
} record_parameters_st;

typedef struct cipher_suite_st {
  int a;
} cipher_suite_st;

int
_gnutls_epoch_set_cipher_suite (gnutls_session_t session,
                                int epoch_rel, cipher_suite_st * suite)
{
  gnutls_cipher_algorithm_t cipher_algo;
  gnutls_mac_algorithm_t mac_algo;
  record_parameters_st *params;
  int ret;

  ret = _gnutls_epoch_get (session, epoch_rel, &params);
  if (ret < 0)
    return gnutls_assert_val (ret);

  if (params->initialized
      || params->cipher_algorithm != GNUTLS_CIPHER_UNKNOWN
      || params->mac_algorithm != GNUTLS_MAC_UNKNOWN)
    return gnutls_assert_val (GNUTLS_E_INTERNAL_ERROR);

  cipher_algo = _gnutls_cipher_suite_get_cipher_algo (suite);
  mac_algo = _gnutls_cipher_suite_get_mac_algo (suite);

  if (_gnutls_cipher_is_ok (cipher_algo) != 0
      || _gnutls_mac_is_ok (mac_algo) != 0)
    return gnutls_assert_val (GNUTLS_E_UNWANTED_ALGORITHM);

  params->cipher_algorithm = cipher_algo;
  params->mac_algorithm = mac_algo;

  return 0;
}
