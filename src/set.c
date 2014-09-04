#define USE_RINTERNALS

#include <R.h>
#include <Rinternals.h>

SEXP setattr(SEXP object, SEXP attr, SEXP value) {
  Rf_setAttrib(object, attr, value);
  return R_NilValue;
}

SEXP set_string_elt(SEXP vector, SEXP index, SEXP value) {
  SET_STRING_ELT(vector, INTEGER(index)[0], STRING_ELT(value, 0));
  return R_NilValue;
}


SEXP set_vector_elt(SEXP vector, SEXP index, SEXP value) {
  SET_VECTOR_ELT(vector, INTEGER(index)[0], value);
  return R_NilValue;
}

SEXP copy(SEXP vector) {
  SEXP result = Rf_duplicate(vector);
  return result;
}

SEXP recycle(SEXP list) {

  // First, validate the list -- should have all elements of length
  // 1 or n. Figure out what 'nrows' is.

  int nrows = 1;
  int n = length(list);
  if (n == 0) {
    return list;
  }

  // Find the first column with length greater than 1
  int i = 0;
  while (i < n) {
    SEXP elt = VECTOR_ELT(list, i);
    int len = length(elt);
    if (len == 0) {
      return ScalarLogical(0);
    }
    if (len > 1) {
      nrows = len;
      ++i;
      break;
    }
    ++i;
  }

  // Ensure all following columns are either length 1, or n
  while (i < n) {
    SEXP elt = VECTOR_ELT(list, i);
    int len = length(elt);
    if (!(len == 1 || len == nrows)) {
      return ScalarLogical(0);
    }
    ++i;
  }

  // Now, loop through once more to recycle
  SEXP rowSEXP = PROTECT(ScalarInteger(nrows));
  for (int i = 0; i < n; i++) {
    SEXP elt = VECTOR_ELT(list, i);
    int len = length(elt);
    if (len == 1) {
      SEXP rep_int = PROTECT(lang3(install("rep.int"), elt, rowSEXP));
      SET_VECTOR_ELT(list, i, eval(rep_int, R_GlobalEnv));
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);

  return ScalarLogical(1);

}

SEXP fill_lazy(SEXP output, SEXP dots, SEXP dots_nm) {

  SEXP output_nm = Rf_getAttrib(output, R_NamesSymbol);
  int n = length(output);

  // Cache the lazy eval call
  SEXP lazy_eval = PROTECT(eval(
        Rf_lang3(
          Rf_install("::"),
          Rf_install("lazy"),
          Rf_install("lazy_eval")
        ),
        R_GlobalEnv
  ));

  for (int i = 0; i < n; i++) {

    // Perform lazy evaluation
    SEXP lazy_call = PROTECT(Rf_lang3(
      lazy_eval,
      VECTOR_ELT(dots, i),
      output
    ));

    SET_VECTOR_ELT(output, i, eval(lazy_call, R_GlobalEnv));

    UNPROTECT(1);

    // Set the names
    if (strcmp(CHAR(STRING_ELT(dots_nm, i)), "") == 0) {
      SEXP elt = VECTOR_ELT(VECTOR_ELT(dots, i), 0);
      SET_STRING_ELT(output_nm, i, PRINTNAME(elt));
    } else {
      SET_STRING_ELT(output_nm, i, STRING_ELT(dots_nm, i));
    }

  }

  UNPROTECT(1);
  return R_NilValue;

}

#undef USE_RINTERNALS
