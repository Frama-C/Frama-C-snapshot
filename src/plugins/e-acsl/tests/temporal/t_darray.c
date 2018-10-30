/* run.config
   COMMENT: Checking propagation of referent numbers in arrays
*/

void area_triangle(double (*vertices)[4]) {
  /*@ assert rte: mem_access: \valid_read((double *)*(vertices + 0)); */
  /*@ assert rte: mem_access: \valid_read((double *)*(vertices + 1)); */
  return;
}

void abe_matrix(double (*vertices)[4]) {
  area_triangle(vertices);
  return;
}

double Vertices[3][4];
double Vertices2[3][4] = {};

int main(int argc, const char **argv) {
  double vertices[3][4] = {
    { 1.0, 2.0, 3.0, 4.0 },
    { 5.0, 6.0, 7.0, 8.0 },
    { 9.0, 10.0, 11.0, 12.0 }
  };
  double vertices2[3][4];
  double vertices3[3][4];
  double triple_vertices[2][3][4];
  double triple_vertices2[2][3][4] = {};

  abe_matrix(vertices);
  abe_matrix(vertices2);
  abe_matrix(vertices3);
  abe_matrix(Vertices);
  abe_matrix(Vertices2);
  abe_matrix(triple_vertices[0]);
  abe_matrix(triple_vertices2[0]);
  return 0;
}
