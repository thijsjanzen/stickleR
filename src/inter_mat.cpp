#include <Rcpp.h>
#include <vector>


struct room {
  room() {
    double max_t = 1 + 3600 * 24;
    entries = std::vector< std::vector<double>>(max_t);
  }

  std::vector< std::vector< double >> entries;

  void add_fish(int time, int fish) {
    entries[time].push_back(fish);
  }

  void update_interaction_matrix(std::vector< std::vector< double >>* im) {
      for (const auto& e : entries) {
        if (!e.empty()) {
          for (int i = 0; i < e.size(); ++i) {
            for (int j = i; j < e.size(); ++j) {
              auto a = e[i];
              auto b = e[j];
              if (a != b) {
                (*im)[a][b]++;
                (*im)[b][a] = (*im)[a][b];
              }
            }
          }
        }
      }
  }
};

void vector_to_numericmatrix(const std::vector< std::vector< double >>& v,
                             Rcpp::NumericMatrix* m) {
  size_t n_rows = v.size();
  size_t n_cols = v[0].size();
  (*m) = Rcpp::NumericMatrix(n_rows, n_cols);
  for (size_t i = 0; i < n_rows; ++i) {
    for (size_t j = 0; j < n_cols; ++j) {
      (*m)(i, j) = v[i][j];
    }
  }
  return;
}


//' function to get all locations
//' @param times tt
//' @param location location
//' @param fish fish
//' @return vector of sites
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix make_matrix_class(const std::vector<double>& times,
                                      const std::vector<double>& location,
                                      const std::vector<double>& fish,
                                      int num_fish) {

  std::vector< room > compartments;
  for (int i = 0; i < 9; ++i) {
    compartments.push_back(room());
  }

  Rcpp::Rcout << "0--------25--------50--------75--------100\n";
  Rcpp::Rcout << "*";

  int updateFreq = times.size() / 20;
  if(updateFreq < 1) updateFreq = 1;

  for (int i = 0; i < times.size(); ++i) {
    if (i % updateFreq == 0) {
      Rcpp::Rcout << "**";
      Rcpp::checkUserInterrupt();
    }
    compartments[location[i] - 1].add_fish(times[i], fish[i]);
  }

  std::vector< std::vector< double >> int_mat(num_fish,
                                              std::vector<double>(num_fish, 0.0));

  for (auto& c : compartments) {
    c.update_interaction_matrix(&int_mat);
  }

  Rcpp::NumericMatrix out;
  vector_to_numericmatrix(int_mat, &out);

  return out;
}



